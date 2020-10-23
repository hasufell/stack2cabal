{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Duplicates a subset of the Stack ADT. It'd be nice if we could just re-use
-- the actual ADT, but stack isn't available as a library that we can build from
-- Hackage.
module StackageToHackage.Stackage where

import StackageToHackage.Stackage.Types
import StackageToHackage.Stackage.YAML ()

import Control.Applicative ((<|>))
import Control.Monad.Extra (loopM, unlessM)
import Data.ByteString.Lazy (toStrict)
import Data.List (nub, foldl', find, (\\))
import Data.List.NonEmpty (NonEmpty(..), head, nonEmpty, reverse, (<|))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Semigroup
import Data.Text (Text, replace, unpack, isPrefixOf)
import Data.YAML (FromYAML, decodeStrict)
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (getGlobalManager)
import Prelude hiding (head, reverse, takeWhile)
import System.Directory
    (XdgDirectory(..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr, hPutStrLn)

import qualified Data.ByteString as BS


localDirs :: Stack -> NonEmpty FilePath
localDirs Stack { packages } = fromMaybe (pure ".") $ nonEmpty $ mapMaybe
    locals
    packages
  where
    locals (Local p) = Just p
    locals (Location _) = Nothing

--------------------------------------------------------------------------------
-- Resolvers

readStack :: BS.ByteString -> IO Stack
readStack text = either fail pure $ decode1Strict text


-- highest priority resolver first
unroll :: FilePath -> Stack -> IO Resolvers
unroll base stack = do
    let stack' = stack2resolver stack
    reverse <$> loopM next (pure (Just base, stack'))
  where
    -- lowest priority (deepest) resolver first
    next :: RelativeResolvers -> IO (Either RelativeResolvers Resolvers)
    next rs = case head rs of
        (_, Resolver Nothing _ _ _) -> pure $ Right (snd <$> rs)
        (dir, Resolver (Just r) _ _ _) -> Left . (<| rs) <$> resolve dir r


-- if the Resolver is a local snapshot, also include its dir
resolve :: Maybe FilePath -> ResolverRef -> IO (Maybe FilePath, Resolver)
resolve _ (Canned lts) = do
    cached <- cache lts
    text <- BS.readFile cached <|> download
    update cached text
    either fail (\r -> pure (Nothing, r)) $ new2old <$> decode1Strict text
  where
    download =
        let path = unpack
                $ replace "." "/" (replace "-" "/" (replace "-0" "-" lts))
            raw = concat
                [ "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/"
                , path, ".yaml"
                ]
        in do
            manager <- getGlobalManager
            url <- parseRequest raw
            hPutStrLn stderr ("Downloading: " <> raw)
            resp <- httpLbs url manager
            pure $ toStrict $ responseBody resp

    update file content =
        unlessM (doesFileExist file) (BS.writeFile file content)

resolve (Just base) (Snapshot path)
    | isPrefixOf "http://" path || isPrefixOf "https://" path = parseFromURL
    | otherwise = parseFromFile
  where
    parseFromFile = do
        let file = base </> unpack path
            dir = takeDirectory file
        text <- BS.readFile file
        either fail (\r -> pure (Just dir, r)) $ decode1Strict text
    parseFromURL = do
        text <- download
        either fail (\r -> pure (Nothing, r)) $ decode1Strict text
    download = do
        manager <- getGlobalManager
        url <- parseRequest (unpack path)
        hPutStrLn stderr ("Downloading: " <> unpack path)
        resp <- httpLbs url manager
        pure $ toStrict $ responseBody resp

resolve Nothing _ = fail "Remote snapshots can't use relative paths."


cache :: Text -> IO FilePath
cache file = do
    dir <- getXdgDirectory XdgCache "stackage"
    createDirectoryIfMissing True dir
    pure $ dir </> unpack file


stack2resolver :: Stack -> Resolver
stack2resolver Stack { resolver, compiler, packages, extraDeps, flags } =
    Resolver (Just resolver) compiler (sourcedeps <> extraDeps) flags
  where
    sourcedeps = mapMaybe pick packages
    pick (Local _) = Nothing
    pick (Location g) = Just . SourceDep $ g


new2old :: NewResolver -> Resolver
new2old NewResolver { compiler, packages, flags } = Resolver
    Nothing
    (Just compiler)
    (new2old' <$> packages)
    flags
    where new2old' (NewDep pkg) = Hackage pkg


-- | Merging two resolvers is straight-forward, except for
-- handling Git deps. These need to be merged carefully, because
-- stack.yaml may add subdirs to the repo of a resolver.
-- Also see: https://github.com/hasufell/stack2cabal/issues/30
mergeResolvers :: Resolver -> Resolver -> Resolver
mergeResolvers (Resolver r c p f) (Resolver r' c' p' f') =
    Resolver (r <|> r') (c <|> c') (mergeDeps p p') (f <> f')
  where
    mergeDeps :: [Dep] -> [Dep] -> [Dep]
    mergeDeps lhs rhs =
        let nonGits = filter (not . isGitDep) lhs <> filter (not . isGitDep) rhs
            gitsLhs = (\(SourceDep dep) -> dep) <$> filter isGitDep lhs
            gitsRhs = (\(SourceDep dep) -> dep) <$> filter isGitDep rhs
            gitMerged = foldl' (\m key -> update key m) gitsRhs gitsLhs
        in (SourceDep <$> gitMerged) <> nonGits

    -- this is somewhat inefficient due to lists, but they're all fairly small
    update :: Git -> [Git] -> [Git]
    update git xs =
        -- find same repos
        case find (\g -> git { subdirs = [], commit = "" }
                     == g { subdirs = [], commit = "" })
                 xs of
            Just g
             -- on same commit, just append subdirs
             | commit g == commit git
             -> git { subdirs = nub (subdirs git <> subdirs g) }
                 : delete g xs
             -- on different commit need to delete subdirs from lower resolver
             | otherwise
             -> git
                 -- > [0, 0, 0] \\ [0, 0]
                 -- [0]
                 : g { subdirs = nub (subdirs g) \\ nub (subdirs git) }
                 : delete g xs
            Nothing -> git : xs

    isGitDep :: Dep -> Bool
    isGitDep (SourceDep _) = True
    isGitDep _ = False

    delete :: Eq a => a -> [a] -> [a]
    delete deleted xs = [ x | x <- xs, x /= deleted ]

--------------------------------------------------------------------------------
-- YAML

-- https://github.com/haskell-hvr/HsYAML/pull/5
decode1Strict :: FromYAML a => BS.ByteString -> Either String a
decode1Strict text = do
    as <- case decodeStrict text of
        Left e -> Left $ snd e
        Right a -> Right a
    maybe (Left "expected unique") Right $ listToMaybe as
