{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Duplicates a subset of the Stack ADT. It'd be nice if we could just re-use
-- the actual ADT, but stack isn't available as a library that we can build from
-- Hackage.
module Stackage where

import           Control.Applicative          ((<|>))
import           Control.Monad.Extra          (loopM, unlessM)
import qualified Data.ByteString              as BS
import           Data.ByteString.Char8        (pack)
import           Data.List.NonEmpty           (NonEmpty (..), head, reverse,
                                               (<|))
import           Data.Map.Strict              (Map, empty)
import           Data.Maybe                   (listToMaybe, mapMaybe)
import           Data.Text                    (Text, concat, isSuffixOf,
                                               replace, takeWhile, unpack)
import           Data.YAML                    (FromYAML, decodeStrict,
                                               parseYAML, withMap, withStr,
                                               (.!=), (.:), (.:?))
import           Distribution.Text            (simpleParse)
import           Distribution.Types.PackageId (PackageIdentifier (..))
import qualified Network.Http.Client          as Http
import           Network.URI                  (parseURI)
import           Options.Applicative.Internal (hoistMaybe)
import           Prelude                      hiding (concat, head, reverse,
                                               takeWhile)
import           System.Directory             (XdgDirectory (..),
                                               createDirectory,
                                               doesDirectoryExist,
                                               doesFileExist, getXdgDirectory)
import           System.FilePath              (takeDirectory, (</>))

data Stack = Stack
  { resolver  :: ResolverRef
  , compiler  :: Maybe Ghc
  , packages  :: [Package]
  , extraDeps :: [Dep]
  , flags     :: Flags
  -- TODO ghcOptions
  } deriving (Show)

newtype Ghc = Ghc Text
  deriving (Show)
  deriving newtype (FromYAML)

data Package = Local FilePath
             | Location Git
               deriving (Show)

data Git = Git
  { repo    :: Repo
  , commit  :: Commit
  , subdirs :: [Subdir]
  } deriving (Show)
type Repo = Text
type Commit = Text
type Subdir = Text

data Resolver = Resolver
  { resolver :: Maybe ResolverRef
  , compiler :: Maybe Ghc
  , deps     :: [Dep]
  , flags    :: Flags
  } deriving (Show)
instance Semigroup Resolver where
  (Resolver r c p f) <> (Resolver r' c' p' f') =
    Resolver (r <|> r') (c <|> c') (p <> p') (f <> f')

data ResolverRef = Canned Text
                 | Snapshot Text
                 deriving (Show)

-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Types-PackageId.html#t:PackageIdentifier
-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Parsec-Class.html#v:simpleParsec

data Dep = Hackage PackageIdentifier
         | SourceDep Git
         deriving (Show)

newtype Flags = Flags (Map PkgName (Map FlagName Bool))
              deriving (Show)
              deriving newtype (FromYAML, Semigroup)

type PkgName = Text
type FlagName = Text

-- the format used at https://github.com/commercialhaskell/stackage-snapshots
-- which is similar to the Resolver format.
data NewResolver = NewResolver
  { compiler :: Ghc
  , packages :: [NewDep]
  , flags    :: Flags
  } deriving (Show)

data NewDep = NewDep PackageIdentifier
              deriving (Show)

--------------------------------------------------------------------------------
-- Resolvers

readStack :: BS.ByteString -> IO Stack
readStack text = either fail pure $ decode1Strict text

type RelativeResolvers = NonEmpty (Maybe FilePath, Resolver)
type Resolvers = NonEmpty Resolver

-- highest priority resolver first
unroll :: FilePath -> Stack -> IO Resolvers
unroll base stack = do
  let stack' = stack2resolver stack
  reverse <$> (loopM next (pure (Just base, stack')))
  where
    -- lowest priority (deepest) resolver first
    next :: RelativeResolvers -> IO (Either RelativeResolvers Resolvers)
    next rs = case head rs of
      (_, Resolver Nothing _ _ _)    -> pure $ Right (snd <$> rs)
      (dir, Resolver (Just r) _ _ _) -> (Left .  (<| rs)) <$> resolve dir r

-- if the Resolver is a local snapshot, also include its dir
resolve :: Maybe FilePath -> ResolverRef -> IO (Maybe FilePath, Resolver)
resolve _ (Canned lts) = do
  cached <- cache lts
  text <- (BS.readFile cached) <|> download
  update cached text
  either fail (\r -> pure (Nothing, r)) $ new2old <$> (decode1Strict $ text)
    where
      download =
        let path = replace "." "/" (replace "-" "/" lts)
            raw = concat ["https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/", path, ".yaml"]
        in do
          url <- maybe (fail "invalid URL") pure (parseURI $ unpack raw)
          putStrLn ("Downloading: " <> (show url))
          Http.get (pack $ show url) Http.concatHandler

      update file content = unlessM (doesFileExist file) (BS.writeFile file content)

-- TODO: remote snapshots
resolve (Just base) (Snapshot path) = do
  let file = base </> (unpack path)
      dir = takeDirectory file
  text <- BS.readFile file
  either fail (\r -> pure (Just dir, r)) $ decode1Strict text

resolve Nothing _ = fail "Remote snapshots can't use relative paths."

cache :: Text -> IO FilePath
cache file = do
  dir <- getXdgDirectory XdgCache "stackage"
  unlessM (doesDirectoryExist dir) (createDirectory dir)
  pure $ dir </> (unpack file)

stack2resolver :: Stack -> Resolver
stack2resolver Stack{resolver, compiler, packages, extraDeps, flags} =
  Resolver (Just resolver) compiler (sourcedeps <> extraDeps) flags
  where sourcedeps = mapMaybe pick packages
        pick (Local _)    = Nothing
        pick (Location g) = Just . SourceDep $ g

new2old :: NewResolver -> Resolver
new2old NewResolver{compiler, packages, flags} =
  Resolver Nothing (Just compiler) (new2old' <$> packages) flags
  where
    new2old' (NewDep pkg) = Hackage pkg

--------------------------------------------------------------------------------
-- YAML boilerplate

-- https://github.com/haskell-hvr/HsYAML/pull/5
decode1Strict :: FromYAML a => BS.ByteString -> Either String a
decode1Strict text = do
  as <- decodeStrict text
  maybe (Left "expected unique") Right $ listToMaybe as

instance FromYAML Stack where
   parseYAML = withMap "Stack" $ \m -> Stack
       <$> m .: "resolver"
       <*> m .:? "compiler"
       <*> m .:? "packages" .!= mempty
       <*> m .:? "extra-deps" .!= mempty
       <*> m .:? "flags" .!= (Flags empty)

instance FromYAML Git where
  parseYAML = withMap "Git" $ \m -> Git
    <$> m .: "git"
    <*> m .: "commit"
    <*> m .:? "subdirs" .!= []

instance FromYAML ResolverRef where
  parseYAML = withStr "ResolverRef" $ \s ->
    if isSuffixOf ".yaml" s
    then (pure . Snapshot) s
    else (pure . Canned) s

instance FromYAML Package where
  parseYAML n = (local n) <|> (location n)
    where
      local = withStr "Local" $ pure . Local . unpack
      location = withMap "Location" $ \m ->
        Location <$> m .: "location"

instance FromYAML Dep where
   parseYAML n = (hackage n) <|> (source n)
     where
       hackage = withStr "Hackage" $ \s ->
         Hackage <$> (hoistMaybe . simpleParse . unpack) s
       source n' = SourceDep <$> parseYAML n'

instance FromYAML Resolver where
  parseYAML = withMap "Resolver" $ \m -> Resolver
    <$> m .:? "resolver"
    <*> m .:? "compiler"
    <*> m .:? "packages" .!= mempty
    <*> m .:? "flags" .!= (Flags empty)

instance FromYAML NewDep where
   parseYAML = withMap "NewDep" $ \m -> hackage' =<< m .: "hackage"
     where
       hackage' = \s -> NewDep <$>
         (hoistMaybe . simpleParse . unpack)
         (takeWhile ('@' /=) s)

instance FromYAML NewResolver where
  parseYAML = withMap "NewResolver" $ \m -> NewResolver
    <$> m .: "compiler"
    <*> m .:? "packages" .!= mempty
    <*> m .:? "flags" .!= (Flags empty)
