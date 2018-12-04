{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (listToMaybe)
import           Data.Text             (replace, unpack)
import           Data.YAML             (FromYAML, decodeStrict)
import qualified Network.Http.Client   as Http
import qualified Options.Applicative   as Opts
import           Stackage              (Dep (..), NewDep (..), NewResolver (..),
                                        Resolver (..), ResolverRef (..),
                                        Stack (..))
import           System.FilePath       (takeDirectory, (</>))

-- MANUAL TEST:
-- cabal v2-run stackage-to-hackage -- tests/snapshot/stack.yaml
-- cabal v2-run stackage-to-hackage -- tests/stackage/stack.yaml
main :: IO ()
main = do
  Options{input} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  text <- BS.readFile input
  stack @ Stack{resolver} <- readStack text
  putStrLn $ show stack
  stackage <- resolve (takeDirectory input) resolver
  putStrLn $ take 1000 $ show stackage

  -- TODO merge nested resolvers using precedence rules
  -- TODO convert to cabal format
  -- TODO print out

readStack :: BS.ByteString -> IO Stack
readStack text = either fail pure $ decode1Strict text

-- https://github.com/haskell-hvr/HsYAML/pull/5
decode1Strict :: FromYAML a => BS.ByteString -> Either String a
decode1Strict text = do
  as <- decodeStrict text
  maybe (Left "expected unique") Right $ listToMaybe as

-- highest priority resolver first
unroll :: Stack -> IO [Resolver]
unroll Stack{resolver} = undefined

-- TODO cache the LTS files
-- TODO withOpenSSL
resolve :: FilePath -> ResolverRef -> IO Resolver
resolve _ (Canned lts) = do
  let path = replace "." "/" (replace "-" "/" lts)
      url = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/" <> unpack path <> ".yaml"
  putStrLn ("Downloading LTS " <> url)
  text <- Http.get (pack url) Http.concatHandler
  either fail pure $ new2old <$> (decode1Strict $ text)
resolve base (Snapshot path) = do
  text <- BS.readFile (base </> (unpack path))
  either fail pure $ decode1Strict text

new2old :: NewResolver -> Resolver
new2old NewResolver{compiler, packages, flags} =
  Resolver Nothing (Just compiler) (new2old' <$> packages) flags
  where
    new2old' (NewDep pkg) = Hackage pkg

merge :: [Resolver] -> Resolver
merge = undefined

-- allow the Stack file to override values in the resolver
override :: Stack -> Resolver -> Resolver
override = undefined

data Options = Options
  { input      :: FilePath
  }
optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> file
  where
    file = Opts.strArgument
             (  Opts.metavar "FILENAME"
             <> Opts.help "Input stack.yaml or snapshot.yaml")
