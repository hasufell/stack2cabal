{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Data.ByteString     as BS
import           Data.Maybe          (listToMaybe)
import           Data.Text           (unpack)
import           Data.YAML           (FromYAML, decodeStrict)
import qualified Options.Applicative as Opts
import           Stackage            (Resolver, ResolverRef (..), Stack (..))
import           System.FilePath     (takeDirectory, (</>))

-- MANUAL TEST:
-- cabal v2-run stackage-to-hackage -- tests/snapshot/stack.yaml
main :: IO ()
main = do
  Options{input} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  text <- BS.readFile input
  stack <- readStack text
  putStrLn $ show stack
  stackage <- resolve (takeDirectory input) (resolver stack)
  putStrLn $ show stackage
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

resolve :: FilePath -> ResolverRef -> IO Resolver
resolve _ (Canned lts) = undefined
resolve base (Snapshot path) = do
  text <- BS.readFile (base </> (unpack path))
  either fail pure $ decode1Strict text

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
