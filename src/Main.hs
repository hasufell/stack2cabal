{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

import qualified Data.ByteString     as BS
import           Data.YAML           (decodeStrict)
import qualified Options.Applicative as Opts
import           Stackage            (Stack)

-- MANUAL TEST:
-- cabal v2-run stackage-to-hackage -- -f tests/snapshot/stack.yaml
main :: IO ()
main = do
  Options{input} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  text <- BS.readFile input
  putStrLn $ show (decodeStrict @Stack text)
  -- TODO parse the stackage file
  -- TODO parse the resolver
  -- TODO merge nested resolvers using precedence rules
  -- TODO convert to cabal format
  -- TODO print out

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
