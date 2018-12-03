{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Applicative ((<|>))
import qualified Data.ByteString     as BS
import           Data.YAML           (decodeStrict)
import qualified Options.Applicative as Opts
import           Stackage            (Stack)

-- cabal v2-run stackage-to-hackage -- -f tests/stack.yaml
main :: IO ()
main = do
  Options{input} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  text <- case input of
            FileInput file -> BS.readFile file
            StdInput       -> BS.getContents
  putStrLn $ show (decodeStrict @Stack text)
  -- TODO parse the stackage file
  -- TODO parse the resolver
  -- TODO merge nested resolvers using precedence rules
  -- TODO convert to cabal format
  -- TODO print out

data Options = Options
  { input      :: Input
  }
data Input = FileInput FilePath | StdInput
optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> (file <|> pure StdInput)
  where
    file = FileInput <$> Opts.strOption
             (  Opts.long "file"
             <> Opts.short 'f'
             <> Opts.metavar "FILENAME"
             <> Opts.help "Input stack.yaml or snapshot.yaml")
