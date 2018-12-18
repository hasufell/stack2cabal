{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Data.ByteString            as BS
import           Data.Text.Encoding         (encodeUtf8)
import qualified Options.Applicative        as Opts
import           StackageToHackage.Hackage  (printFreeze, printProject,
                                             stackageToHackage)
import           StackageToHackage.Stackage (readStack)
import           System.FilePath            (takeDirectory, (</>))

main :: IO ()
main = do
  Options{input} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  stack <- readStack =<< BS.readFile input
  let dir = (takeDirectory input)
  (project, freeze) <- stackageToHackage dir stack
  BS.writeFile (dir </> "cabal.project") (encodeUtf8 $ printProject project)
  BS.writeFile (dir </> "cabal.project.freeze") (encodeUtf8 $ printFreeze freeze)

data Options = Options
  { input      :: FilePath
  }
optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> file
  where
    file = Opts.strArgument
             (  Opts.metavar "FILENAME"
             <> Opts.help "Input stack.yaml")
