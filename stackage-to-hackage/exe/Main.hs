{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Control.Monad.Extra            (ifM)
import qualified Data.ByteString                as BS
import qualified Data.List.NonEmpty             as NEL
import           Data.Text.Encoding             (encodeUtf8)
import           Distribution.Types.PackageName (mkPackageName)
import qualified Options.Applicative            as Opts
import           StackageToHackage.Hackage      (printFreeze, printProject,
                                                 stackToCabal)
import           StackageToHackage.Stackage     (localDirs, readStack)
import           System.Directory               (doesDirectoryExist,
                                                 listDirectory)
import           System.FilePath                (takeBaseName, takeDirectory,
                                                 takeExtension, (</>))

main :: IO ()
main = do
  Options{input} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  stack <- readStack =<< BS.readFile input
  let dir = (takeDirectory input)
      subs = NEL.toList $ (dir </>) <$> localDirs stack
  cabals <- concat <$> traverse (globExt ".cabal") subs
  -- assumes that .cabal files are named correctly, otherwise we need
  -- PackageDescription-Parsec.html#v:readGenericPackageDescription
  let ignore = (mkPackageName . takeBaseName) <$> cabals
  (project, freeze) <- stackToCabal ignore dir stack
  BS.writeFile (dir </> "cabal.project") (encodeUtf8 $ printProject project)
  BS.writeFile (dir </> "cabal.project.freeze") (encodeUtf8 $ printFreeze freeze)

globExt :: String -> FilePath -> IO [FilePath]
globExt ext path = do
  files <- ifM (doesDirectoryExist path) (listDirectory path) (pure [])
  pure $ filter ((ext ==) . takeExtension) files

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
