{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad                  (filterM)
import           Control.Monad.Extra            (ifM)
import qualified Data.ByteString                as BS
import qualified Data.List.NonEmpty             as NEL
import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import           Distribution.Types.PackageName (mkPackageName)
import           Hpack                          (Force(..), Options(..),
                                                 defaultOptions, hpackResult,
                                                 setTarget)
import           Options.Applicative
import           Prelude                        hiding (lines)
import           StackageToHackage.Hackage      (printFreeze, printProject,
                                                 stackToCabal, FreezeRemotes(..), PinGHC(..))
import           StackageToHackage.Stackage     (localDirs, readStack)
import           System.Directory               (doesDirectoryExist,
                                                 doesFileExist,
                                                 getCurrentDirectory,
                                                 listDirectory,
                                                 makeAbsolute)
import           System.FilePath                (takeBaseName, takeDirectory, takeExtension,
                                                 (</>))

version :: String
#ifdef CURRENT_PACKAGE_VERSION
version = CURRENT_PACKAGE_VERSION
#else
version = "unknown"
#endif

data Opts = Opts
  { input :: FilePath
  , output :: FilePath
  , freezeRemotes :: FreezeRemotes
  , pinGHC :: PinGHC
  }

optsP :: Parser Opts
optsP = Opts <$>
  (
    (strOption
      (short 'f' <> long "file" <> metavar "STACK_YAML" <> help
        "Path to stack.yaml file"
        <> value "stack.yaml"
        <> showDefaultWith show
      )
    )
  ) <*>
  (
    (strOption
      (short 'o' <> long "output-file" <> metavar "CABAL_PROJECT" <> help
        "Path to output file"
        <> value "cabal.project"
        <> showDefaultWith show
      )
    )
  ) <*>
  (FreezeRemotes <$> switch (short 'r' <> long "freeze-remotes" <> help "Additionally freeze all remote repos (slower, but more correct, because remote repos also can be hackage deps)")
  ) <*>
  ((PinGHC . not) <$> switch (short 'n' <> long "no-pin-ghc" <> help "Don't pin the GHC version"))



main :: IO ()
main = do
  let versionHelp = infoOption
        version
        (long "version" <> help "Show version" <> hidden)

  customExecParser (prefs showHelpOnError) (info (optsP <**> helper <**> versionHelp) idm) >>= \case
    Opts inFile outFile fremotes pin -> do
      -- read stack file
      inDir <- makeAbsolute (takeDirectory inFile)
      stack <- readStack =<< BS.readFile inFile

      -- get cabal files
      let subs = NEL.toList $ (inDir </>) <$> localDirs stack
      hpacks <- filterM (\d -> doesFileExist $ hpackInput d) $ subs
      _ <- traverse runHpack hpacks
      cabals <- concat <$> traverse (globExt ".cabal") subs

      -- run conversion
      let ignore = (mkPackageName . takeBaseName) <$> cabals
      (project, freeze) <- stackToCabal fremotes ignore inDir stack
      hack <- extractHack . decodeUtf8 <$> BS.readFile (inDir </> "stack.yaml")
      printText <- printProject pin project hack

      -- write files
      outDir <- makeAbsolute (takeDirectory outFile)
      BS.writeFile (outDir </> outFile) (encodeUtf8 printText)
      BS.writeFile (outDir </> (outFile <> ".freeze")) (encodeUtf8 $ printFreeze freeze)
  where
    hpackInput sub = sub </> "package.yaml"
    opts = defaultOptions {optionsForce = Force}
    runHpack sub = hpackResult $ setTarget (hpackInput sub) opts

-- Backdoor allowing the stack.yaml to contain arbitrary text that will be
-- included in the cabal.project
extractHack :: Text -> Maybe Text
extractHack (T.split ('\n' ==) -> lines) =
  let (_, region) = break (T.isPrefixOf "#+BEGIN_STACK2CABAL") lines
      (hack, _) = break (T.isPrefixOf "#+END_STACK2CABAL") region
      verbatim = mapMaybe (T.stripPrefix "# ") hack
  in if null verbatim
     then Nothing
     else Just $ T.intercalate "\n" verbatim

globExt :: String -> FilePath -> IO [FilePath]
globExt ext path = do
  files <- ifM (doesDirectoryExist path) (listDirectory path) (pure [])
  pure $ filter ((ext ==) . takeExtension) files

