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
                                                 stackToCabal, FreezeRemotes(..))
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
  { input :: (Maybe FilePath)
  , freezeRemotes :: FreezeRemotes
  }

optsP :: Parser Opts
optsP = Opts <$>
  (optional
    (strOption
      (short 'f' <> long "file" <> metavar "STACK_FILE" <> help
        "Path to stack.yaml (optional, uses current dir by default)"
      )
    )
  ) <*>
  (FreezeRemotes <$> switch (short 'r' <> long "freeze-remotes" <> help "Additionally freeze all remote repos (slower, but more correct, because remote repos also can be hackage deps)")
  )



main :: IO ()
main = do
  let versionHelp = infoOption
        version
        (long "version" <> help "Show version" <> hidden)

  (stack, dir, fremotes) <- customExecParser (prefs showHelpOnError) (info (optsP <**> helper <**> versionHelp) idm) >>= \case
    Opts (Just fp) fremotes -> do
      dir <- makeAbsolute (takeDirectory fp)
      stack <- readStack =<< BS.readFile fp
      pure (stack, dir, fremotes)
    Opts Nothing fremotes -> do
      dir <- getCurrentDirectory
      stack <- readStack =<< BS.readFile (dir </> "stack.yaml")
      pure (stack, dir, fremotes)
  let subs = NEL.toList $ (dir </>) <$> localDirs stack
  hpacks <- filterM (\d -> doesFileExist $ hpackInput d) $ subs
  _ <- traverse runHpack hpacks
  cabals <- concat <$> traverse (globExt ".cabal") subs
  -- we could use the hpack output to figure out which cabal files to use, but
  -- that misses projects that have explicit .cabal files, so just scan.
  let ignore = (mkPackageName . takeBaseName) <$> cabals
  (project, freeze) <- stackToCabal fremotes ignore dir stack
  hack <- extractHack . decodeUtf8 <$> BS.readFile (dir </> "stack.yaml")
  BS.writeFile (dir </> "cabal.project") (encodeUtf8 $ printProject project hack)
  BS.writeFile (dir </> "cabal.project.freeze") (encodeUtf8 $ printFreeze freeze)
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

