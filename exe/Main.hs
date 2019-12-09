{-# LANGUAGE DuplicateRecordFields #-}
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
import           Prelude                        hiding (lines)
import           StackageToHackage.Hackage      (printFreeze, printProject,
                                                 stackToCabal)
import           StackageToHackage.Stackage     (localDirs, readStack)
import           System.Directory               (doesDirectoryExist,
                                                 doesFileExist,
                                                 getCurrentDirectory,
                                                 listDirectory)
import           System.FilePath                (takeBaseName, takeExtension,
                                                 (</>))

main :: IO ()
main = do
  dir <- getCurrentDirectory
  stack <- readStack =<< BS.readFile (dir </> "stack.yaml")
  let subs = NEL.toList $ (dir </>) <$> localDirs stack
  hpacks <- filterM (\d -> doesFileExist $ hpackInput d) $ subs
  _ <- traverse runHpack hpacks
  cabals <- concat <$> traverse (globExt ".cabal") subs
  -- we could use the hpack output to figure out which cabal files to use, but
  -- that misses projects that have explicit .cabal files, so just scan.
  let ignore = (mkPackageName . takeBaseName) <$> cabals
  (project, freeze) <- stackToCabal ignore dir stack
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

