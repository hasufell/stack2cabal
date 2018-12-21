{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Control.Monad                  (filterM)
import           Control.Monad.Extra            (ifM)
import qualified Data.ByteString                as BS
import qualified Data.List.NonEmpty             as NEL
import           Data.Text.Encoding             (encodeUtf8)
import           Distribution.Types.PackageName (mkPackageName)
import           Hpack                          (Force (..), Options (..),
                                                 defaultOptions, hpackResult,
                                                 setTarget)
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
  hpacks <- filterM doesFileExist $ subs
  _ <- traverse runHpack hpacks
  cabals <- concat <$> traverse (globExt ".cabal") subs
  -- we could use the hpack output to figure out which cabal files to use, but
  -- that misses projects that have explicit .cabal files, so just scan.
  let ignore = (mkPackageName . takeBaseName) <$> cabals
  (project, freeze) <- stackToCabal ignore dir stack
  BS.writeFile (dir </> "cabal.project") (encodeUtf8 $ printProject project)
  BS.writeFile (dir </> "cabal.project.freeze") (encodeUtf8 $ printFreeze freeze)
  where
    target sub = sub </> "package.yaml"
    opts = defaultOptions {optionsForce = Force}
    runHpack sub = hpackResult $ setTarget (target sub) opts

globExt :: String -> FilePath -> IO [FilePath]
globExt ext path = do
  files <- ifM (doesDirectoryExist path) (listDirectory path) (pure [])
  pure $ filter ((ext ==) . takeExtension) files

-- TODO invoke cabal v2-freeze to minimise the freeze file
