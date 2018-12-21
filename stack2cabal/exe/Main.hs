{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Data.ByteString                as BS
import qualified Data.List.NonEmpty             as NEL
import           Data.Text.Encoding             (encodeUtf8)
import           Distribution.Types.PackageName (mkPackageName)
import           Hpack                          (Force (..), Options (..),
                                                 defaultOptions, hpackResult,
                                                 resultCabalFile, setTarget)
import           StackageToHackage.Hackage      (printFreeze, printProject,
                                                 stackToCabal)
import           StackageToHackage.Stackage     (localDirs, readStack)
import           System.Directory               (getCurrentDirectory)
import           System.FilePath                (takeBaseName, (</>))

main :: IO ()
main = do
  dir <- getCurrentDirectory
  stack <- readStack =<< BS.readFile (dir </> "stack.yaml")
  let subs = NEL.toList $ (dir </>) <$> localDirs stack
  hResults <- traverse runHpack subs
  let ignore = (mkPackageName . takeBaseName . resultCabalFile) <$> hResults
  (project, freeze) <- stackToCabal ignore dir stack
  BS.writeFile (dir </> "cabal.project") (encodeUtf8 $ printProject project)
  BS.writeFile (dir </> "cabal.project.freeze") (encodeUtf8 $ printFreeze freeze)
  where
    runHpack sub = hpackResult $ setTarget (sub </> "package.yaml")
                                           (defaultOptions {optionsForce = Force})

-- TODO invoke cabal v2-freeze to minimise the freeze file
