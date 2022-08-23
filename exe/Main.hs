{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import StackageToHackage.Hackage
    ( InspectRemotes(..), PinGhc(..), SortRepos(..), RunHpack(..)
    , printFreeze, printProject, stackToCabal
    )
import StackageToHackage.Hpack (hpackInput, execHpack)
import StackageToHackage.Stackage (localDirs, readStack)

import Control.Exception (throwIO)
import Control.Monad (filterM, when)
import Data.Dates.Parsing (parseDateTime, defaultConfigIO)
import Data.Foldable (traverse_)
import Data.Hourglass (timeConvert, Elapsed)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Options.Applicative
import Prelude hiding (lines)
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T


version :: String
#ifdef CURRENT_PACKAGE_VERSION
version = CURRENT_PACKAGE_VERSION
#else
version = "unknown"
#endif


data Opts = Opts
  { input :: FilePath
  , output :: Maybe FilePath
  , inspectRemotes :: Bool
  , pinGHC :: Bool
  , sortRepos :: Bool
  , runHpack :: Bool
  , hackageIndexDate :: Maybe String -- ^ fuzzy date string
  }


optsP :: Parser Opts
optsP =
    Opts
        <$> strOption
                (short 'f'
                <> long "file"
                <> metavar "STACK_YAML"
                <> help "Path to stack.yaml file"
                <> value "stack.yaml"
                <> showDefaultWith show
                )
        <*> optional
                (strOption
                    (short 'o'
                    <> long "output-file"
                    <> metavar "CABAL_PROJECT"
                    <> help
                           "Path to output file (default: <project-dir>/cabal.project)"
                    <> showDefaultWith show
                    )
                )
        <*> (not <$> switch
                (long "no-inspect-remotes"
                <> help
                       "Don't check package names from remote git sources (this is faster, but may leave incorrect versions in cabal.project.freeze if remote packages overwrite stack resolver versions)"
                )
            )
        <*> (not <$> switch
                (long "no-pin-ghc" <> help "Don't pin the GHC version")
            )
        <*> (not <$> switch
                (long "no-sort-repos" <> help "Don't sort the source repositories")
            )
        <*> (not <$> switch (long "no-run-hpack" <> help "Don't run hpack"))
        <*> optional
                (strOption
                    (short 'p'
                    <> long "pin-hackage-index"
                    <> metavar "FUZZY_DATE"
                    <> help "Pin hackage index state (values like \"now\" and \"yesterday\" work)"
                    )
                )


main :: IO ()
main = do
    let versionHelp = infoOption version (long "version" <> help "Show version" <> hidden)

    customExecParser (prefs showHelpOnError) (info (optsP <**> helper <**> versionHelp) idm) >>= \Opts {..} -> do
        -- read stack file
        inDir <- makeAbsolute (takeDirectory input)
        stack <- readStack =<< BS.readFile input

        let subs = NEL.toList $ (inDir </>) <$> localDirs stack
        when runHpack $ do
            hpacks <-
                filterM (doesFileExist . hpackInput) subs
            traverse_ execHpack hpacks

        -- run conversion
        config <- defaultConfigIO
        let dt :: Maybe Elapsed
            dt = hackageIndexDate >>= \d -> fmap timeConvert
                $ either (const Nothing) Just
                $ parseDateTime config d
        case (hackageIndexDate, dt) of
            (Just d, Nothing) ->
                throwIO $ userError ("Warning: failed to convert hackage index state date \""
                    <> d <> "\"")
            _ -> pure ()
        (project, freeze) <- stackToCabal (InspectRemotes inspectRemotes) (RunHpack runHpack) inDir stack
        hack <- extractHack . decodeUtf8 <$> BS.readFile
            (inDir </> "stack.yaml")
        printText <- printProject (PinGhc pinGHC) (SortRepos sortRepos) dt project hack

        -- write files
        outFile <- case output of
            Just output' -> makeAbsolute output'
            Nothing -> pure (inDir </> "cabal.project")
        BS.writeFile outFile (encodeUtf8 printText)
        BS.writeFile
            (outFile <> ".freeze")
            (encodeUtf8 $ printFreeze freeze)


-- Backdoor allowing the stack.yaml to contain arbitrary text that will be
-- included in the cabal.project
extractHack :: Text -> Maybe Text
extractHack (T.split ('\n' ==) -> lines) =
    let (_, region) = break (T.isPrefixOf "#+BEGIN_STACK2CABAL") lines
        (hack, _) = break (T.isPrefixOf "#+END_STACK2CABAL") region
        verbatim = mapMaybe (T.stripPrefix "# ") hack
    in if null verbatim then Nothing else Just $ T.intercalate "\n" verbatim
