module StackageToHackage.Hpack (execHpack, hpackInput) where


import Hpack
    ( defaultOptions
    , hpackResult
    , setTarget
    , Options(optionsForce)
    , Result
    , Force(Force)
    )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)


hpackInput :: FilePath -> FilePath
hpackInput sub = sub </> "package.yaml"

opts :: Options
opts = defaultOptions { optionsForce = Force }

execHpack :: FilePath -> IO Result
execHpack sub = do
    hPutStrLn stderr ("Running hpack in: " <> sub)
    hpackResult $ setTarget (hpackInput sub) opts
