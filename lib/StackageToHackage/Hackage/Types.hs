module StackageToHackage.Hackage.Types where

import StackageToHackage.Stackage.Types (Ghc, Git, GhcOptions, Flags)

import Data.List.NonEmpty (NonEmpty)
import Distribution.Types.PackageId (PackageIdentifier(..))


data Project = Project
    { ghc :: Ghc
    , pkgs :: NonEmpty FilePath
    , srcs :: [Git]
    , ghcOpts :: GhcOptions
    } deriving (Show)


data Freeze = Freeze [PackageIdentifier] Flags deriving (Show)
