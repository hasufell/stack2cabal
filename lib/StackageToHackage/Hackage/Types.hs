{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StackageToHackage.Hackage.Types where

import StackageToHackage.Stackage.Types (Ghc, Git, GhcOptions, PkgName, FlagName)

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Types.PackageName (unPackageName)

import qualified Data.Text as T


data Project = Project
    { ghc :: Ghc
    , pkgs :: NonEmpty FilePath
    , srcs :: [Git]
    , ghcOpts :: GhcOptions
    } deriving (Show)


newtype Freeze = Freeze [Constraint] deriving (Show)


data Constraint = VersionPin PackageIdentifier
                | FlagSetting PkgName (Map FlagName Bool)
    deriving (Show, Eq)

instance Ord Constraint where
    compare (VersionPin pkg1) (VersionPin pkg2) = compare pkg1 pkg2
    compare (FlagSetting pkg1 _) (FlagSetting pkg2 _) = compare pkg1 pkg2
    compare (VersionPin pkg1) (FlagSetting pkg2 _) =
        case compare (T.pack . unPackageName . pkgName $ pkg1) pkg2 of
            EQ -> GT
            x -> x
    compare (FlagSetting pkg1 _) (VersionPin pkg2) =
        case compare pkg1 (T.pack . unPackageName . pkgName $ pkg2) of
            EQ -> LT
            x -> x
