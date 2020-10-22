{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StackageToHackage.Stackage.Types where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Semigroup
import Data.Text (Text)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Prelude hiding (head, reverse, takeWhile)


data Stack = Stack
  { resolver  :: ResolverRef
  , compiler  :: Maybe Ghc
  , packages  :: [Package]
  , extraDeps :: [Dep]
  , flags     :: Flags
  , ghcOptions :: GhcOptions
  } deriving (Show)


newtype Ghc = Ghc Text
  deriving (Show)


data Package = Local FilePath
             | Location Git
               deriving (Show)


data Git = Git
  { repo    :: Repo
  , commit  :: Commit
  , subdirs :: [Subdir]
  } deriving (Show, Eq, Ord)


type Repo = Text
type Commit = Text
type Subdir = Text


-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Types-PackageId.html#t:PackageIdentifier
-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Parsec-Class.html#v:simpleParsec

data Dep = Hackage PkgId
         | SourceDep Git
         | LocalDep FilePath
         deriving (Show)


newtype Flags = Flags (Map PkgName (Map FlagName Bool))
              deriving (Show)
              deriving newtype (Semigroup, Monoid)


newtype PackageGhcOpts = PackageGhcOpts (Map PkgId GhcFlags)
              deriving (Show)
              deriving newtype (Semigroup, Monoid)


data GhcOptions = GhcOptions
  { locals :: Maybe GhcFlags
  , targets :: Maybe GhcFlags  -- cabal doesn't know about these
  , everything :: Maybe GhcFlags
  , packagesGhcOpts :: PackageGhcOpts
  } deriving (Show)

emptyGhcOptions :: GhcOptions
emptyGhcOptions = GhcOptions Nothing Nothing Nothing mempty

type PkgName = Text
type FlagName = Text
type GhcFlags = Text


newtype NewDep = NewDep PkgId deriving (Show)


newtype PkgId = PkgId { unPkgId :: PackageIdentifier } deriving (Show, Ord, Eq)

--------------------------------------------------------------------------------
-- Resolvers

-- the format used at https://github.com/commercialhaskell/stackage-snapshots
-- which is similar to the Resolver format.
data NewResolver = NewResolver
  { compiler :: Ghc
  , packages :: [NewDep]
  , flags    :: Flags
  } deriving (Show)


data Resolver = Resolver
  { resolver :: Maybe ResolverRef
  , compiler :: Maybe Ghc
  , deps     :: [Dep]
  , flags    :: Flags
  } deriving (Show)


-- TODO: remote ResolverRefs
data ResolverRef = Canned Text
                 | Snapshot Text
                 deriving (Show)


type RelativeResolvers = NonEmpty (Maybe FilePath, Resolver)
type Resolvers = NonEmpty Resolver
