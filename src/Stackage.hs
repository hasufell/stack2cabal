{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Duplicates a subset of the Stack ADT. It'd be nice if we could just re-use
-- the actual ADT, but stack isn't available as a library that we can build from
-- Hackage.
module Stackage where

import           Control.Applicative          ((<|>))
import           Data.Map.Strict              (Map, empty)
import           Data.Text                    (Text, isSuffixOf, unpack)
import           Data.YAML
import           Distribution.Text            (simpleParse)
import           Distribution.Types.PackageId (PackageIdentifier)
--import           Distribution.Types.PackageName (PackageName)
import           Options.Applicative.Internal (hoistMaybe)

data Stack = Stack
  { resolver  :: ResolverRef
  , compiler  :: Maybe Ghc
  , packages  :: [Package]
  , extraDeps :: [Dep]
  , flags     :: Flags
  -- TODO ghcOptions
  } deriving (Show)

newtype Ghc = Ghc Text
  deriving (Show)
  deriving newtype (FromYAML)

data Package = Local Text
             | Location Git
               deriving (Show)

data Git = Git Repo Commit [Subdir] -- https://github.com/haskell-hvr/HsYAML/pull/5 for NonEmpty
           deriving (Show)
type Repo = Text
type Commit = Text
type Subdir = Text

data Resolver = Resolver
  { resolver :: Maybe ResolverRef
  , compiler :: Maybe Ghc
  , packages :: [Dep]
  , flags    :: Flags
  } deriving (Show)

data ResolverRef = Canned LTS
                 | Snapshot Text
                 deriving (Show)
                 -- TODO: remote snapshots

-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Types-PackageId.html#t:PackageIdentifier
-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Parsec-Class.html#v:simpleParsec

data Dep = Hackage PackageIdentifier
         | SourceDep Git
         deriving (Show)

newtype Flags = Flags (Map PkgName (Map FlagName Bool))
              deriving (Show)
              deriving newtype (FromYAML)

type PkgName = Text
type FlagName = Text

-- Canned resolvers point to LTS or snapshots here
-- https://github.com/commercialhaskell/stackage-snapshots/blob/master/lts/12/20.yaml
newtype LTS = LTS Text deriving (Show)

--------------------------------------------------------------------------------
-- YAML boilerplate

instance FromYAML Stack where
   parseYAML = withMap "Stack" $ \m -> Stack
       <$> m .: "resolver"
       <*> m .:? "compiler"
       <*> m .:? "packages" .!= mempty
       <*> m .:? "extra-deps" .!= mempty
       <*> m .:? "flags" .!= (Flags empty)

instance FromYAML Git where
  parseYAML = withMap "Git" $ \m -> Git
    <$> m .: "git"
    <*> m .: "commit"
    <*> m .:? "subdirs" .!= (pure ".")

instance FromYAML ResolverRef where
  parseYAML = withStr "ResolverRef" $ \s ->
    if isSuffixOf ".yaml" s
    then (pure . Snapshot) s
    else (pure . Canned . LTS) s

instance FromYAML Package where
  parseYAML n = (local n) <|> (location n)
    where
      local = withStr "Local" $ pure . Local
      location = withMap "Location" $ \m ->
        Location <$> m .: "location"

instance FromYAML Dep where
   parseYAML n = (hackage n) <|> (source n)
     where
       hackage = withStr "Hackage" $ \s ->
         Hackage <$> (hoistMaybe . simpleParse . unpack) s
       source n' = SourceDep <$> parseYAML n'

instance FromYAML Resolver where
  parseYAML = withMap "Resolver" $ \m -> Resolver
    <$> m .:? "resolver"
    <*> m .:? "compiler"
    <*> m .:? "packages" .!= mempty
    <*> m .:? "flags" .!= (Flags empty)
