{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Stackage where

import           Control.Applicative          ((<|>))
import           Data.Map.Strict              (Map, empty)
import           Data.Text                    (Text, isSuffixOf, unpack)
import           Data.YAML
import           Distribution.Parsec.Class    (simpleParsec)
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

data Package = Local FilePath
             | Remote Git Commit [SubDir]
             deriving (Show)

data Resolver = Resolver
  { resolver :: ResolverRef
  , compiler :: Maybe Ghc
  , packages :: [Dep]
  , flags    :: Flags
  } deriving (Show)

data ResolverRef = Canned LTS
                 | Snapshot FilePath
                 deriving (Show)
                 -- TODO: remote snapshots

-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Types-PackageId.html#t:PackageIdentifier
-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Parsec-Class.html#v:simpleParsec

data Dep = Hackage PackageIdentifier
         | GitRepo Git Commit [SubDir]
         deriving (Show)

newtype Git = Git Text
  deriving (Show)
  deriving newtype (FromYAML) -- URL
newtype Commit = Commit Text
  deriving (Show)
  deriving newtype (FromYAML) -- hash
newtype SubDir = SubDir Text
  deriving (Show)
  deriving newtype (FromYAML)

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

instance FromYAML ResolverRef where
  parseYAML = withStr "ResolverRef" $ \s ->
    if isSuffixOf ".yaml" s
    then (pure . Snapshot . unpack) s
    else (pure . Canned . LTS) s

instance FromYAML Dep where
  parseYAML n = (hackage n) <|> (gitrepo n)
    where
      hackage = withStr "Hackage" $ \s ->
        Hackage <$> (hoistMaybe . simpleParsec . unpack) s
      gitrepo = withMap "GitRepo" $ \m -> GitRepo
                                  <$> m .: "git"
                                  <*> m .: "commit"
                                  <*> m .: "subdirs"

instance FromYAML Package where
   parseYAML n = (remote n) <|> (local n)
     where
       remote = withMap "Remote" $ \m -> Remote
                                         <$> m .: "git"
                                         <*> m .: "commit"
                                         <*> m .: "subdirs"
       local = withStr "Local" $ pure . Local . unpack
