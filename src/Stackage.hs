{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Stackage where

import           Control.Applicative ((<|>))
import qualified Data.ByteString     as BS
import           Data.List           (intersect, nub, (\\))
import           Data.Text           (Text, intercalate, map, pack)
import           Data.Text.IO        (putStrLn)
import           Data.YAML
import qualified Options.Applicative as Opts
import           Prelude             hiding (map, putStrLn)

-- stack.yaml can be parsed as a Stack and a Resolver
data Stack = Stack
  { packages   :: PkgName
  -- TODO ghcOptions
  }

data Resolver = Resolver
  { resolver :: ResolverRef
  , compiler :: Maybe String
  , packages :: [Package]
  , flags    :: [Flags]
  }

data ResolverRef = LTS String
                 | Snapshot FilePath
                 -- TODO: remote snapshots

data Package = Hackage PkgName Version
             | Remote Git Commit [SubDir]

newtype PkgName = PkgName String
newtype Version = Version String -- Cabal version string
newtype Git = Git String -- URL
newtype Commit = Commit String -- hash
newtype SubDir = SubDir String

data Flags = Flags PkgName [Flag]
data Flag = Flag String Bool
