{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Stackage where

import           Control.Applicative          ((<|>))
import           Data.Map.Strict              (lookupMin)
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
  , flags     :: [Flags]
  -- TODO ghcOptions
  }

newtype Ghc = Ghc Text

data Package = Local FilePath
             | Remote Git Commit [SubDir]

data Resolver = Resolver
  { resolver :: ResolverRef
  , compiler :: Maybe Ghc
  , packages :: [Dep]
  , flags    :: [Flags]
  }

data ResolverRef = Canned LTS
                 | Snapshot FilePath
                 -- TODO: remote snapshots

-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Types-PackageId.html#t:PackageIdentifier
-- http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Parsec-Class.html#v:simpleParsec

data Dep = Hackage PackageIdentifier
         | GitRepo Git Commit [SubDir]

newtype Git = Git Text -- URL
newtype Commit = Commit Text -- hash
newtype SubDir = SubDir Text

data Flags = Flags Text [Flag] -- PackageName
data Flag = Flag Text Bool

-- Canned resolvers point to LTS or snapshots here
-- https://github.com/commercialhaskell/stackage-snapshots/blob/master/lts/12/20.yaml
newtype LTS = LTS Text

--------------------------------------------------------------------------------
-- YAML boilerplate

instance FromYAML Stack where
   parseYAML = withMap "Stack" $ \m -> Stack
       <$> m .: "resolver"
       <*> m .: "compiler"
       <*> m .: "packages"
       <*> m .: "extra-deps"
       <*> m .: "flags"

instance FromYAML Ghc where
  parseYAML = withStr "Ghc" $ pure . Ghc

instance FromYAML ResolverRef where
  parseYAML = withStr "ResolverRef" $ \s ->
    if isSuffixOf ".yaml" s
    then (pure . Snapshot . unpack) s
    else (pure . Canned . LTS) s

-- instance FromYAML Resolver where
--   parseYAML = withMap "Resolver" $ \m -> Resolver
--     <$> m .: "resolver"

instance FromYAML Dep where
  parseYAML n = (hackage n) <|> (gitrepo n)
    where
      hackage = withStr "Hackage" $ \s ->
        Hackage <$> (hoistMaybe . simpleParsec . unpack) s
      gitrepo = withMap "GitRepo" $ \m -> GitRepo
                                  <$> m .: "git"
                                  <*> m .: "commit"
                                  <*> m .: "subdirs"

instance FromYAML Flags where
  parseYAML = withMap "Flags" $ \s ->
    do
      -- how can I get the key here?
      (_, fs) <- (hoistMaybe . lookupMin) s
      (Flags "FIXME") <$> (parseYAML fs)

instance FromYAML Flag where
  parseYAML = undefined

instance FromYAML Package where
   parseYAML n = (remote n) <|> (local n)
     where
       remote = withMap "Remote" $ \m -> Remote
                                         <$> m .: "git"
                                         <*> m .: "commit"
                                         <*> m .: "subdirs"
       local = withStr "Local" $ pure . Local . unpack

instance FromYAML Git where
  parseYAML = undefined

instance FromYAML Commit where
  parseYAML = undefined

instance FromYAML SubDir where
  parseYAML = undefined
