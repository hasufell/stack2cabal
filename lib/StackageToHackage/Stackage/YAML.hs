{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module StackageToHackage.Stackage.YAML where

import StackageToHackage.Prelude
import StackageToHackage.Stackage.Types


import Control.Applicative ((<|>))
import Data.Semigroup
import Data.Text (Text, isSuffixOf, takeWhile, unpack)
import Data.YAML
    ( FromYAML
    , Mapping
    , Node(..)
    , Parser
    , Pos(..)
    , Scalar(..)
    , parseYAML
    , withMap
    , withStr
    , (.!=)
    , (.:)
    , (.:?)
    )
import Distribution.Text (simpleParse)
import Prelude hiding (head, reverse, takeWhile)

import qualified Data.Map.Strict as M


fakePos :: Pos
fakePos = Pos
  { posByteOffset = -1 , posCharOffset = -1  , posLine = 1 , posColumn = 0 }


instance FromYAML Stack where
   parseYAML = withMap "Stack" $ \m -> Stack
       <$> m .: "resolver"
       <*> m .:? "compiler"
       <*> m .:? "packages" .!= mempty
       <*> m .:? "extra-deps" .!= mempty
       <*> m .:? "flags" .!= Flags M.empty
       <*> m .:? "ghc-options" .!= emptyGhcOptions

instance FromYAML GhcOptions where
  parseYAML = withMap "GhcOptions" $ \m -> do
      locals <- m .:? "$locals"
      targets <- m .:? "$targets"
      everything <- m .:? "$everything"
      packagesGhcOpts <- PackageGhcOpts <$>
          M.foldrWithKey (\k a action -> do
              m1 <- withStr "val" (\val -> do
                  key <- parseYAML k
                  pure (M.singleton key val)) a
              m2 <- action
              pure (m1 <> m2)
              ) (pure M.empty) (newMap m)
      pure $ GhcOptions{..}
   where
    newMap m =
        node "$everything" `M.delete`
            (node "$targets" `M.delete`
                (node "$locals" `M.delete` m))
    node = Scalar fakePos . SStr

instance FromYAML Git where
  parseYAML = withMap "Git" $ \m -> Git
      <$> (m .: "git" <|> github m)
      <*> m .: "commit"
      <*> m .:? "subdirs" .!= []
    where github m = ("git@github.com:" <>) <$> (m .: "github")

instance FromYAML ResolverRef where
  parseYAML = withStr "ResolverRef" $ \s ->
      if ".yaml" `isSuffixOf` s
      then (pure . Snapshot) s
      else (pure . Canned) s

instance FromYAML Package where
  parseYAML n = local n <|> location n
    where
      local = withStr "Local" $ pure . Local . unpack
      location = withMap "Location" $ \m ->
          Location <$> m .: "location"

instance FromYAML Dep where
   parseYAML n = hackage <|> source <|> local
     where
       hackage = Hackage <$> parseYAML n
       source = SourceDep <$> parseYAML n
       local = LocalDep . unpack <$> parseYAML n

instance FromYAML Resolver where
  parseYAML = withMap "Resolver" $ \m -> Resolver
      <$> m .:? "resolver"
      <*> m .:? "compiler"
      <*> m .:? "packages" .!= mempty
      <*> m .:? "flags" .!= Flags M.empty

instance FromYAML NewDep where
   parseYAML = withMap "NewDep" $ \m -> hackage' =<< m .: "hackage"
     where
       hackage' n = NewDep <$> parseYAML n

instance FromYAML NewResolver where
  parseYAML = withMap "NewResolver" $ \m -> NewResolver
      <$> (m .: "compiler" <|> m ..: ("resolver", "compiler"))
      <*> m .:? "packages" .!= mempty
      <*> m .:? "flags" .!= Flags M.empty
    where
      (..:) :: FromYAML a => Mapping Pos -> (Text, Text) -> Parser a
      m1 ..: (k1, k2) =
          case M.lookup (Scalar fakePos (SStr k1)) m1 of
              Just (Mapping _ _ m2) -> m2 .: k2
              _ -> fail $ "key " ++ show k1 ++ " not found"

instance FromYAML PkgId where
  parseYAML = withStr "PackageIdentifier" $ \s ->
      PkgId <$> (hoistMaybe . simpleParse . unpack) (takeWhile ('@' /=) s)


deriving instance FromYAML Ghc
deriving instance FromYAML PackageGhcOpts
deriving instance FromYAML Flags
