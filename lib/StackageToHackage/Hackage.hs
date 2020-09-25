{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | A simplistic model of cabal multi-package files and convertors from Stackage.
module StackageToHackage.Hackage
  ( stackToCabal
  , Project(..), printProject
  , Freeze(..), printFreeze
  ) where

import           Data.List                      (sort)
import           Data.List.Extra                (nubOrdOn)
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import qualified Data.List.NonEmpty             as NEL
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromMaybe, mapMaybe, catMaybes)
import           Data.Semigroup
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Distribution.Pretty            (prettyShow)
import           Distribution.Types.PackageId   (PackageIdentifier(..))
import           Distribution.Types.PackageName (PackageName, unPackageName)
import           StackageToHackage.Stackage
import           System.FilePath                (addTrailingPathSeparator)

-- | Converts a stack.yaml (and list of local packages) to cabal.project and
-- cabal.project.freeze.
stackToCabal :: [PackageName] -> FilePath -> Stack -> IO (Project, Freeze)
stackToCabal ignore dir stack = do
  resolvers <- unroll dir stack
  let resolver = sconcat resolvers
      project = genProject stack resolver
      freeze = genFreeze resolver ignore
  pure (project, freeze)

printProject :: Project -> Maybe Text -> Text
printProject (Project (Ghc ghc) pkgs srcs) hack =
  T.concat $ [ "-- Generated by stackage-to-hackage\n\n"
         , "with-compiler: ", ghc, "\n\n"
         , "packages:\n    ", packages, "\n\n"
         , sources, "\n"
         , "allow-older: *\n"
         , "allow-newer: *\n"
         ] <> verbatim hack
  where
    verbatim Nothing = []
    verbatim (Just txt) = ["\n-- Verbatim\n", txt]
    packages = T.intercalate "\n  , " (T.pack . addTrailingPathSeparator <$>
                                     NEL.toList pkgs)
    sources = T.intercalate "\n" (source =<< srcs)
    source Git{repo, commit, subdirs} =
      let base = T.concat [ "source-repository-package\n    "
                        , "type: git\n    "
                        , "location: ", repo, "\n    "
                        , "tag: ", commit, "\n"]
      in if null subdirs
         then [base]
         else (\d -> T.concat [base, "    subdir: ", d, "\n"]) <$> subdirs

data Project = Project Ghc (NonEmpty FilePath) [Git] deriving (Show)

genProject :: Stack -> Resolver -> Project
genProject stack Resolver{compiler, deps} = Project
  (fromMaybe (Ghc "ghc") compiler)
  (localDirs stack `appendList` localDeps deps)
  (nubOrdOn repo $ mapMaybe pickGit deps)
  where
    pickGit (Hackage _ )  = Nothing
    pickGit (LocalDep _)  = Nothing
    pickGit (SourceDep g) = Just g
    --
    localDeps = catMaybes . map fromLocalDeps
    fromLocalDeps (Hackage _) = Nothing
    fromLocalDeps (SourceDep _) = Nothing
    fromLocalDeps (LocalDep d) = Just d
    --
    appendList :: NonEmpty a -> [a] -> NonEmpty a
    appendList (x:|xs) ys = x:|(xs++ys)

-- TODO if there is a dependency listed in the snapshot or LTS but the user
-- provides a git repo or local package, we are generating the wrong version
-- constraint. We would need to parse the .cabal of all git repos in the same
-- way that we exclude self packages.
printFreeze :: Freeze -> Text
printFreeze (Freeze deps (Flags flags)) =
  T.concat [ "constraints: ", constraints, "\n"]
  where
    spacing = ",\n             "
    constraints = T.intercalate spacing (constrait <$> sort deps)
    constrait pkg =
      let name = (T.pack . unPackageName . pkgName $ pkg)
          ver  = (T.pack . prettyShow . pkgVersion $ pkg)
          base = T.concat ["any.", name, " ==", ver]
      in case M.lookup name flags of
        Nothing      -> base
        Just entries -> T.concat [name, " ", (custom entries), spacing, base]
    custom (M.toList -> lst) = T.intercalate " " $ (renderFlag <$> lst)
    renderFlag (name, True)  = "+" <> name
    renderFlag (name, False) = "-" <> name

data Freeze = Freeze [PackageIdentifier] Flags deriving (Show)

genFreeze :: Resolver -> [PackageName] -> Freeze
genFreeze Resolver{deps, flags} ignore =
  let pkgs = filter noSelfs $ unPkgId <$> mapMaybe pick deps
      uniqpkgs = nubOrdOn pkgName pkgs
   in Freeze uniqpkgs flags
  where pick (Hackage p)   = Just p
        pick (SourceDep _) = Nothing
        pick (LocalDep _) = Nothing
        noSelfs (pkgName -> n) = notElem n ignore
