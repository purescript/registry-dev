module Registry.PackageGraph
  ( CheckedPackages
  , checkPackages
  , CheckedRegistryIndex
  , checkRegistryIndex
  , topologicalSort
  ) where

import Registry.Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (foldMap)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List as List
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Schema (Manifest(..))
import Registry.Version (Version)
import Registry.Version as Version

type PackageWithVersion = { package :: PackageName, version :: Version }

type PackageWithDependencies = { package :: PackageWithVersion, dependencies :: Array PackageName }

type CheckedPackages =
  { accepted :: Map PackageName Version
  , rejected :: Array { package :: PackageName, version :: Version, dependencies :: Array PackageName }
  }

-- Verify that a collection of package versions are self-contained (no package
-- has dependencies outside the provided set of packages).
checkPackages :: RegistryIndex -> Map PackageName Version -> CheckedPackages
checkPackages index packages = do
  let
    constraints :: Array PackageWithDependencies
    constraints = Array.sortWith (_.dependencies >>> Array.length) do
      Tuple package version <- Map.toUnfoldable packages
      case Map.lookup package index >>= Map.lookup version of
        Nothing -> unsafeCrashWith ("Unregistered package version: " <> show { package: PackageName.print package, version: Version.print version })
        Just (Manifest manifest) -> do
          let dependencies = Set.toUnfoldable (Map.keys manifest.dependencies)
          [ { package: { package, version }, dependencies } ]

    { satisfied, unsatisfied } =
      solveConstraints { satisfied: Map.empty, constraints }

    -- Our input maps a package name to a version, so all successes must also be
    -- a package name -> single version, despite the array returned by
    -- solveConstraints.
    accepted =
      filterMap Array.head satisfied

    rejected = unsatisfied <#> \{ package, dependencies } ->
      { package: package.package
      , version: package.version
      , dependencies
      }

  { accepted, rejected }

type CheckedRegistryIndex =
  { index :: RegistryIndex
  , unsatisfied :: Array { package :: PackageName, version :: Version, dependencies :: Array PackageName }
  }

-- Produces a maximally self-contained RegistryIndex, recording any unsatisfied dependencies.
checkRegistryIndex :: RegistryIndex -> CheckedRegistryIndex
checkRegistryIndex original = do
  let
    -- From a RegistryIndex, construct a list of constraints that we would like to satisfy,
    -- via the `dependencies` of each `Manifest`.
    -- Invariant: RegistryIndex is a Map of Maps, so the entries in the constraint list will be unique.
    -- We will maintain this throughout processing.
    -- Note: We sort these constraints by the amount of dependencies so that our first solver pass
    -- hits root Manifests first.
    constraints :: Array PackageWithDependencies
    constraints = Array.sortWith (_.dependencies >>> Array.length) do
      Tuple package versions' <- Map.toUnfoldable original
      Tuple version (Manifest manifest) <- Map.toUnfoldable versions'

      let
        dependencies :: Array PackageName
        dependencies = Set.toUnfoldable $ Map.keys manifest.dependencies

      [ { package: { package, version }, dependencies } ]

    { satisfied, unsatisfied: allUnsatisfied } =
      solveConstraints { satisfied: Map.empty, constraints }

    -- Once we have satisfied as many package dependencies as possible,
    -- prune provided RegistryIndex to only include Manifest entries with
    -- satisfied dependencies to create a self-contained RegistryIndex.
    index :: RegistryIndex
    index = Map.filter (not Map.isEmpty) $ Map.fromFoldable do
      Tuple package versions <- Map.toUnfoldable original
      Array.singleton $ Tuple package $ Map.fromFoldable do
        Tuple version manifest <- Map.toUnfoldable versions
        satisfiedVersions <- maybe [] Array.singleton (Map.lookup package satisfied)
        guard (Array.elem version satisfiedVersions) [ Tuple version manifest ]

    unsatisfied = allUnsatisfied <#> \{ package, dependencies } ->
      { package: package.package
      , version: package.version
      , dependencies
      }

  { index, unsatisfied }

type ConstraintArgs =
  { satisfied :: Map PackageName (Array Version)
  , constraints :: Array PackageWithDependencies
  }

type ConstraintResult =
  { satisfied :: Map PackageName (Array Version)
  , unsatisfied :: Array PackageWithDependencies
  }

solveConstraints :: ConstraintArgs -> ConstraintResult
solveConstraints args = do
  let
    -- Go through each constraint, recording satisfied manifest dependencies & updating constraints.
    { satisfied, iterationProgress, constraints } =
      Array.foldl go { satisfied: args.satisfied, iterationProgress: false, constraints: [] } args.constraints

  -- If there was progress, then we need to iterate through constraints again.
  -- When there is no progress, we have satisfied all of the constraints possible.
  if iterationProgress then
    solveConstraints { satisfied, constraints }
  else
    { satisfied, unsatisfied: constraints }
  where
  isSolved :: Map PackageName (Array Version) -> PackageName -> Boolean
  isSolved solved package = Map.member package solved

  -- Check if the given constraint is satisfied
  -- If so, add package/version to satisfied & record progress
  -- If not, update constraint to only include unsatisfied dependencies
  go { satisfied, iterationProgress, constraints } { package, dependencies } = do
    let
      unsolved = Array.filter (not <<< isSolved satisfied) dependencies
    if Array.null unsolved then
      { satisfied: Map.insertWith append package.package [ package.version ] satisfied
      , iterationProgress: true
      , constraints
      }
    else
      { satisfied
      , iterationProgress
      , constraints: constraints <> [ { package, dependencies: unsolved } ]
      }

topologicalSort :: RegistryIndex -> Array Manifest
topologicalSort index = do
  let graph = toPackageGraph index
  Array.reverse
    $ Array.fromFoldable
    $ List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph

type PackageGraph = Graph (Tuple PackageName Version) Manifest

-- We will construct an edge to each version of each dependency PackageName.
toPackageGraph :: RegistryIndex -> PackageGraph
toPackageGraph index =
  Graph.fromMap
    $ Map.fromFoldable
    $ map (map resolveDependencies)
    $ foldMap flatten
    $ map (map Map.toUnfoldable)
    $ (Map.toUnfoldable index :: Array (Tuple PackageName (Map Version Manifest)))
  where
  allVersions :: Map PackageName (Array Version)
  allVersions = map (Map.keys >>> Set.toUnfoldable) index

  flatten
    :: Tuple PackageName (Array (Tuple Version Manifest))
    -> Array (Tuple (Tuple PackageName Version) Manifest)
  flatten (Tuple packageName versions) = do
    Tuple version manifest <- versions
    pure $ Tuple (Tuple packageName version) manifest

  resolveDependencies :: Manifest -> Tuple Manifest (List (Tuple PackageName Version))
  resolveDependencies manifest@(Manifest { dependencies }) = do
    let deps = List.fromFoldable $ bindFlipped resolveDependency $ Map.toUnfoldable dependencies
    Tuple manifest deps

  resolveDependency :: Tuple PackageName Range -> Array (Tuple PackageName Version)
  resolveDependency (Tuple dependency _) = do
    depVersions <- maybe [] Array.singleton (Map.lookup dependency allVersions)
    version <- depVersions
    pure $ Tuple dependency version
