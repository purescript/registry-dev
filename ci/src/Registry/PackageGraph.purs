module Registry.PackageGraph
  ( CheckResult
  , checkRegistryIndex
  , topologicalSort
  ) where

import Registry.Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List as List
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Foreign.Object as Object
import Foreign.SemVer (Range, SemVer)
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)

type Node =
  { manifest :: Manifest
  , packageName :: PackageName
  , version :: SemVer
  }

type PackageWithVersion = { package :: PackageName, version :: SemVer }

type PackageWithDependencies = { package :: PackageWithVersion, dependencies :: Array PackageName }

type ConstraintArgs =
  { satisfied :: Map PackageName (Array SemVer)
  , constraints :: Array PackageWithDependencies
  }

type ConstraintResult =
  { satisfied :: Map PackageName (Array SemVer)
  , unsatisfied :: Array PackageWithDependencies
  }

type CheckResult =
  { index :: RegistryIndex
  , unsatisfied :: Array { package :: PackageName, version :: SemVer, dependencies :: Array PackageName }
  }

-- Produces a maximally self-contained RegistryIndex, recording any unsatisfied dependencies.
checkRegistryIndex :: RegistryIndex -> CheckResult
checkRegistryIndex original = do
  let
    -- From a RegistryIndex, construct a list of constraints that we would like to satisfy,
    -- via the `dependencies` of the `lib` target of each `Manifest`.
    -- Invariant: RegistryIndex is a Map of Maps, so the entries in the constraint list will be unique.
    -- We will maintain this throughout processing.
    -- Note: We sort these constraints by the amount of dependencies so that our first solver pass
    -- hits root Manifests first.
    constraints :: Array PackageWithDependencies
    constraints = Array.sortWith (_.dependencies >>> Array.length) do
      Tuple package versions' <- Map.toUnfoldable original
      Tuple version manifest <- Map.toUnfoldable versions'
      let
        dependencies :: Array PackageName
        dependencies = do
          -- All valid manifests have a `lib` target, so this unsafe path is
          -- unreachable.
          Tuple p' _ <- Object.toUnfoldable $ unsafeGetLibDependencies manifest
          maybe [] Array.singleton $ hush $ PackageName.parse p'

      [ { package: { package, version }, dependencies } ]

    { satisfied, unsatisfied: allUnsatisfied } = go { satisfied: Map.empty, constraints }

    -- Once we have satisfied as many package dependencies as possible,
    -- prune provided RegistryIndex to only include Manifest entries with satisfied dependencies
    -- to create a self-contained RegistryIndex.
    index :: RegistryIndex
    index = Map.fromFoldable do
      Tuple package versions <- Map.toUnfoldable original
      Array.singleton $ Tuple package $ Map.fromFoldable do
        Tuple version manifest <- Map.toUnfoldable versions
        satisfiedVersions <- maybe [] Array.singleton (Map.lookup package satisfied)
        guard (Array.elem version satisfiedVersions) [ Tuple version manifest ]

    unsatisfied =
      map (\{ package: { package, version }, dependencies } -> { package, version, dependencies })
        allUnsatisfied

  { index, unsatisfied }
  where
  go :: ConstraintArgs -> ConstraintResult
  go args = do
    let
      -- Check if the given constraint is satisfied
      -- If so, add package/version to satisfied & record progress
      -- If not, update constraint to only include unsatisfied dependencies
      go' { satisfied, iterationProgress, constraints } { package, dependencies } = do
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

      -- Go through each constraint, recording satisfied manifest dependencies & updating constraints.
      { satisfied, iterationProgress, constraints } =
        Array.foldl go' { satisfied: args.satisfied, iterationProgress: false, constraints: [] } args.constraints

    -- If there was progress, then we need to iterate through constraints again.
    -- When there is no progress, we have satisfied all of the constraints possible.
    if iterationProgress then do
      go { satisfied, constraints }
    else
      { satisfied, unsatisfied: constraints }

  isSolved :: Map PackageName (Array SemVer) -> PackageName -> Boolean
  isSolved solved package = Map.member package solved

topologicalSort :: RegistryIndex -> Array Node
topologicalSort index = do
  let graph = toPackageGraph index
  Array.reverse
    $ Array.fromFoldable
    $ List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph

type PackageGraph = Graph (Tuple PackageName SemVer) Node

-- We will construct an edge to each version of each dependency PackageName.
-- Note: This function only looks at the `lib` target of a `Manifest`.
toPackageGraph :: RegistryIndex -> PackageGraph
toPackageGraph index =
  Graph.fromMap
    $ Map.fromFoldable
    $ map (map resolveDependencies)
    $ foldMap flatten
    $ map (map Map.toUnfoldable)
    $ (Map.toUnfoldable index :: Array (Tuple PackageName (Map SemVer Manifest)))
  where
  allVersions :: Map PackageName (Array SemVer)
  allVersions = map (Map.keys >>> Set.toUnfoldable) index

  flatten
    :: Tuple PackageName (Array (Tuple SemVer Manifest))
    -> Array (Tuple (Tuple PackageName SemVer) Node)
  flatten (Tuple packageName versions) = do
    Tuple version manifest <- versions
    pure $ Tuple (Tuple packageName version) { packageName, version, manifest }

  resolveDependencies :: Node -> Tuple Node (List (Tuple PackageName SemVer))
  resolveDependencies node@{ manifest } = do
    let
      deps =
        (List.fromFoldable :: Array _ -> _)
          $ bindFlipped resolveDependency
          $ map (lmap unsafeParsePackageName)
          $ Object.toUnfoldable
          $ unsafeGetLibDependencies manifest

    Tuple node deps

  resolveDependency :: Tuple PackageName Range -> Array (Tuple PackageName SemVer)
  resolveDependency (Tuple dependency _) = do
    depVersions <- maybe [] Array.singleton (Map.lookup dependency allVersions)
    version <- depVersions
    pure $ Tuple dependency version

  unsafeParsePackageName :: String -> PackageName
  unsafeParsePackageName = fromRight' (\_ -> unsafeCrashWith "PackageName must parse") <<< PackageName.parse

-- | For internal use only
unsafeGetLibDependencies :: Manifest -> Object Range
unsafeGetLibDependencies manifest =
  ( fromJust'
      (\_ -> unsafeCrashWith "Manifest has no 'lib' target in 'toPackageGraph'")
      (Object.lookup "lib" manifest.targets)
  ).dependencies
