module Registry.PackageGraph where

import Registry.Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List as List
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Monoid (guard)
import Data.Set as Set
import Foreign.Object as Object
import Foreign.SemVer (Range, SemVer)
import Foreign.SemVer as SemVer
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)

{-
Goal: Given a RegistryIndex, provide a maximally self-contained RegistryIndex.

Definitions:
- A (PackageName, SemVer) pair is "valid" if all of it's dependencies (for the "lib" target) are also valid.
  - (PackageName, SemVer) pairs with no dependencies in the "lib" target are trivially valid.

Reason: Lots of things can go wrong when running LegacyImport to create a RegistryIndex.
        We need to construct the RegistryIndex from legacy files, and then take all of those
        entries and prune them to only include valid entries at the end.
        We do this after the rest of LegacyImport because we want to avoid including a package
        but then drop one of it's dependencies for some reason later.

Complications:
Package dependencies specify a `Range`, not a concrete `SemVer`.
If we have to drop a (PackageName, SemVer), we need to make sure we don't drop entries
which relied on it until we have checked that there are no other valid versions which satisfy the range.
For any given dependency, we want to use the `maxSatisfying` valid version that satisfies the range.

Insight:
When processing, we don't care which dependency SemVer matches a Range, just that one exists.
Can do a post-processing step to figure out which of the SemVer `maxSatisfying` Range
-}

type PackageWithVersion = { package :: PackageName, version :: SemVer }

type PackageWithRange = { package :: PackageName, range :: Range }

type PackageWithDependencies = { package :: PackageWithVersion, dependencies :: Array PackageWithRange }

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
  , unsatisfied :: Array PackageWithDependencies
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
    -- hits root Manifests first. This seems to be slightly faster (but not as significant as I'd think).
    constraints :: Array PackageWithDependencies
    constraints = Array.sortWith (_.dependencies >>> Array.length) do
      Tuple package versions' <- Map.toUnfoldable original
      Tuple version manifest <- Map.toUnfoldable versions'
      let
        -- All valid manifests have a `lib` target.
        -- If they have no dependencies, then lib.dependencies is [].
        lib =
          fromJust' (\_ -> unsafeCrashWith "Manifest missing lib target") $ Object.lookup "lib" manifest.targets

        dependencies :: Array PackageWithRange
        dependencies = do
          Tuple p' range <- Object.toUnfoldable lib.dependencies
          p <- maybe [] Array.singleton $ hush $ PackageName.parse p'
          [ { package: p, range } ]

      [ { package: { package, version }, dependencies } ]

    { satisfied, unsatisfied } = go { satisfied: Map.empty, constraints }

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

  { index, unsatisfied }
  where
  go :: ConstraintArgs -> ConstraintResult
  go args = do
    let
      -- Check if the given constraint is satisfied
      -- If so, add package/version to satisfied & record progress
      -- If not, update constraint to only include unsatisfied dependencies
      go' { satisfied, progress, constraints } { package, dependencies } = do
        let
          unsolved = Array.filter (not <<< isSolved satisfied) dependencies
        if Array.null unsolved then
          { satisfied: Map.insertWith append package.package [ package.version ] satisfied
          , progress: progress <> [ package ]
          , constraints
          }
        else
          { satisfied
          , progress
          , constraints: constraints <> [ { package, dependencies: unsolved } ]
          }

      -- Go through each constraint, recording satisfied manifest dependencies & updating constraints.
      { satisfied, progress, constraints } =
        Array.foldl go' { satisfied: args.satisfied, progress: [], constraints: [] } args.constraints

    -- When there is no progress, we have satisfied all of the constraints possible.
    -- If there was progress, then we need to iterate through constraints again.
    if Array.null progress then do
      { satisfied, unsatisfied: constraints }
    else
      go { satisfied, constraints }

  isSolved :: Map PackageName (Array SemVer) -> PackageWithRange -> Boolean
  isSolved solved { package, range } = fromMaybe false do
    versions <- Map.lookup package solved
    Just $ Array.any (flip SemVer.satisfies range) versions

type PackageGraph = Graph (Tuple PackageName SemVer) Manifest

-- We will store `maxSatisfying` for all dependencies.
-- Note: This function only looks at the `lib` target of a `Manifest`.
-- This will crash if dependencies are unsatisfied. It is intended to be used after `checkRegistryIndex`.
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
    -> Array (Tuple (Tuple PackageName SemVer) Manifest)
  flatten (Tuple packageName versions) = do
    Tuple version manifest <- versions
    pure $ Tuple (Tuple packageName version) manifest

  resolveDependencies
    :: Manifest
    -> Tuple Manifest (List (Tuple PackageName SemVer))
  resolveDependencies manifest = do
    let
      deps =
        (List.fromFoldable :: Array _ -> _)
          $ map resolveDependency
          $ map (lmap unsafeParsePackageName)
          $ Object.toUnfoldable
          $ _.dependencies
          $ fromJust' (\_ -> unsafeCrashWith "Manifest missing lib target")
          $ Object.lookup "lib" manifest.targets
    Tuple manifest deps

  resolveDependency :: Tuple PackageName Range -> Tuple PackageName SemVer
  resolveDependency (Tuple dependency range) =
    Tuple dependency $ fromJust' (\_ -> unsafeCrashWith "Failed to resolve dependency, have you run `checkRegistryIndex`?") do
      depVersions <- Map.lookup dependency allVersions
      SemVer.maxSatisfying depVersions range

  unsafeParsePackageName :: String -> PackageName
  unsafeParsePackageName = fromRight' (\_ -> unsafeCrashWith "PackageName must parse") <<< PackageName.parse

-- This will crash if dependencies are unsatisfied. It is intended to be used after `checkRegistryIndex`.
inOrder :: RegistryIndex -> List Manifest
inOrder index = do
  let graph = toPackageGraph index

  List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph
