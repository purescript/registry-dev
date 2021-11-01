module Registry.PackageGraph where

import Registry.Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List as List
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Semigroup.First (First(..))
import Data.Set as Set
import Foreign.Object as Object
import Foreign.SemVer (Range, SemVer)
import Foreign.SemVer as SemVer
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)
import Safe.Coerce (coerce)

{-
Goal: Given a RegistryIndex, provide a maximally-valid-set of (PackageName, SemVer) pairs.


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

type ConstraintArgs =
  { satisfied :: Array PackageWithVersion
  , constraints :: Map PackageWithVersion (Array PackageWithRange)
  }

type ConstraintResult =
  { satisfied :: Array PackageWithVersion
  , unsolved :: Map PackageWithVersion (Array PackageWithRange)
  }

type CheckResult =
  { unsatisfied :: Map PackageWithVersion (Array PackageWithRange)
  , index :: RegistryIndex
  }

checkRegistryIndex :: RegistryIndex -> CheckResult
checkRegistryIndex index = do
  let
    constraints :: Map PackageWithVersion (Array PackageWithRange)
    constraints = Map.fromFoldableWith append do
      Tuple package versions' <- (Map.toUnfoldable index :: Array _)
      Tuple version manifest <- (Map.toUnfoldable versions' :: Array _)
      [ Tuple { package, version } [] ] <> do
        lib <- maybe [] pure $ Object.lookup "lib" manifest.targets
        let
          deps :: Array PackageWithRange
          deps = do
            Tuple p' range <- Object.toUnfoldable lib.dependencies
            p <- maybe [] pure $ hush $ PackageName.parse p'
            pure { package: p, range }

        pure $ Tuple { package, version } deps

    constraintResults = go { satisfied: mempty, constraints }

    checkedIndex :: RegistryIndex
    checkedIndex = Map.fromFoldable do
      Tuple package versions <- (Map.toUnfoldable index :: Array _)
      pure $ Tuple package $ Map.fromFoldable do
        Tuple version manifest <- (Map.toUnfoldable versions :: Array _)
        if Array.elem { package, version } constraintResults.satisfied then
          pure $ Tuple version manifest
        else
          []

  { index: checkedIndex, unsatisfied: constraintResults.unsolved }
  where
  go :: ConstraintArgs -> ConstraintResult
  go { satisfied, constraints } = do
    let
      go' (Tuple s c) (Tuple package dependencies) = do
        let
          dependencies' = Array.filter (not <<< isSolved (s <> satisfied)) dependencies
        if Array.null dependencies' then
          Tuple (s <> pure package) c
        else
          Tuple s (Map.insert package dependencies' c)

      Tuple solved constraints' =
        Array.foldl go' (Tuple [] Map.empty) (Map.toUnfoldable constraints :: Array _)

    if Array.null solved then
      { satisfied, unsolved: constraints' }
    else
      go { satisfied: satisfied <> solved, constraints: constraints' }

  isSolved :: Array PackageWithVersion -> PackageWithRange -> Boolean
  isSolved solved { package: neededPackage, range } =
    Array.any (\{ package: solvedPackage, version } -> solvedPackage == neededPackage && SemVer.satisfies version range) solved

type PackageGraph = Graph (Tuple PackageName SemVer) Manifest

-- We will store `maxSatisfying` for all dependencies
toPackageGraph :: RegistryIndex -> PackageGraph
toPackageGraph index =
  Graph.fromMap
    $ (Map.fromFoldable :: _ -> Map (Tuple PackageName SemVer) (Tuple Manifest (List (Tuple PackageName SemVer))))
    -- Need Array (Tuple (Tuple PackageName SemVer) (Tuple Manifest (List (Tuple PackageName SemVer))))
    $ map (map resolveDependencies)
    -- Array (Tuple (Tuple PackageName SemVer) Manifest)
    $ foldMap flatten
    -- Array (Tuple PackageName (Array (Tuple SemVer Manifest)))
    $ map (map (Map.toUnfoldable :: Map _ _ -> Array _))
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
        List.fromFoldable
          -- Can we crash if we can't find a satisfying range?
          $ map resolveDependency
          $ map (lmap unsafeParsePackageName)
          $ (Object.toUnfoldable :: Object _ -> Array (Tuple String _))
          -- Assuming that different targets have the same version ranges for dependencies
          $ foldMap (_.dependencies >>> (coerce :: Object Range -> Object (First Range)))
          -- TODO: Do we only care about `lib`?
          $ maybe [] pure (Object.lookup "lib" manifest.targets)
    Tuple manifest deps

  resolveDependency :: Tuple PackageName (First Range) -> Tuple PackageName SemVer
  resolveDependency (Tuple dependency (First range)) = Tuple dependency $ unsafePartial $ fromJust do
    depVersions <- Map.lookup dependency allVersions
    SemVer.maxSatisfying depVersions range

  unsafeParsePackageName :: String -> PackageName
  unsafeParsePackageName = fromRight' (\_ -> unsafeCrashWith "PackageName must parse") <<< PackageName.parse

inOrder :: RegistryIndex -> List Manifest
inOrder index = do
  let graph = toPackageGraph index

  List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph
