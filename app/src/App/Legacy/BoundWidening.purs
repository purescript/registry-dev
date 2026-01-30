-- | Evidence-based manifest correction using package set data.
-- |
-- | This module provides helpers for widening dependency bounds in legacy
-- | packages based on evidence from package sets. If a package compiled with
-- | a specific dependency version in a package set, that provides proof the
-- | dependency works at that version, and we can use this to correct overly
-- | strict bounds.
module Registry.App.Legacy.BoundWidening
  ( PackageSetEvidence
  , LegacyPublishContext
  , buildPackageSetEvidence
  , widenRangeWithEvidence
  , widenManifestDependencies
  , widenLegacyIndex
  ) where

import Registry.App.Prelude

import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Set as Set
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Solver as Solver
import Registry.Version as Version

-- | Evidence from package sets: for each (package, version) pair,
-- | records which versions of each dependency it compiled with.
-- | Key: package name, Value: Map of version -> (Map of dependency -> Set of working dep versions)
type PackageSetEvidence = Map PackageName (Map Version (Map PackageName (Set Version)))

-- | Context for legacy publishing, containing both the legacy index and evidence.
type LegacyPublishContext =
  { legacyIndex :: Solver.TransitivizedRegistry
  , packageSetEvidence :: PackageSetEvidence
  }

-- | Build evidence map from all package sets.
-- | For each package in each set, record what versions of its dependencies
-- | were used (i.e., the other packages in the same set).
buildPackageSetEvidence
  :: Map Version PackageSet
  -> ManifestIndex
  -> PackageSetEvidence
buildPackageSetEvidence packageSets manifestIndex =
  foldl addPackageSetEvidence Map.empty packageSets
  where
  addPackageSetEvidence :: PackageSetEvidence -> PackageSet -> PackageSetEvidence
  addPackageSetEvidence evidence (PackageSet { packages }) =
    foldlWithIndex (addPackageEvidence packages) evidence packages

  addPackageEvidence
    :: Map PackageName Version
    -> PackageName
    -> PackageSetEvidence
    -> Version
    -> PackageSetEvidence
  addPackageEvidence setPackages pkgName evidence pkgVersion =
    case ManifestIndex.lookup pkgName pkgVersion manifestIndex of
      Nothing -> evidence
      Just (Manifest { dependencies }) ->
        Map.insertWith
          (Map.unionWith (Map.unionWith Set.union))
          pkgName
          (Map.singleton pkgVersion depEvidence)
          evidence
        where
        depEvidence :: Map PackageName (Set Version)
        depEvidence =
          Map.mapMaybeWithKey
            (\depName _ -> map Set.singleton (Map.lookup depName setPackages))
            dependencies

-- | Widen a range to include evidence, using "next breaking boundary" rule.
-- | If evidence shows version V works, widen upper bound to < (next breaking version of V).
-- | Uses Version.bumpHighest which follows SemVer conventions:
-- |   - For X.Y.Z where X >= 1: bump major (1.2.3 -> 2.0.0)
-- |   - For 0.Y.Z where Y >= 1: bump minor (0.2.3 -> 0.3.0)
-- |   - For 0.0.Z: bump patch (0.0.3 -> 0.0.4)
widenRangeWithEvidence :: Range -> Set Version -> Range
widenRangeWithEvidence original observedVersions =
  case Set.findMax observedVersions of
    Nothing -> original
    Just maxObserved ->
      if Range.includes original maxObserved then
        original
      else do
        let
          newUpper = Version.bumpHighest maxObserved
          currentLower = Range.greaterThanOrEq original
          currentUpper = Range.lessThan original

        if newUpper > currentUpper then
          Range.mk currentLower newUpper # fromMaybe original
        else
          original

-- | Apply package set evidence to widen bounds in a manifest's dependencies.
-- |
-- | Evidence lookup strategy:
-- | 1. First try exact version match (pkg@X.Y.Z evidence for pkg@X.Y.Z)
-- | 2. If no exact match, aggregate evidence from all versions in the same
-- |    major series (pkg@X.*.* evidence for pkg@X.Y.Z)
-- |
-- | Rationale for same-major propagation:
-- | - Package sets only contain specific versions, but many patch/minor versions
-- |   exist that aren't in any set
-- | - If arrays@7.1.0 compiled with bifunctors@6.0.0, arrays@7.0.0 likely does too
-- | - SemVer says major version changes are breaking, so we stay within major
-- | - The compile gate catches any incorrect widening, and we fall back to
-- |   original bounds if solving fails (see API.purs lines 686-700)
widenManifestDependencies
  :: PackageSetEvidence
  -> PackageName
  -> Version
  -> Map PackageName Range
  -> Map PackageName Range
widenManifestDependencies evidence pkgName pkgVersion deps =
  -- First widen based on evidence, then apply manual overrides to narrow
  -- upper bounds for known-incompatible versions
  applyBoundOverrides pkgName pkgVersion widenedDeps
  where
  widenedDeps = case lookupEvidenceWithFallback of
    Nothing -> deps
    Just depEvidence ->
      mapWithIndex
        ( \depName range ->
            case Map.lookup depName depEvidence of
              Nothing -> range
              Just observedVersions -> widenRangeWithEvidence range observedVersions
        )
        deps

  -- Try exact version first, then fall back to aggregated same-major evidence
  lookupEvidenceWithFallback :: Maybe (Map PackageName (Set Version))
  lookupEvidenceWithFallback =
    case Map.lookup pkgName evidence of
      Nothing -> Nothing
      Just versionEvidence ->
        -- First try exact match
        case Map.lookup pkgVersion versionEvidence of
          Just exact -> Just exact
          Nothing ->
            -- No exact match: aggregate evidence from all versions in the same major series.
            -- For example, if we want evidence for arrays@7.0.0 but only have arrays@7.1.0
            -- in the package sets, we use that evidence.
            let
              sameMajorVersions = Map.filterKeys (sameMajor pkgVersion) versionEvidence
            in
              if Map.isEmpty sameMajorVersions then
                Nothing
              else
                -- Merge all evidence from same-major versions: union the dep version sets
                Just $ foldl (Map.unionWith Set.union) Map.empty sameMajorVersions

  -- Check if two versions share the same major version.
  -- For 0.x versions, we also require same minor (0.1.x vs 0.2.x are different series).
  -- For 0.0.x versions, every patch is breaking, so we require exact match (no fallback).
  sameMajor :: Version -> Version -> Boolean
  sameMajor v1 v2 =
    let
      m1 = Version.major v1
      m2 = Version.major v2
      n1 = Version.minor v1
      n2 = Version.minor v2
    in
      if m1 == 0 && m2 == 0 then
        if n1 == 0 && n2 == 0 then
          -- For 0.0.x versions, every patch is effectively breaking (per SemVer: 0.0.MAJOR)
          -- Don't propagate evidence across patches - require exact match only
          false
        else
          -- For 0.x versions where x > 0, minor is effectively the major version
          n1 == n2
      else
        m1 == m2

-- | Apply widening to the entire legacy index.
widenLegacyIndex
  :: PackageSetEvidence
  -> Map PackageName (Map Version (Map PackageName Range))
  -> Map PackageName (Map Version (Map PackageName Range))
widenLegacyIndex evidence =
  mapWithIndex \pkgName ->
    mapWithIndex \pkgVersion deps ->
      widenManifestDependencies evidence pkgName pkgVersion deps

-- | Manual bound overrides for packages where automatic widening picks
-- | incompatible versions. These are cases where a dependency made breaking
-- | changes without a major version bump.
-- |
-- | Format: Map of (package, version) -> Map of (dependency -> upper bound)
boundOverrides :: Map (Tuple PackageName Version) (Map PackageName Version)
boundOverrides = Map.fromFoldable
  [ -- pirates-charm 0.0.1 only compiles with hyrule <2.4.0
    -- hyrule 2.4.0 changed Event internals (STFn2 vs EffectFn2)
    Tuple
      (Tuple (unsafeFromRight $ PackageName.parse "pirates-charm") (unsafeFromRight $ Version.parse "0.0.1"))
      (Map.singleton (unsafeFromRight $ PackageName.parse "hyrule") (unsafeFromRight $ Version.parse "2.4.0"))
  ]

-- | Apply manual bound overrides to narrow upper bounds for known-incompatible
-- | dependency versions.
applyBoundOverrides
  :: PackageName
  -> Version
  -> Map PackageName Range
  -> Map PackageName Range
applyBoundOverrides pkgName pkgVersion deps =
  case Map.lookup (Tuple pkgName pkgVersion) boundOverrides of
    Nothing -> deps
    Just overrides ->
      deps # mapWithIndex \depName range ->
        case Map.lookup depName overrides of
          Nothing -> range
          Just maxVersion -> do
            -- Narrow upper bound to < maxVersion if current upper is higher
            let
              currentUpper = Range.lessThan range
              currentLower = Range.greaterThanOrEq range

            if currentUpper > maxVersion then
              Range.mk currentLower maxVersion # fromMaybe range
            else
              range
