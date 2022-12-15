module Registry.Effect.PackageSets where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Effect.Cache (CACHE)
import Registry.App.Effect.GitHub (WRITE_GITHUB)
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Json as Json
import Registry.Constants (packageSetsDirectory)
import Registry.Constants as Constants
import Registry.PackageSet as PackageSet
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Except (FAIL)
import Run.Except as Exn
import Type.Proxy (Proxy(..))

data Change = Update Version | Remove

derive instance Eq Change

type ChangeSet = Map PackageName Change

type SequentialUpgradeResult =
  { failed :: ChangeSet
  , succeeded :: ChangeSet
  , result :: PackageSet
  }

data PackageSets a
  = ReadAll (Map Version PackageSet -> a)
  | ReadLatest (Maybe PackageSet -> a)
  | UpgradeAtomic PackageSet Version ChangeSet (Either ChangeSet PackageSet -> a)
  | UpgradeSequential PackageSet Version ChangeSet (SequentialUpgradeResult -> a)

derive instance Functor PackageSets

-- | An effect for interacting with package sets
type PACKAGE_SETS r = (packageSets :: PackageSets | r)

_packageSets :: Proxy "packageSets"
_packageSets = Proxy

-- | Read all package sets published by the registry.
readAll :: forall r. Run (PACKAGE_SETS + r) (Map Version PackageSet)
readAll = Run.lift _packageSets (ReadAll identity)

-- | Read the latest package set published by the registry.
readLatest :: forall r. Run (PACKAGE_SETS + r) (Maybe PackageSet)
readLatest = Run.lift _packageSets (ReadLatest identity)

-- | Upgrade the given package set using the provided compiler version and set
-- | of changes. If any change fails, then the upgrade is aborted and the
-- | unsuccessful changes are returned.
upgradeAtomic :: forall r. PackageSet -> Version -> ChangeSet -> Run (PACKAGE_SETS + r) (Either ChangeSet PackageSet)
upgradeAtomic oldSet compiler changes = Run.lift _packageSets (UpgradeAtomic oldSet compiler changes identity)

-- | Upgrade the given package set using the provided compiler version and set
-- | of changes. Any successful change is applied, and any unsuccessful changes
-- | are returned along with the new package set.
upgradeSequential :: forall r. PackageSet -> Version -> ChangeSet -> Run (PACKAGE_SETS + r) SequentialUpgradeResult
upgradeSequential oldSet compiler changes = Run.lift _packageSets (UpgradeSequential oldSet compiler changes identity)

type PackageSetsEnv =
  { registry :: FilePath
  }

-- | A handler for the PACKAGE_SETS effect which attempts to compile the package
-- | sets and commit the results.
handlePackageSetsAff :: forall r a. PackageSetsEnv -> PackageSets a -> Run (WRITE_GITHUB + STORAGE + CACHE + LOG + FAIL + AFF + r) a
handlePackageSetsAff env = case _ of
  ReadLatest reply -> do
    Log.debug "Reading latest package set..."
    versions <- listPackageSetVersions
    case Array.last (Array.sort versions) of
      Nothing -> do
        Log.error ("No package sets exist in directory " <> packageSetsDirectory)
        Exn.fail
      Just version -> do
        readPackageSetVersion version >>= case _ of
          Left err -> do
            Log.error $ "Found package set " <> Version.print version <> " but could not decode its contents: " <> err
            Exn.fail
          Right set -> do
            Log.debug $ "Successfully read package set version " <> Version.print version
            pure $ reply $ Just set

  ReadAll reply -> do
    Log.debug "Reading all package sets..."
    versions <- listPackageSetVersions
    decoded <- for versions \version -> map (bimap (Tuple version) (Tuple version)) (readPackageSetVersion version)
    let results = partitionEithers decoded
    case results.fail of
      [] -> do
        Log.debug "Successfully read all package sets."
        pure $ reply $ Map.fromFoldable results.success
      xs -> do
        let format (Tuple v err) = "\n  - " <> Version.print v <> ": " <> err
        Log.warn $ "Some package sets could not be read and were skipped: " <> Array.foldMap format xs
        pure $ reply $ Map.fromFoldable results.success

  UpgradeAtomic _oldSet _compiler changes reply ->
    pure $ reply (Left changes)

  UpgradeSequential oldSet _compiler changes reply ->
    pure $ reply { failed: changes, succeeded: changes, result: oldSet }

  where
  packageSetsDir :: FilePath
  packageSetsDir = Path.concat [ env.registry, Constants.packageSetsDirectory ]

  packageSetPath :: Version -> FilePath
  packageSetPath version = Path.concat [ packageSetsDir, Version.print version <> ".json" ]

  listPackageSetVersions :: Run _ (Array Version)
  listPackageSetVersions = do
    Log.debug "Reading package set versions..."
    files <- Run.liftAff (Aff.attempt (FS.Aff.readdir packageSetsDir)) >>= case _ of
      Left fsError -> do
        Log.error $ "Failed to read package set directory at path " <> packageSetsDir <> " due to an fs error: " <> Aff.message fsError
        Exn.fail
      Right paths ->
        pure paths

    let
      versions :: { fail :: Array String, success :: Array Version }
      versions = partitionEithers $ files <#> \file -> do
        name <- note "File has no .json suffix" $ String.stripSuffix (String.Pattern ".json") file
        Version.parse name

    case versions.fail of
      [] -> pure versions.success
      xs -> do
        Log.warn $ "Some package sets have invalid names and have been skipped: " <> String.joinWith ", " xs
        pure versions.success

  readPackageSetVersion :: Version -> Run _ (Either _ PackageSet)
  readPackageSetVersion version = do
    let path = packageSetPath version
    Run.liftAff (Json.readJsonFile PackageSet.codec path)
