-- | An effect for interacting with registry data, such as metadata, manifests,
-- | and package sets.
module Registry.App.Effect.Registry where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Exists as Exists
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Effect.Cache (class MemoryEncodable, Cache, CacheRef, MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Git (GIT)
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.PackageSet (PscTag(..))
import Registry.App.Legacy.PackageSet as Legacy.Manifest
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Legacy.Types (legacyPackageSetCodec)
import Registry.Constants as Constants
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

data RegistryCache (c :: Type -> Type -> Type) a
  = AllManifests (c ManifestIndex a)
  | AllMetadata (c (Map PackageName Metadata) a)

instance Functor2 c => Functor (RegistryCache c) where
  map k (AllManifests a) = AllManifests (map2 k a)
  map k (AllMetadata a) = AllMetadata (map2 k a)

instance MemoryEncodable RegistryCache where
  encodeMemory (AllManifests next) = Exists.mkExists $ Key "ManifestIndex" next
  encodeMemory (AllMetadata next) = Exists.mkExists $ Key "AllMetadata" next

type REGISTRY_CACHE r = (registryCache :: Cache RegistryCache | r)

_registryCache :: Proxy "registryCache"
_registryCache = Proxy

data Registry a
  = ReadManifest PackageName Version (Either String (Maybe Manifest) -> a)
  | WriteManifest Manifest (Either String Unit -> a)
  | DeleteManifest PackageName Version (Either String Unit -> a)
  | ReadAllManifests (Either String ManifestIndex -> a)
  | ReadMetadata PackageName (Either String (Maybe Metadata) -> a)
  | WriteMetadata PackageName Metadata (Either String Unit -> a)
  | ReadAllMetadata (Either String (Map PackageName Metadata) -> a)
  | ReadLatestPackageSet (Either String (Maybe PackageSet) -> a)
  | WritePackageSet PackageSet String (Either String Unit -> a)
  | ReadAllPackageSets (Either String (Map Version PackageSet) -> a)
  -- Legacy operations
  | MirrorPackageSet PackageSet (Either String Unit -> a)
  | ReadLegacyRegistry (Either String { bower :: Map String String, new :: Map String String } -> a)
  | MirrorLegacyRegistry PackageName Location (Either String Unit -> a)

derive instance Functor Registry

-- | An effect for interacting with registry resources, like manifests, metadata,
-- | and the package sets.
type REGISTRY r = (registry :: Registry | r)

_registry :: Proxy "registry"
_registry = Proxy

-- | Read a manifest from the manifest index
readManifest :: forall r. PackageName -> Version -> Run (REGISTRY + EXCEPT String + r) (Maybe Manifest)
readManifest name version = Except.rethrow =<< Run.lift _registry (ReadManifest name version identity)

-- | Write a manifest to the manifest index
writeManifest :: forall r. Manifest -> Run (REGISTRY + EXCEPT String + r) Unit
writeManifest manifest = Except.rethrow =<< Run.lift _registry (WriteManifest manifest identity)

-- | Delete a manifest from the manifest index
deleteManifest :: forall r. PackageName -> Version -> Run (REGISTRY + EXCEPT String + r) Unit
deleteManifest name version = Except.rethrow =<< Run.lift _registry (DeleteManifest name version identity)

-- | Read the entire manifest index
readAllManifests :: forall r. Run (REGISTRY + EXCEPT String + r) ManifestIndex
readAllManifests = Except.rethrow =<< Run.lift _registry (ReadAllManifests identity)

-- | Read the registry metadata for a package
readMetadata :: forall r. PackageName -> Run (REGISTRY + EXCEPT String + r) (Maybe Metadata)
readMetadata name = Except.rethrow =<< Run.lift _registry (ReadMetadata name identity)

-- | Write the registry metadata for a package
writeMetadata :: forall r. PackageName -> Metadata -> Run (REGISTRY + EXCEPT String + r) Unit
writeMetadata name metadata = Except.rethrow =<< Run.lift _registry (WriteMetadata name metadata identity)

-- | Read the registry metadata for all packages
readAllMetadata :: forall r. Run (REGISTRY + EXCEPT String + r) (Map PackageName Metadata)
readAllMetadata = Except.rethrow =<< Run.lift _registry (ReadAllMetadata identity)

-- | Read the latest package set from the registry
readLatestPackageSet :: forall r. Run (REGISTRY + EXCEPT String + r) (Maybe PackageSet)
readLatestPackageSet = Except.rethrow =<< Run.lift _registry (ReadLatestPackageSet identity)

-- | Write a package set to the registry
writePackageSet :: forall r. PackageSet -> String -> Run (REGISTRY + EXCEPT String + r) Unit
writePackageSet set message = Except.rethrow =<< Run.lift _registry (WritePackageSet set message identity)

-- | Read all package sets from the registry
readAllPackageSets :: forall r. Run (REGISTRY + EXCEPT String + r) (Map Version PackageSet)
readAllPackageSets = Except.rethrow =<< Run.lift _registry (ReadAllPackageSets identity)

-- | Mirror a package set to the legacy package-sets repo
mirrorPackageSet :: forall r. PackageSet -> Run (REGISTRY + EXCEPT String + r) Unit
mirrorPackageSet set = Except.rethrow =<< Run.lift _registry (MirrorPackageSet set identity)

-- | Read the contents of the legacy registry.
readLegacyRegistry :: forall r. Run (REGISTRY + EXCEPT String + r) { bower :: Map String String, new :: Map String String }
readLegacyRegistry = Except.rethrow =<< Run.lift _registry (ReadLegacyRegistry identity)

-- | Mirror a package name and location to the legacy registry files.
mirrorLegacyRegistry :: forall r. PackageName -> Location -> Run (REGISTRY + EXCEPT String + r) Unit
mirrorLegacyRegistry name location = Except.rethrow =<< Run.lift _registry (MirrorLegacyRegistry name location identity)

interpret :: forall r a. (Registry ~> Run r) -> Run (REGISTRY + r) a -> Run r a
interpret handler = Run.interpret (Run.on _registry handler Run.send)

-- | Handle the REGISTRY effect by downloading the registry and registry-index
-- | repositories locally and reading and writing their contents from disk.
-- | Writes can optionally commit and push to the upstream Git repository.
-- |
-- | This handler enforces a memory-only cache: we do not want to cache on the
-- | file system or other storage because this handler relies on the registry
-- | Git repositories instead.
handle :: forall r a. CacheRef -> Registry a -> Run (GITHUB + GIT + LOG + AFF + EFFECT + r) a
handle ref = Cache.interpret _registryCache (Cache.handleMemory ref) <<< case _ of
  ReadManifest name version reply -> do
    let formatted = formatPackageVersion name version
    handle ref (ReadAllManifests identity) >>= case _ of
      Left error -> pure $ reply $ Left error
      Right index -> case ManifestIndex.lookup name version index of
        Nothing -> do
          Log.debug $ "Did not find manifest for " <> formatted <> " in memory cache or local registry repo checkout."
          pure $ reply $ Right Nothing
        Just manifest -> do
          pure $ reply $ Right $ Just manifest

  WriteManifest manifest@(Manifest { name, version }) reply -> map (map reply) Except.runExcept do
    let formatted = formatPackageVersion name version
    Log.info $ "Writing manifest for " <> formatted <> ":\n" <> printJson Manifest.codec manifest
    index <- Except.rethrow =<< handle ref (ReadAllManifests identity)
    case ManifestIndex.insert manifest index of
      Left error ->
        Except.throw $ Array.fold
          [ "Can't insert " <> formatted <> " into manifest index because it has unsatisfied dependencies:"
          , printJson (Internal.Codec.packageMap Range.codec) error
          ]
      Right updated -> do
        result <- Git.writeCommitPush (Git.CommitManifestEntry name) \indexPath -> do
          ManifestIndex.insertIntoEntryFile indexPath manifest >>= case _ of
            Left error -> Except.throw $ "Could not insert manifest for " <> formatted <> " into its entry file in WriteManifest: " <> error
            Right _ -> pure $ Just $ "Update manifest for " <> formatted
        case result of
          Left error -> Except.throw $ "Failed to write and commit manifest: " <> error
          Right r -> do
            case r of
              Git.NoChange -> Log.info "Did not commit manifest because it did not change."
              Git.Changed -> Log.info "Wrote and committed manifest."
            Cache.put _registryCache AllManifests updated

  DeleteManifest name version reply -> map (map reply) Except.runExcept do
    let formatted = formatPackageVersion name version
    Log.info $ "Deleting manifest for " <> formatted
    index <- Except.rethrow =<< handle ref (ReadAllManifests identity)
    case ManifestIndex.delete name version index of
      Left error ->
        Except.throw $ Array.fold
          [ "Can't delete " <> formatted <> " from manifest index because it would produce unsatisfied dependencies:"
          , printJson (Internal.Codec.packageMap (Internal.Codec.versionMap (Internal.Codec.packageMap Range.codec))) error
          ]
      Right updated -> do
        commitResult <- Git.writeCommitPush (Git.CommitManifestEntry name) \indexPath -> do
          ManifestIndex.removeFromEntryFile indexPath name version >>= case _ of
            Left error -> Except.throw $ "Could not remove manifest for " <> formatted <> " from its entry file in DeleteManifest: " <> error
            Right _ -> pure $ Just $ "Remove manifest entry for " <> formatted
        case commitResult of
          Left error -> Except.throw $ "Failed to delete and commit manifest: " <> error
          Right r -> do
            case r of
              Git.NoChange ->
                Log.info "Did not commit manifest because it already didn't exist."
              Git.Changed ->
                Log.info "Wrote and committed manifest."
            Cache.put _registryCache AllManifests updated

  ReadAllManifests reply -> map (map reply) Except.runExcept do
    let
      refreshIndex = do
        indexPath <- Git.getPath Git.ManifestIndexRepo
        index <- readManifestIndexFromDisk indexPath
        Cache.put _registryCache AllManifests index
        pure index

    Git.pull Git.ManifestIndexRepo >>= case _ of
      Left error ->
        Except.throw $ "Could not read manifests because the manifest index repo could not be checked: " <> error
      Right Git.NoChange -> do
        cache <- Cache.get _registryCache AllManifests
        case cache of
          Nothing -> do
            Log.info "No cached manifest index, reading from disk..."
            refreshIndex
          Just cached -> pure cached
      Right Git.Changed -> do
        Log.info "Manifest index has changed, replacing cache..."
        refreshIndex

  ReadMetadata name reply -> map (map reply) Except.runExcept do
    let printedName = PackageName.print name
    registryPath <- Git.getPath Git.RegistryRepo

    let
      path = Path.concat [ registryPath, Constants.metadataDirectory, printedName <> ".json" ]

      -- Attempt to read and decode the metadata file from the local checkout.
      readMetadataFromDisk = do
        Log.debug $ "Reading metadata for " <> printedName <> " from disk because it is not available in cache."
        Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
          Left fsError -> do
            Log.debug $ "Could not find metadata file for package " <> printedName <> ": " <> Aff.message fsError
            pure Nothing
          Right contents -> case Argonaut.Parser.jsonParser contents of
            Left jsonError ->
              Except.throw $ Array.fold
                [ "Found metadata file for " <> printedName <> " at path " <> path
                , ", but the file is not valid JSON: " <> jsonError
                , "\narising from contents:\n" <> contents
                ]
            Right parsed -> case CA.decode Metadata.codec parsed of
              Left decodeError -> do
                Except.throw $ Array.fold
                  [ "Found metadata file for " <> printedName <> " at path " <> path
                  , ", but could not decode the JSON" <> CA.printJsonDecodeError decodeError
                  , "\narising from contents:\n" <> contents
                  ]
              Right metadata -> do
                Log.debug $ "Successfully read metadata for " <> printedName <> " from path " <> path
                pure (Just metadata)

      -- Should be used when the cache may not be valid. Reads the metadata from
      -- disk and replaces the cache with it.
      resetFromDisk = readMetadataFromDisk >>= case _ of
        Nothing -> do
          Log.debug $ "Did not find " <> printedName <> " in memory cache or local registry repo checkout."
          pure Nothing

        Just metadata -> do
          Log.debug $ "Successfully read metadata for " <> printedName <> " from path " <> path
          Log.debug $ "Setting metadata cache to singleton entry (as cache was previosuly empty)."
          Cache.put _registryCache AllMetadata (Map.singleton name metadata)
          pure $ Just metadata

    Git.pull Git.RegistryRepo >>= case _ of
      Left error ->
        Except.throw $ "Could not read metadata because the registry repo could not be checked: " <> error

      Right Git.NoChange -> do
        Cache.get _registryCache AllMetadata >>= case _ of
          Nothing -> resetFromDisk
          Just allMetadata -> case Map.lookup name allMetadata of
            Nothing -> do
              Log.debug $ "Did not find " <> printedName <> " in memory cache, trying local registry checkout..."
              readMetadataFromDisk >>= case _ of
                Nothing -> do
                  Log.debug $ "Did not find " <> printedName <> " in memory cache or local registry repo checkout."
                  pure Nothing
                Just metadata -> do
                  Log.debug $ "Read metadata for " <> printedName <> " from path " <> path
                  Log.debug $ "Updating metadata cache to insert entry."
                  Cache.put _registryCache AllMetadata (Map.insert name metadata allMetadata)
                  pure $ Just metadata

            Just cached ->
              pure $ Just cached

      Right Git.Changed -> do
        Log.info "Registry repo has changed, clearing metadata cache..."
        resetFromDisk

  WriteMetadata name metadata reply -> map (map reply) Except.runExcept do
    let printedName = PackageName.print name
    Log.info $ "Writing metadata for " <> printedName
    Log.debug $ printJson Metadata.codec metadata
    commitResult <- Git.writeCommitPush (Git.CommitMetadataEntry name) \registryPath -> do
      let path = Path.concat [ registryPath, Constants.metadataDirectory, printedName <> ".json" ]
      Run.liftAff (Aff.attempt (writeJsonFile Metadata.codec path metadata)) >>= case _ of
        Left fsError -> Except.throw $ "Failed to write metadata for " <> printedName <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Right _ -> pure $ Just $ "Update metadata for " <> printedName
    case commitResult of
      Left error -> Except.throw $ "Failed to write and commit metadata: " <> error
      Right r -> do
        case r of
          Git.NoChange ->
            Log.info "Did not commit metadata because it was unchanged."
          Git.Changed ->
            Log.info "Wrote and committed metadata."
        cache <- Cache.get _registryCache AllMetadata
        for_ cache \cached ->
          Cache.put _registryCache AllMetadata (Map.insert name metadata cached)

  ReadAllMetadata reply -> map (map reply) Except.runExcept do
    let
      refreshMetadata = do
        registryPath <- Git.getPath Git.RegistryRepo
        let metadataDir = Path.concat [ registryPath, Constants.metadataDirectory ]
        Log.info $ "Reading metadata for all packages from directory " <> metadataDir
        allMetadata <- readAllMetadataFromDisk metadataDir
        Cache.put _registryCache AllMetadata allMetadata
        pure allMetadata

    Git.pull Git.RegistryRepo >>= case _ of
      Left error ->
        Except.throw $ "Could not read metadata because the registry repo could not be checked: " <> error
      Right Git.NoChange -> do
        Cache.get _registryCache AllMetadata >>= case _ of
          Nothing -> do
            Log.info "No cached metadata map, reading from disk..."
            refreshMetadata
          Just cached ->
            pure cached
      Right Git.Changed -> do
        Log.info "Registry repo has changed, replacing metadata cache..."
        refreshMetadata

  ReadLatestPackageSet reply -> map (map reply) Except.runExcept do
    Git.pull Git.RegistryRepo >>= case _ of
      Left error -> Except.throw $ "Could not read package sets because the registry repo could not be checked: " <> error
      Right _ -> pure unit
    registryPath <- Git.getPath Git.RegistryRepo
    let packageSetsDir = Path.concat [ registryPath, Constants.packageSetsDirectory ]
    Log.info $ "Reading latest package set from directory " <> packageSetsDir
    versions <- listPackageSetVersions packageSetsDir
    case Array.last (Array.sort versions) of
      Nothing ->
        Except.throw $ "Could not read latest package set because no package sets exist in local directory " <> packageSetsDir
      Just version -> do
        let printed = Version.print version
        let path = Path.concat [ packageSetsDir, printed <> ".json" ]
        Run.liftAff (readJsonFile PackageSet.codec path) >>= case _ of
          Left error ->
            Except.throw $ "Could not read package set " <> printed <> " from local path " <> path <> ": " <> error
          Right set -> do
            Log.debug $ "Successfully read package set " <> printed
            pure $ Just set

  WritePackageSet set@(PackageSet { version }) message reply -> map (map reply) Except.runExcept do
    Git.pull Git.RegistryRepo >>= case _ of
      Left error -> Except.throw $ "Could not read package sets because the registry repo could not be checked: " <> error
      Right _ -> pure unit
    let name = Version.print version
    Log.info $ "Writing package set " <> name
    commitResult <- Git.writeCommitPush (Git.CommitPackageSet version) \registryPath -> do
      let path = Path.concat [ registryPath, Constants.packageSetsDirectory, name <> ".json" ]
      Run.liftAff (Aff.attempt (writeJsonFile PackageSet.codec path set)) >>= case _ of
        Left fsError -> Except.throw $ "Failed to write package set " <> name <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Right _ -> pure $ Just message
    case commitResult of
      Left error -> Except.throw $ "Failed to write and commit package set: " <> error
      Right Git.NoChange -> Log.info "Did not commit package set because it was unchanged."
      Right Git.Changed -> Log.info "Wrote and committed package set."

  ReadAllPackageSets reply -> map (map reply) Except.runExcept do
    Git.pull Git.RegistryRepo >>= case _ of
      Left error -> Except.throw $ "Could not read package sets because the registry repo could not be checked: " <> error
      Right _ -> pure unit
    registryPath <- Git.getPath Git.RegistryRepo
    let packageSetsDir = Path.concat [ registryPath, Constants.packageSetsDirectory ]
    Log.info $ "Reading all package sets from directory " <> packageSetsDir
    versions <- listPackageSetVersions packageSetsDir
    decoded <- for versions \version -> do
      let printed = Version.print version
      let path = Path.concat [ packageSetsDir, printed <> ".json" ]
      map (bimap (Tuple version) (Tuple version)) $ Run.liftAff (readJsonFile PackageSet.codec path)
    let results = partitionEithers decoded
    case results.fail of
      [] -> do
        Log.debug "Successfully read all package sets."
        pure $ Map.fromFoldable results.success
      xs -> do
        let format (Tuple v err) = "\n  - " <> Version.print v <> ": " <> err
        Log.warn $ "Some package sets could not be read and were skipped: " <> Array.foldMap format xs
        pure $ Map.fromFoldable results.success

  -- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/release.sh
  -- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/update-latest-compatible-sets.sh
  MirrorPackageSet set@(PackageSet { version }) reply -> map (map reply) Except.runExcept do
    let name = Version.print version
    Log.info $ "Mirroring legacy package set " <> name <> " to the legacy package sets repo"

    manifests <- Except.rethrow =<< handle ref (ReadAllManifests identity)
    metadata <- Except.rethrow =<< handle ref (ReadAllMetadata identity)

    Log.debug $ "Converting package set..."
    converted <- case Legacy.Manifest.convertPackageSet manifests metadata set of
      Left error -> Except.throw $ "Failed to convert package set " <> name <> " to a legacy package set: " <> error
      Right converted -> pure converted

    let printedTag = Legacy.PackageSet.printPscTag converted.tag
    legacyRepo <- Git.getAddress Git.LegacyPackageSetsRepo

    packageSetsTags <- GitHub.listTags legacyRepo >>= case _ of
      Left githubError ->
        Except.throw $ Array.fold
          [ "Could not mirror package set " <> name
          , " because fetching tags from the legacy package-sets repo ("
          , legacyRepo.owner <> "/" <> legacyRepo.repo
          , ") failed: " <> Octokit.printGitHubError githubError
          ]
      Right tags -> pure $ Set.fromFoldable $ map _.name tags

    when (Set.member printedTag packageSetsTags) do
      Except.throw $ "Could not mirror package set " <> name <> " because the tag " <> printedTag <> " already exists."

    -- We need to write three files to the package sets repository:
    --
    -- * latest-compatible-sets.json
    --   stores a mapping of compiler versions to their highest compatible tag
    --
    -- * packages.json
    --   stores the JSON representation of the latest package set
    --
    -- * src/packages.dhall
    --   stores the Dhall representation of the latest package set
    let latestSetsPath = "latest-compatible-sets.json"
    let packagesJsonPath = "packages.json"
    let dhallPath = Path.concat [ "src", "packages.dhall" ]
    let files = [ latestSetsPath, packagesJsonPath, dhallPath ]
    let compilerKey = (un PscTag converted.tag).compiler

    commitFilesResult <- Git.writeCommitPush (Git.CommitLegacyPackageSets files) \legacyPath -> do
      latestCompatibleSets <- do
        latestSets <- Run.liftAff (readJsonFile Legacy.PackageSet.latestCompatibleSetsCodec (Path.concat [ legacyPath, latestSetsPath ])) >>= case _ of
          Left err -> Except.throw $ "Could not mirror package set because reading the latest compatible sets file from " <> latestSetsPath <> " failed: " <> err
          Right parsed -> pure parsed

        case Map.lookup compilerKey latestSets of
          Just existingTag | existingTag == converted.tag -> do
            Log.warn $ "Not updating latest-compatible-sets.json because the tag " <> printedTag <> " already exists."
            pure latestSets
          Just existingTag | existingTag > converted.tag -> do
            Log.warn $ Array.fold
              [ "Not updating latest-compatible-sets.json because an existing tag ("
              , Legacy.PackageSet.printPscTag existingTag
              , ") is higher than the tag we are pushing ("
              , Legacy.PackageSet.printPscTag converted.tag
              , ")."
              ]
            pure latestSets
          _ ->
            pure $ Map.insert compilerKey converted.tag latestSets

      -- Next we need to write the files that will be pushed to the package-sets repo
      Log.debug $ "Writing " <> dhallPath
      let fullDhallPath = Path.concat [ legacyPath, dhallPath ]
      Run.liftAff $ FS.Aff.writeTextFile UTF8 fullDhallPath (Legacy.PackageSet.printDhall converted.packageSet <> "\n")

      Log.debug $ "Writing " <> packagesJsonPath
      let fullPackagesJsonPath = Path.concat [ legacyPath, packagesJsonPath ]
      Run.liftAff $ writeJsonFile legacyPackageSetCodec fullPackagesJsonPath converted.packageSet

      Log.debug $ "Writing " <> latestSetsPath
      let fullLatestSetsPath = Path.concat [ legacyPath, latestSetsPath ]
      Run.liftAff $ writeJsonFile Legacy.PackageSet.latestCompatibleSetsCodec fullLatestSetsPath latestCompatibleSets

      pure $ Just $ "Update to the " <> name <> " package set."

    case commitFilesResult of
      Left error -> Except.throw $ "Failed to commit to legacy registry:" <> error
      Right Git.NoChange -> Log.info "Did not commit legacy registry files because nothing has changed."
      Right Git.Changed -> do
        Log.info "Committed legacy registry files."
        -- Now that we've written and pushed our commit, we also need to push some
        -- tags to trigger the legacy package sets release workflow.
        tagResult <- Git.tagAndPush Git.LegacyPackageSetsRepo do
          -- We push the stable tag (ie. just a compiler version) if one does not yet
          -- exist, and we always push the full tag.
          let stable = Version.print compilerKey
          Array.catMaybes
            [ Just printedTag
            , if Set.member stable packageSetsTags then Nothing else Just stable
            ]
        case tagResult of
          Left error ->
            Except.throw $ "Failed to push tags to legacy registry: " <> error
          Right Git.NoChange ->
            Log.warn $ "Tried to push tags to legacy registry, but there was no effect (they already existed)."
          Right Git.Changed ->
            Log.info "Pushed new tags to legacy registry."

  ReadLegacyRegistry reply -> map (map reply) Except.runExcept do
    registryPath <- Git.getPath Git.RegistryRepo
    Log.info $ "Reading legacy registry from " <> registryPath
    let readRegistryFile path = readJsonFile (CA.Common.strMap CA.string) (Path.concat [ registryPath, path ])
    bower <- Run.liftAff (readRegistryFile "bower-packages.json") >>= case _ of
      Left error -> Except.throw $ "Failed to read bower-packages.json file: " <> error
      Right packages -> pure packages
    new <- Run.liftAff (readRegistryFile "new-packages.json") >>= case _ of
      Left error -> Except.throw $ "Failed to read new-packages.json file: " <> error
      Right packages -> pure packages
    pure { bower, new }

  MirrorLegacyRegistry name location reply -> map (map reply) Except.runExcept do
    Log.debug $ "Mirroring package " <> PackageName.print name <> " to location " <> stringifyJson Location.codec location
    url <- case location of
      GitHub { owner, repo, subdir: Nothing } ->
        pure $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
      GitHub { owner, repo, subdir: Just dir } ->
        Except.throw $ Array.fold
          [ "Cannot mirror location " <> owner <> "/" <> repo
          , " to the legacy registry because it specifies a 'subdir' key (" <> dir
          , "), and the legacy registry does not support monorepos."
          ]
      Git { url } ->
        Except.throw $ "Cannot mirror location (Git " <> url <> ") because it is a Git location, and only GitHub packages are supported in the legacy registry."

    { bower, new } <- Except.rethrow =<< handle ref (ReadLegacyRegistry identity)

    let rawPackageName = "purescript-" <> PackageName.print name

    let
      -- Here we determine which, if any, legacy registry file should be updated with this package.
      -- If the package is new (ie. not listed in either registry file) then we insert it into the
      -- new-packages.json file. If not (ie. we found it in one of the registry files), and the location
      -- of the package in the registry file is different from its one in the registry metadata, then we
      -- update the package in that registry file. If the package exists at the proper location already
      -- then we do nothing.
      targetFile = case Map.lookup rawPackageName new, Map.lookup rawPackageName bower of
        Nothing, Nothing -> Just "new-packages.json"
        Just existingUrl, _
          | existingUrl /= url -> Just "new-packages.json"
          | otherwise -> Nothing
        _, Just existingUrl
          | existingUrl /= url -> Just "bower-packages.json"
          | otherwise -> Nothing

    result <- Git.writeCommitPush Git.CommitLegacyRegistry \registryPath -> do
      for_ targetFile \file -> do
        let sourcePackages = if file == "new-packages.json" then new else bower
        let packages = Map.insert rawPackageName url sourcePackages
        let path = Path.concat [ registryPath, file ]
        Run.liftAff $ writeJsonFile (CA.Common.strMap CA.string) path packages
      pure $ Just $ "Sync " <> PackageName.print name <> " with legacy registry."

    case result of
      Left error ->
        Except.throw $ "Failed to commit and push legacy registry files: " <> error
      Right Git.NoChange ->
        Log.info $ "Did not commit and push legacy registry files because there was no change."
      Right Git.Changed ->
        Log.info "Wrote and committed legacy registry files."

-- | Given the file path of a local manifest index on disk, read its contents.
readManifestIndexFromDisk :: forall r. FilePath -> Run (LOG + EXCEPT String + AFF + EFFECT + r) ManifestIndex
readManifestIndexFromDisk root = do
  paths <- FastGlob.match' root [ "**/*" ] { include: FastGlob.FilesOnly, ignore: [ "config.json", "README.md" ] }

  let
    packages = do
      let parsePath = Path.basename >>> \path -> lmap (Tuple path) (PackageName.parse path)
      partitionEithers $ map parsePath paths.succeeded

  unless (Array.null packages.fail) do
    Log.warn $ Array.fold
      [ "Some entries in the manifest index are not valid package names: "
      , Array.foldMap (\(Tuple path err) -> "\n  - " <> path <> ": " <> err) packages.fail
      ]

  entries <- map partitionEithers $ for packages.success (ManifestIndex.readEntryFile root)
  case entries.fail of
    [] -> case ManifestIndex.fromSet $ Set.fromFoldable $ Array.foldMap NonEmptyArray.toArray entries.success of
      Left errors -> do
        Except.throw $ append "Unable to read manifest index (some packages are not satisfiable): " $ Array.foldMap (append "\n  - ") do
          Tuple name versions <- Map.toUnfoldable errors
          Tuple version dependency <- Map.toUnfoldable versions
          let
            dependencies = do
              Tuple depName depRange <- Map.toUnfoldable dependency
              [ PackageName.print depName <> "(" <> Range.print depRange <> ")" ]
          pure $ Array.fold [ formatPackageVersion name version, " cannot satisfy: ", String.joinWith ", " dependencies ]

      Right index -> do
        Log.debug "Successfully read manifest index."
        pure index

    failed ->
      Except.throw $ append "Unable to read manifest index (some package entries cannot be decoded): " $ Array.foldMap (append "\n  - ") failed

-- | Given the file path of a directory of metadata on disk, read its contents.
readAllMetadataFromDisk :: forall r. FilePath -> Run (LOG + EXCEPT String + AFF + r) (Map PackageName Metadata)
readAllMetadataFromDisk metadataDir = do
  files <- Run.liftAff (Aff.attempt (FS.Aff.readdir metadataDir)) >>= case _ of
    Left err ->
      Except.throw $ "Could not metadata for all packages from path " <> metadataDir <> " due to an fs error: " <> Aff.message err
    Right paths ->
      pure paths

  let
    parsePath path = lmap (Tuple path) do
      base <- note "No .json suffix" $ String.stripSuffix (String.Pattern ".json") path
      name <- PackageName.parse base
      pure name

  let packages = partitionEithers (map parsePath files)
  unless (Array.null packages.fail) do
    Except.throw $ Array.fold
      [ "Could not read metadata for all packages becauses some entries in the metadata directory are not valid package names:"
      , Array.foldMap (\(Tuple path err) -> "\n  - " <> path <> ": " <> err) packages.fail
      ]

  entries <- Run.liftAff $ map partitionEithers $ for packages.success \name -> do
    result <- readJsonFile Metadata.codec (Path.concat [ metadataDir, PackageName.print name <> ".json" ])
    pure $ map (Tuple name) result

  unless (Array.null entries.fail) do
    Except.throw $ append "Could not read metadata for all packages because the metadata directory is invalid (some package metadata cannot be decoded):" $ Array.foldMap (append "\n  - ") entries.fail

  Log.debug "Successfully read metadata entries."
  pure $ Map.fromFoldable entries.success

-- List all package set versions found in the package sets directory by reading
-- each package set filename.
listPackageSetVersions :: forall r. FilePath -> Run (LOG + EXCEPT String + AFF + r) (Array Version)
listPackageSetVersions packageSetsDir = do
  Log.debug "Reading all package set versions..."
  files <- Run.liftAff (Aff.attempt (FS.Aff.readdir packageSetsDir)) >>= case _ of
    Left fsError ->
      Except.throw $ "Failed to read package set directory at path " <> packageSetsDir <> " due to an fs error: " <> Aff.message fsError
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
