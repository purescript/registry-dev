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
import Registry.App.Effect.Git (GIT, GitResult(..))
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.TypedCache (CacheKey, CacheRef, MemoryEncoder, MemoryEncoding(..), TypedCache)
import Registry.App.Effect.TypedCache as TypedCache
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

data Registry a
  = ReadManifest PackageName Version (Maybe Manifest -> a)
  | WriteManifest Manifest a
  | DeleteManifest PackageName Version a
  | ReadAllManifests (ManifestIndex -> a)
  | ReadMetadata PackageName (Maybe Metadata -> a)
  | WriteMetadata PackageName Metadata a
  | ReadAllMetadata (Map PackageName Metadata -> a)
  | ReadLatestPackageSet (Maybe PackageSet -> a)
  | WritePackageSet PackageSet String a
  | ReadAllPackageSets (Map Version PackageSet -> a)
  -- Legacy operations
  | MirrorPackageSet PackageSet a
  | ReadLegacyRegistry ({ bower :: Map String String, new :: Map String String } -> a)
  | MirrorLegacyRegistry PackageName Location a

derive instance Functor Registry

type REGISTRY r = (registry :: Registry | r)

_registry :: Proxy "registry"
_registry = Proxy

-- | Read a manifest from the manifest index
readManifest :: forall r. PackageName -> Version -> Run (REGISTRY + r) (Maybe Manifest)
readManifest name version = Run.lift _registry (ReadManifest name version identity)

-- | Write a manifest to the manifest index
writeManifest :: forall r. Manifest -> Run (REGISTRY + r) Unit
writeManifest manifest = Run.lift _registry (WriteManifest manifest unit)

-- | Delete a manifest from the manifest index
deleteManifest :: forall r. PackageName -> Version -> Run (REGISTRY + r) Unit
deleteManifest name version = Run.lift _registry (DeleteManifest name version unit)

-- | Read the entire manifest index
readAllManifests :: forall r. Run (REGISTRY + r) ManifestIndex
readAllManifests = Run.lift _registry (ReadAllManifests identity)

-- | Read the registry metadata for a package
readMetadata :: forall r. PackageName -> Run (REGISTRY + r) (Maybe Metadata)
readMetadata name = Run.lift _registry (ReadMetadata name identity)

-- | Write the registry metadata for a package
writeMetadata :: forall r. PackageName -> Metadata -> Run (REGISTRY + r) Unit
writeMetadata name metadata = Run.lift _registry (WriteMetadata name metadata unit)

-- | Read the registry metadata for all packages
readAllMetadata :: forall r. Run (REGISTRY + r) (Map PackageName Metadata)
readAllMetadata = Run.lift _registry (ReadAllMetadata identity)

-- | Read the latest package set from the registry
readLatestPackageSet :: forall r. Run (REGISTRY + r) (Maybe PackageSet)
readLatestPackageSet = Run.lift _registry (ReadLatestPackageSet identity)

-- | Write a package set to the registry
writePackageSet :: forall r. PackageSet -> String -> Run (REGISTRY + r) Unit
writePackageSet set message = Run.lift _registry (WritePackageSet set message unit)

-- | Read all package sets from the registry
readAllPackageSets :: forall r. Run (REGISTRY + r) (Map Version PackageSet)
readAllPackageSets = Run.lift _registry (ReadAllPackageSets identity)

-- | Mirror a package set to the legacy package-sets repo
mirrorPackageSet :: forall r. PackageSet -> Run (REGISTRY + r) Unit
mirrorPackageSet set = Run.lift _registry (MirrorPackageSet set unit)

-- | Read the contents of the legacy registry.
readLegacyRegistry :: forall r. Run (REGISTRY + r) { bower :: Map String String, new :: Map String String }
readLegacyRegistry = Run.lift _registry (ReadLegacyRegistry identity)

-- | Mirror a package name and location to the legacy registry files.
mirrorLegacyRegistry :: forall r. PackageName -> Location -> Run (REGISTRY + r) Unit
mirrorLegacyRegistry name location = Run.lift _registry (MirrorLegacyRegistry name location unit)

runRegistry :: forall r a. (Registry ~> Run r) -> Run (REGISTRY + r) a -> Run r a
runRegistry handler = Run.interpret (Run.on _registry handler Run.send)

-- | Handle the REGISTRY effect by downloading the registry and registry-index
-- | repositories locally and reading and writing their contents from disk.
-- | Writes can optionally commit and push to the uptsream Git repository.
handleRegistryGit :: forall r a. Registry a -> Run (REGISTRY_CACHE + GITHUB + GIT + LOG + LOG_EXCEPT + AFF + EFFECT + r) a
handleRegistryGit = case _ of
  ReadManifest name version reply -> do
    let formatted = formatPackageVersion name version
    Log.debug $ "Reading manifest for " <> formatted
    index <- handleRegistryGit (ReadAllManifests identity)
    case ManifestIndex.lookup name version index of
      Nothing -> do
        Log.debug $ "Did not find manifest for " <> formatted <> " in memory cache or local registry repo checkout."
        pure $ reply Nothing
      Just manifest -> do
        Log.debug $ "Read manifest for " <> formatted <> " from refreshed index."
        pure $ reply $ Just manifest

  WriteManifest manifest@(Manifest { name, version }) next -> do
    let formatted = formatPackageVersion name version
    Log.info $ "Writing manifest for " <> formatted <> ":\n" <> printJson Manifest.codec manifest
    let exitMessage = "Failed to write manifest for " <> formatted <> " to the manifest index."
    index <- handleRegistryGit (ReadAllManifests identity)
    case ManifestIndex.insert manifest index of
      Left error ->
        Log.exit $ Array.fold
          [ "Can't insert " <> formatted <> " into manifest index because it has unsatisfied dependencies:"
          , printJson (Internal.Codec.packageMap Range.codec) error
          ]
      Right updated -> do
        result <- Git.writeCommitPush (Git.CommitManifestEntry name) \indexPath -> do
          ManifestIndex.insertIntoEntryFile indexPath manifest >>= case _ of
            Left error -> do
              Log.error $ "Could not insert manifest for " <> formatted <> " into its entry file in WriteManifest: " <> error
              Log.exit exitMessage
            Right _ -> pure $ Just $ "Update manifest for " <> formatted
        case result of
          Left error -> Log.exit $ "Failed to write and commit manifest: " <> error
          Right NoChange -> Log.info "Did not commit manifest because it did not change." *> pure next
          Right Changed -> do
            Log.info "Wrote and committed manifest."
            putRegistryCache AllManifests updated
            pure next

  DeleteManifest name version next -> do
    let formatted = formatPackageVersion name version
    Log.info $ "Deleting manifest for " <> formatted
    let exitMessage = "Failed to delete manifest for " <> formatted <> " from the manifest index."
    index <- handleRegistryGit (ReadAllManifests identity)
    case ManifestIndex.delete name version index of
      Left error ->
        Log.exit $ Array.fold
          [ "Can't delete " <> formatted <> " from manifest index because it would produce unsatisfied dependencies:"
          , printJson (Internal.Codec.packageMap (Internal.Codec.versionMap (Internal.Codec.packageMap Range.codec))) error
          ]
      Right updated -> do
        commitResult <- Git.writeCommitPush (Git.CommitManifestEntry name) \indexPath -> do
          ManifestIndex.removeFromEntryFile indexPath name version >>= case _ of
            Left error -> do
              Log.error $ "Could not remove manifest for " <> formatted <> " from its entry file in DeleteManifest: " <> error
              Log.exit exitMessage
            Right _ -> pure $ Just $ "Remove manifest entry for " <> formatted
        case commitResult of
          Left error -> Log.exit $ "Failed to delete and commit manifest: " <> error
          Right NoChange -> Log.info "Did not commit manifest because it already didn't exist." *> pure next
          Right Changed -> do
            Log.info "Wrote and committed manifest."
            putRegistryCache AllManifests updated
            pure next

  ReadAllManifests reply -> do
    let
      refreshIndex = do
        indexPath <- Git.getPath Git.ManifestIndexRepo
        index <- readManifestIndexFromDisk indexPath
        putRegistryCache AllManifests index
        pure $ reply index

    Git.pull Git.ManifestIndexRepo >>= case _ of
      Left error ->
        Log.exit $ "Could not read manifests because the manifest index repo could not be checked: " <> error
      Right NoChange -> do
        Log.debug "Manifest index repo up to date, reading from cache..."
        cache <- getRegistryCache AllManifests
        case cache of
          Nothing -> do
            Log.info "No cached manifest index, reading from disk..."
            refreshIndex
          Just cached -> pure $ reply cached
      Right Changed -> do
        Log.info "Manifest index has changed, replacing cache..."
        refreshIndex

  ReadMetadata name reply -> do
    let printedName = PackageName.print name
    Log.debug $ "Reading metadata for " <> printedName
    registryPath <- Git.getPath Git.RegistryRepo

    let
      path = Path.concat [ registryPath, Constants.metadataDirectory, printedName <> ".json" ]

      -- Attempt to read and decode the metadata file from the local checkout.
      readMetadataFromDisk = do
        Log.debug $ "Reading metadata for " <> printedName <> " from disk because it is not available in cache."
        let exitMessage = "Found metadata for " <> printedName <> ", but it could not be read."
        Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
          Left fsError -> do
            Log.debug $ "Could not find metadata file for package " <> printedName <> ": " <> Aff.message fsError
            pure Nothing
          Right contents -> case Argonaut.Parser.jsonParser contents of
            Left jsonError -> do
              Log.error $ Array.fold
                [ "Found metadata file for " <> printedName <> " at path " <> path
                , ", but the file is not valid JSON: " <> jsonError
                , "\narising from contents:\n" <> contents
                ]
              Log.exit exitMessage
            Right parsed -> case CA.decode Metadata.codec parsed of
              Left decodeError -> do
                Log.error $ Array.fold
                  [ "Found metadata file for " <> printedName <> " at path " <> path
                  , ", but could not decode the JSON" <> CA.printJsonDecodeError decodeError
                  , "\narising from contents:\n" <> contents
                  ]
                Log.exit exitMessage
              Right metadata -> do
                Log.debug $ "Successfully read metadata for " <> printedName <> " from path " <> path
                pure (Just metadata)

      -- Should be used when the cache may not be valid. Reads the metadata from
      -- disk and replaces the cache with it.
      resetFromDisk = readMetadataFromDisk >>= case _ of
        Nothing -> do
          Log.debug $ "Did not find " <> printedName <> " in memory cache or local registry repo checkout."
          pure $ reply Nothing

        Just metadata -> do
          Log.debug $ "Successfully read metadata for " <> printedName <> " from path " <> path
          Log.debug $ "Setting metadata cache to singleton entry (as cache was previosuly empty)."
          putRegistryCache AllMetadata (Map.singleton name metadata)
          pure $ reply $ Just metadata

    Git.pull Git.RegistryRepo >>= case _ of
      Left error ->
        Log.exit $ "Could not read metadata because the registry repo could not be checked: " <> error

      Right NoChange -> do
        Log.debug "Registry repo up to date, reading from cache..."
        getRegistryCache AllMetadata >>= case _ of
          Nothing -> resetFromDisk
          Just allMetadata -> case Map.lookup name allMetadata of
            Nothing -> do
              Log.debug $ "Did not find " <> printedName <> " in memory cache, trying local registry checkout..."
              readMetadataFromDisk >>= case _ of
                Nothing -> do
                  Log.debug $ "Did not find " <> printedName <> " in memory cache or local registry repo checkout."
                  pure $ reply Nothing
                Just metadata -> do
                  Log.debug $ "Read metadata for " <> printedName <> " from path " <> path
                  Log.debug $ "Updating metadata cache to insert entry."
                  putRegistryCache AllMetadata (Map.insert name metadata allMetadata)
                  pure $ reply $ Just metadata

            Just cached ->
              pure $ reply $ Just cached

      Right Changed -> do
        Log.info "Registry repo has changed, clearing metadata cache..."
        resetFromDisk

  WriteMetadata name metadata next -> do
    let printedName = PackageName.print name
    Log.info $ "Writing metadata for " <> printedName
    Log.debug $ printJson Metadata.codec metadata
    commitResult <- Git.writeCommitPush (Git.CommitMetadataEntry name) \registryPath -> do
      let path = Path.concat [ registryPath, Constants.metadataDirectory, printedName <> ".json" ]
      let couldNotWriteError = "Could not write metadata for " <> printedName
      Run.liftAff (Aff.attempt (writeJsonFile Metadata.codec path metadata)) >>= case _ of
        Left fsError -> do
          Log.error $ "Failed to write metadata for " <> printedName <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
          Log.exit couldNotWriteError
        Right _ -> pure $ Just $ "Update metadata for " <> printedName
    case commitResult of
      Left error -> Log.exit $ "Failed to write and commit metadata: " <> error
      Right NoChange -> Log.info "Did not commit metadata because it was unchanged." *> pure next
      Right Changed -> do
        Log.info "Wrote and committed metadata."
        cache <- getRegistryCache AllMetadata
        for_ cache \cached ->
          putRegistryCache AllMetadata (Map.insert name metadata cached)
        pure next

  ReadAllMetadata reply -> do
    let
      refreshMetadata = do
        registryPath <- Git.getPath Git.RegistryRepo
        let metadataDir = Path.concat [ registryPath, Constants.metadataDirectory ]
        Log.info $ "Reading metadata for all packages from directory " <> metadataDir
        allMetadata <- readAllMetadataFromDisk metadataDir
        putRegistryCache AllMetadata allMetadata
        pure $ reply allMetadata

    Git.pull Git.RegistryRepo >>= case _ of
      Left error ->
        Log.exit $ "Could not read metadata because the registry repo could not be checked: " <> error
      Right NoChange -> do
        Log.debug "Registry repo up to date, reading from cache..."
        getRegistryCache AllMetadata >>= case _ of
          Nothing -> do
            Log.info "No cached metadata map, reading from disk..."
            refreshMetadata
          Just cached ->
            pure $ reply cached
      Right Changed -> do
        Log.info "Registry repo has changed, replacing metadata cache..."
        refreshMetadata

  ReadLatestPackageSet reply -> do
    registryPath <- Git.getPath Git.RegistryRepo
    let packageSetsDir = Path.concat [ registryPath, Constants.packageSetsDirectory ]
    Log.info $ "Reading latest package set from directory " <> packageSetsDir
    versions <- listPackageSetVersions packageSetsDir
    case Array.last (Array.sort versions) of
      Nothing -> do
        Log.error $ "No package sets exist in directory " <> packageSetsDir
        Log.exit "Could not read latest package set."
      Just version -> do
        let printed = Version.print version
        let path = Path.concat [ packageSetsDir, printed <> ".json" ]
        Run.liftAff (readJsonFile PackageSet.codec path) >>= case _ of
          Left error -> do
            Log.error $ "Could not read " <> path <> ": " <> error
            Log.exit $ "Could not read package set " <> printed
          Right set -> do
            Log.debug $ "Successfully read package set " <> printed
            pure $ reply $ Just set

  WritePackageSet set@(PackageSet { version }) message next -> do
    let name = Version.print version
    Log.info $ "Writing package set " <> name
    commitResult <- Git.writeCommitPush (Git.CommitPackageSet version) \registryPath -> do
      let path = Path.concat [ registryPath, Constants.packageSetsDirectory, name <> ".json" ]
      let couldNotWriteError = "Could not write package set " <> name
      Run.liftAff (Aff.attempt (writeJsonFile PackageSet.codec path set)) >>= case _ of
        Left fsError -> do
          Log.error $ "Failed to write package set " <> name <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
          Log.exit couldNotWriteError
        Right _ -> pure $ Just message
    case commitResult of
      Left error -> Log.exit $ "Failed to write and commit package set: " <> error
      Right NoChange -> Log.info "Did not commit package set because it was unchanged." *> pure next
      Right Changed -> Log.info "Wrote and committed package set." *> pure next

  ReadAllPackageSets reply -> do
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
        pure $ reply $ Map.fromFoldable results.success
      xs -> do
        let format (Tuple v err) = "\n  - " <> Version.print v <> ": " <> err
        Log.warn $ "Some package sets could not be read and were skipped: " <> Array.foldMap format xs
        pure $ reply $ Map.fromFoldable results.success

  -- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/release.sh
  -- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/update-latest-compatible-sets.sh
  MirrorPackageSet set@(PackageSet { version }) next -> do
    let name = Version.print version
    Log.info $ "Mirroring legacy package set " <> name <> " to the legacy package sets repo"

    manifests <- handleRegistryGit (ReadAllManifests identity)
    metadata <- handleRegistryGit (ReadAllMetadata identity)

    Log.debug $ "Converting package set..."
    converted <- case Legacy.Manifest.convertPackageSet manifests metadata set of
      Left error -> do
        Log.error $ "Failed to convert package set " <> name <> " to a legacy package set: " <> error
        Log.exit $ "Could not produce a legacy package set from package set " <> name
      Right converted -> pure converted

    let printedTag = Legacy.PackageSet.printPscTag converted.tag
    legacyRepo <- Git.getAddress Git.LegacyPackageSetsRepo

    packageSetsTags <- GitHub.listTags legacyRepo >>= case _ of
      Left githubError -> do
        Log.error $ Array.fold
          [ "Fetching tags for the repo " <> legacyRepo.owner <> "/" <> legacyRepo.repo
          , " failed: " <> Octokit.printGitHubError githubError
          ]
        Log.exit "Could not mirror package set because we were unable to fetch tags from the legacy package-sets repo."
      Right tags -> pure $ Set.fromFoldable $ map _.name tags

    when (Set.member printedTag packageSetsTags) do
      Log.error $ printedTag <> " already exists."
      Log.exit $ "Could not mirror package set because the tag " <> printedTag <> " already exists."

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
        latestSets <- Run.liftAff (readJsonFile Legacy.PackageSet.latestCompatibleSetsCodec latestSetsPath) >>= case _ of
          Left err -> do
            Log.error $ "Could not read latest-compatible-sets from " <> latestSetsPath <> " due to an error: " <> err
            Log.exit $ "Could not mirror package set because we could not read the latest-compatible-sets.json file."
          Right parsed -> pure parsed

        case Map.lookup compilerKey latestSets of
          Just existingTag | existingTag == converted.tag -> do
            Log.warn $ "Not updating latest-compatible sets because the tag " <> printedTag <> " already exists."
            pure latestSets
          Just existingTag | existingTag > converted.tag -> do
            Log.warn $ "Not updating latest-compatible sets because an existing tag is higher."
            pure latestSets
          _ ->
            pure $ Map.insert compilerKey converted.tag latestSets

      -- Next we need to write the files that will be pushed to the package-sets repo
      Log.debug $ "Writing " <> dhallPath
      let fullDhallPath = Path.concat [ legacyPath, dhallPath ]
      Run.liftAff $ FS.Aff.writeTextFile UTF8 fullDhallPath (Legacy.PackageSet.printDhall converted.packageSet)

      Log.debug $ "Writing " <> packagesJsonPath
      let fullPackagesJsonPath = Path.concat [ legacyPath, packagesJsonPath ]
      Run.liftAff $ writeJsonFile legacyPackageSetCodec fullPackagesJsonPath converted.packageSet

      Log.debug $ "Writing " <> latestSetsPath
      let fullLatestSetsPath = Path.concat [ legacyPath, latestSetsPath ]
      Run.liftAff $ writeJsonFile Legacy.PackageSet.latestCompatibleSetsCodec fullLatestSetsPath latestCompatibleSets

      pure $ Just $ "Update to the " <> name <> " package set."

    case commitFilesResult of
      Left error -> Log.exit $ "Failed to commit to legacy registry:" <> error
      Right NoChange -> Log.info "Did not commit legacy registry files because nothing has changed." *> pure next
      Right Changed -> do
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
            Log.exit $ "Failed to push tags to legacy registry: " <> error
          Right NoChange -> do
            Log.warn $ "Tried to push tags to legacy registry, but there was no effect (they already existed)."
            pure next
          Right Changed -> do
            Log.info "Pushed new tags to legacy registry."
            pure next

  ReadLegacyRegistry reply -> do
    registryPath <- Git.getPath Git.RegistryRepo
    Log.info $ "Reading legacy registry from " <> registryPath
    let readRegistryFile path = readJsonFile (CA.Common.strMap CA.string) (Path.concat [ registryPath, path ])
    bower <- Run.liftAff (readRegistryFile "bower-packages.json") >>= case _ of
      Left _ -> Log.exit "Failed to read bower-packages.json file."
      Right packages -> pure packages
    new <- Run.liftAff (readRegistryFile "new-packages.json") >>= case _ of
      Left _ -> Log.exit "Failed to read new-packages.json file."
      Right packages -> pure packages
    pure $ reply $ { bower, new }

  MirrorLegacyRegistry name location next -> do
    Log.debug $ "Mirroring package " <> PackageName.print name <> " to location " <> stringifyJson Location.codec location
    url <- case location of
      GitHub { owner, repo, subdir: Nothing } ->
        pure $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
      GitHub { owner, repo, subdir: Just dir } -> do
        Log.error $ "Cannot mirror location " <> owner <> "/" <> repo <> " because it specifies a subdirectory (" <> dir <> ")."
        Log.exit "Could not sync package with the legacy registry because it uses a 'subdir' key."
      Git { url } -> do
        Log.error $ "Cannot mirror location " <> url <> " because it is a Git location, and only GitHub is supported in the legacy registry."
        Log.exit "Could not sync package with the legacy registry because only GitHub packages are supported."

    { bower, new } <- handleRegistryGit $ ReadLegacyRegistry identity

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
      Left error -> do
        Log.exit $ "Failed to commit and push legacy registry files: " <> error
      Right NoChange -> do
        Log.info $ "Did not commit and push legacy registry files because there was no change."
        pure next
      Right Changed -> do
        Log.info "Wrote and committed legacy registry files."
        pure next

-- | Given the file path of a local manifest index on disk, read its contents.
readManifestIndexFromDisk :: forall r. FilePath -> Run (LOG + LOG_EXCEPT + AFF + EFFECT + r) ManifestIndex
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
        Log.error $ append "Unable to read manifest index (some packages are not satisfiable): " $ Array.foldMap (append "\n  - ") do
          Tuple name versions <- Map.toUnfoldable errors
          Tuple version dependency <- Map.toUnfoldable versions
          let
            dependencies = do
              Tuple depName depRange <- Map.toUnfoldable dependency
              [ PackageName.print depName <> "(" <> Range.print depRange <> ")" ]
          pure $ Array.fold [ formatPackageVersion name version, " cannot satisfy: ", String.joinWith ", " dependencies ]
        Log.exit "Failed to read the manifest index."

      Right index -> do
        Log.debug "Successfully read manifest index."
        pure index

    failed -> do
      Log.error $ append "Unable to read manifest index (some package entries cannot be decoded): " $ Array.foldMap (append "\n  - ") failed
      Log.exit "Failed to read the manifest index."

-- | Given the file path of a directory of metadata on disk, read its contents.
readAllMetadataFromDisk :: forall r. FilePath -> Run (LOG + LOG_EXCEPT + AFF + r) (Map PackageName Metadata)
readAllMetadataFromDisk metadataDir = do
  let exitMessage = "Could not read metadata for all packages."
  files <- Run.liftAff (Aff.attempt (FS.Aff.readdir metadataDir)) >>= case _ of
    Left err -> do
      Log.error $ "Could not read the metadata directory at path " <> metadataDir <> " due to an fs error: " <> Aff.message err
      Log.exit exitMessage
    Right paths ->
      pure paths

  let
    parsePath path = lmap (Tuple path) do
      base <- note "No .json suffix" $ String.stripSuffix (String.Pattern ".json") path
      name <- PackageName.parse base
      pure name

  let packages = partitionEithers (map parsePath files)
  unless (Array.null packages.fail) do
    Log.error $ Array.fold
      [ "Some entries in the metadata directory are not valid package names:"
      , Array.foldMap (\(Tuple path err) -> "\n  - " <> path <> ": " <> err) packages.fail
      ]
    Log.exit exitMessage

  entries <- Run.liftAff $ map partitionEithers $ for packages.success \name -> do
    result <- readJsonFile Metadata.codec (Path.concat [ metadataDir, PackageName.print name <> ".json" ])
    pure $ map (Tuple name) result

  unless (Array.null entries.fail) do
    Log.error $ append "The metadata directory is invalid (some package metadata cannot be decoded):" $ Array.foldMap (append "\n  - ") entries.fail
    Log.exit exitMessage

  Log.debug "Successfully read metadata entries."
  pure $ Map.fromFoldable entries.success

-- List all package set versions found in the package sets directory by reading
-- each package set filename.
listPackageSetVersions :: forall r. FilePath -> Run (LOG + LOG_EXCEPT + AFF + r) (Array Version)
listPackageSetVersions packageSetsDir = do
  Log.debug "Reading all package set versions..."
  files <- Run.liftAff (Aff.attempt (FS.Aff.readdir packageSetsDir)) >>= case _ of
    Left fsError -> do
      Log.error $ "Failed to read package set directory at path " <> packageSetsDir <> " due to an fs error: " <> Aff.message fsError
      Log.exit "Could not read package sets."
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

data RegistryCache (c :: Type -> Type -> Type) a
  = AllManifests (c ManifestIndex a)
  | AllMetadata (c (Map PackageName Metadata) a)

instance Functor2 c => Functor (RegistryCache c) where
  map k (AllManifests a) = AllManifests (map2 k a)
  map k (AllMetadata a) = AllMetadata (map2 k a)

type REGISTRY_CACHE r = (registryCache :: TypedCache RegistryCache | r)

_registryCache :: Proxy "registryCache"
_registryCache = Proxy

getRegistryCache :: forall r a. CacheKey RegistryCache a -> Run (REGISTRY_CACHE + r) (Maybe a)
getRegistryCache key = Run.lift _registryCache (TypedCache.getCache key)

putRegistryCache :: forall r a. CacheKey RegistryCache a -> a -> Run (REGISTRY_CACHE + r) Unit
putRegistryCache key value = Run.lift _registryCache (TypedCache.putCache key value)

registryMemoryEncoder :: MemoryEncoder RegistryCache
registryMemoryEncoder = case _ of
  AllManifests next -> Exists.mkExists $ Key "ManifestIndex" next
  AllMetadata next -> Exists.mkExists $ Key "AllMetadata" next

runRegistryCacheMemory
  :: forall r a
   . { ref :: CacheRef }
  -> Run (REGISTRY_CACHE + LOG + AFF + EFFECT + r) a
  -> Run (LOG + AFF + EFFECT + r) a
runRegistryCacheMemory { ref } =
  TypedCache.runCacheAt _registryCache (TypedCache.handleCacheMemory { ref, encoder: registryMemoryEncoder })
