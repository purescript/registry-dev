-- | An effect for interacting with registry data, such as metadata, manifests,
-- | and package sets.
module Registry.App.Monad.Registry where

import Registry.App.Prelude

import Control.Monad.Except as Except
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
import Registry.App.Legacy.PackageSet (PscTag(..))
import Registry.App.Legacy.PackageSet as Legacy.Manifest
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Legacy.Types (legacyPackageSetCodec)
import Registry.App.Monad.Cache (class MemoryEncodable, class MonadCache, MemoryEncoding(..))
import Registry.App.Monad.Cache as Cache
import Registry.App.Monad.Git (class MonadGit)
import Registry.App.Monad.Git as Git
import Registry.App.Monad.GitHub (class MonadGitHub)
import Registry.App.Monad.GitHub as GitHub
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
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

data RegistryCache (c :: Type -> Type -> Type) a
  = AllManifests (c ManifestIndex a)
  | AllMetadata (c (Map PackageName Metadata) a)

instance MemoryEncodable RegistryCache where
  encodeMemory = case _ of
    AllManifests k -> Exists.mkExists $ Key "manifest-index" k
    AllMetadata k -> Exists.mkExists $ Key "all-metadata" k

class Monad m <= MonadRegistry m where
  readManifest :: PackageName -> Version -> m (Either String (Maybe Manifest))
  writeManifest :: Manifest -> m (Either String Unit)
  deleteManifest :: PackageName -> Version -> m (Either String Unit)
  readAllManifests :: m (Either String ManifestIndex)
  readMetadata :: PackageName -> m (Either String (Maybe Metadata))
  writeMetadata :: PackageName -> Metadata -> m (Either String Unit)
  readAllMetadata :: m (Either String (Map PackageName Metadata))
  readLatestPackageSet :: m (Either String PackageSet)
  writePackageSet :: PackageSet -> String -> m (Either String Unit)
  readAllPackageSets :: m (Either String (Map Version PackageSet))
  -- legacy operations
  readLegacyRegistry :: m (Either String { bower :: Map String String, new :: Map String String })
  mirrorPackageSet :: PackageSet -> m (Either String Unit)
  mirrorLegacyRegistry :: PackageName -> Location -> m (Either String Unit)

instance MonadRegistry m => MonadRegistry (ExceptT e m) where
  readManifest name = lift <<< readManifest name
  writeManifest = lift <<< writeManifest
  deleteManifest name = lift <<< deleteManifest name
  readAllManifests = lift readAllManifests
  readMetadata = lift <<< readMetadata
  writeMetadata name = lift <<< writeMetadata name
  readAllMetadata = lift readAllMetadata
  readLatestPackageSet = lift readLatestPackageSet
  writePackageSet name = lift <<< writePackageSet name
  readAllPackageSets = lift readAllPackageSets
  readLegacyRegistry = lift readLegacyRegistry
  mirrorPackageSet = lift <<< mirrorPackageSet
  mirrorLegacyRegistry name = lift <<< mirrorLegacyRegistry name

handleReadManifest
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> m (Either String (Maybe Manifest))
handleReadManifest name version = Except.runExceptT do
  let formatted = formatPackageVersion name version
  Log.debug $ "Reading manifest for " <> formatted
  index <- Except.ExceptT handleReadAllManifests
  case ManifestIndex.lookup name version index of
    Nothing -> do
      Log.debug $ "Did not find manifest for " <> formatted <> " in memory cache or local registry repo checkout."
      pure Nothing
    Just manifest -> do
      pure $ Just manifest

handleWriteManifest
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => Manifest
  -> m (Either String Unit)
handleWriteManifest manifest@(Manifest { name, version }) = Except.runExceptT do
  let formatted = formatPackageVersion name version
  Log.info $ "Writing manifest for " <> formatted <> ":\n" <> printJson Manifest.codec manifest
  let exitMessage = "Failed to write manifest for " <> formatted <> " to the manifest index."
  index <- Except.ExceptT handleReadAllManifests
  case ManifestIndex.insert manifest index of
    Left error ->
      Except.throwError $ Array.fold
        [ "Can't insert " <> formatted <> " into manifest index because it has unsatisfied dependencies:"
        , printJson (Internal.Codec.packageMap Range.codec) error
        ]
    Right updated -> do
      result <- Git.writeCommitPush (Git.CommitManifestEntry name) \indexPath -> do
        ManifestIndex.insertIntoEntryFile indexPath manifest >>= case _ of
          Left error -> do
            Log.error $ "Could not insert manifest for " <> formatted <> " into its entry file in WriteManifest: " <> error
            Except.throwError exitMessage
          Right _ -> pure $ Just $ "Update manifest for " <> formatted
      case result of
        Left error ->
          Except.throwError $ "Failed to write and commit manifest: " <> error
        Right Git.NoChange ->
          Log.info "Did not commit manifest because it did not change."
        Right Git.Changed -> do
          Log.info "Wrote and committed manifest."
          Cache.put AllManifests updated

handleDeleteManifest
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> m (Either String Unit)
handleDeleteManifest name version = Except.runExceptT do
  let formatted = formatPackageVersion name version
  Log.info $ "Deleting manifest for " <> formatted
  let exitMessage = "Failed to delete manifest for " <> formatted <> " from the manifest index."
  index <- Except.ExceptT handleReadAllManifests
  case ManifestIndex.delete name version index of
    Left error ->
      Except.throwError $ Array.fold
        [ "Can't delete " <> formatted <> " from manifest index because it would produce unsatisfied dependencies:"
        , printJson (Internal.Codec.packageMap (Internal.Codec.versionMap (Internal.Codec.packageMap Range.codec))) error
        ]
    Right updated -> do
      commitResult <- Git.writeCommitPush (Git.CommitManifestEntry name) \indexPath -> do
        ManifestIndex.removeFromEntryFile indexPath name version >>= case _ of
          Left error -> do
            Log.error $ "Could not remove manifest for " <> formatted <> " from its entry file in DeleteManifest: " <> error
            Except.throwError exitMessage
          Right _ -> pure $ Just $ "Remove manifest entry for " <> formatted
      case commitResult of
        Left error -> Except.throwError $ "Failed to delete and commit manifest: " <> error
        Right Git.NoChange -> Log.info "Did not commit manifest because it already didn't exist."
        Right Git.Changed -> do
          Log.info "Wrote and committed manifest."
          Cache.put AllManifests updated

handleReadAllManifests
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => m (Either String ManifestIndex)
handleReadAllManifests = Except.runExceptT do
  let
    refreshIndex = do
      indexPath <- Git.getPath Git.ManifestIndexRepo
      index <- Except.ExceptT $ readManifestIndexFromDisk indexPath
      Cache.put AllManifests index
      pure index

  Git.pull Git.ManifestIndexRepo >>= case _ of
    Left error ->
      Except.throwError $ "Could not read manifests because the manifest index repo could not be checked: " <> error
    Right Git.NoChange -> do
      Log.debug "Manifest index repo up to date, reading from cache..."
      cache <- Cache.get AllManifests
      case cache of
        Nothing -> do
          Log.info "No cached manifest index, reading from disk..."
          refreshIndex
        Just cached -> pure cached
    Right Git.Changed -> do
      Log.info "Manifest index has changed, replacing cache..."
      refreshIndex

handleReadMetadata
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> m (Either String (Maybe Metadata))
handleReadMetadata name = Except.runExceptT do
  let printedName = PackageName.print name
  Log.debug $ "Reading metadata for " <> printedName
  registryPath <- Git.getPath Git.RegistryRepo

  let
    path = Path.concat [ registryPath, Constants.metadataDirectory, printedName <> ".json" ]

    -- Attempt to read and decode the metadata file from the local checkout.
    readMetadataFromDisk = do
      Log.debug $ "Reading metadata for " <> printedName <> " from disk because it is not available in cache."
      let exitMessage = "Found metadata for " <> printedName <> ", but it could not be read."
      liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
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
            Except.throwError exitMessage
          Right parsed -> case CA.decode Metadata.codec parsed of
            Left decodeError -> do
              Log.error $ Array.fold
                [ "Found metadata file for " <> printedName <> " at path " <> path
                , ", but could not decode the JSON" <> CA.printJsonDecodeError decodeError
                , "\narising from contents:\n" <> contents
                ]
              Except.throwError exitMessage
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
        Cache.put AllMetadata (Map.singleton name metadata)
        pure $ Just metadata

  Git.pull Git.RegistryRepo >>= case _ of
    Left error ->
      Except.throwError $ "Could not read metadata because the registry repo could not be checked: " <> error

    Right Git.NoChange -> do
      Log.debug "Registry repo up to date, reading from cache..."
      Cache.get AllMetadata >>= case _ of
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
                Cache.put AllMetadata (Map.insert name metadata allMetadata)
                pure $ Just metadata

          Just cached ->
            pure $ Just cached

    Right Git.Changed -> do
      Log.info "Registry repo has changed, clearing metadata cache..."
      resetFromDisk

handleWriteMetadata
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Metadata
  -> m (Either String Unit)
handleWriteMetadata name metadata = Except.runExceptT do
  let printedName = PackageName.print name
  Log.info $ "Writing metadata for " <> printedName
  Log.debug $ printJson Metadata.codec metadata
  commitResult <- Git.writeCommitPush (Git.CommitMetadataEntry name) \registryPath -> do
    let path = Path.concat [ registryPath, Constants.metadataDirectory, printedName <> ".json" ]
    let couldNotWriteError = "Could not write metadata for " <> printedName
    liftAff (Aff.attempt (writeJsonFile Metadata.codec path metadata)) >>= case _ of
      Left fsError -> do
        Log.error $ "Failed to write metadata for " <> printedName <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Except.throwError couldNotWriteError
      Right _ -> pure $ Just $ "Update metadata for " <> printedName
  case commitResult of
    Left error -> Except.throwError $ "Failed to write and commit metadata: " <> error
    Right Git.NoChange -> Log.info "Did not commit metadata because it was unchanged."
    Right Git.Changed -> do
      Log.info "Wrote and committed metadata."
      cache <- Cache.get AllMetadata
      for_ cache \cached ->
        Cache.put AllMetadata (Map.insert name metadata cached)

handleReadAllMetadata
  :: forall m
   . MonadCache RegistryCache m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => m (Either String (Map PackageName Metadata))
handleReadAllMetadata = Except.runExceptT do
  let
    refreshMetadata = do
      registryPath <- Git.getPath Git.RegistryRepo
      let metadataDir = Path.concat [ registryPath, Constants.metadataDirectory ]
      Log.info $ "Reading metadata for all packages from directory " <> metadataDir
      allMetadata <- Except.ExceptT $ readAllMetadataFromDisk metadataDir
      Cache.put AllMetadata allMetadata
      pure allMetadata

  Git.pull Git.RegistryRepo >>= case _ of
    Left error ->
      Except.throwError $ "Could not read metadata because the registry repo could not be checked: " <> error
    Right Git.NoChange -> do
      Log.debug "Registry repo up to date, reading from cache..."
      Cache.get AllMetadata >>= case _ of
        Nothing -> do
          Log.info "No cached metadata map, reading from disk..."
          refreshMetadata
        Just cached ->
          pure cached
    Right Git.Changed -> do
      Log.info "Registry repo has changed, replacing metadata cache..."
      refreshMetadata

handleReadLatestPackageSet
  :: forall m
   . MonadGit m
  => MonadLog m
  => MonadAff m
  => m (Either String PackageSet)
handleReadLatestPackageSet = Except.runExceptT do
  registryPath <- Git.getPath Git.RegistryRepo
  let packageSetsDir = Path.concat [ registryPath, Constants.packageSetsDirectory ]
  Log.info $ "Reading latest package set from directory " <> packageSetsDir
  versions <- Except.ExceptT $ listPackageSetVersions packageSetsDir
  case Array.last (Array.sort versions) of
    Nothing -> do
      Log.error $ "No package sets exist in directory " <> packageSetsDir
      Except.throwError "Could not read latest package set."
    Just version -> do
      let printed = Version.print version
      let path = Path.concat [ packageSetsDir, printed <> ".json" ]
      liftAff (readJsonFile PackageSet.codec path) >>= case _ of
        Left error -> do
          Log.error $ "Could not read " <> path <> ": " <> error
          Except.throwError $ "Could not read package set " <> printed
        Right set -> do
          Log.debug $ "Successfully read package set " <> printed
          pure set

handleWritePackageSet
  :: forall m
   . MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageSet
  -> String
  -> m (Either String Unit)
handleWritePackageSet set@(PackageSet { version }) message = Except.runExceptT do
  let name = Version.print version
  Log.info $ "Writing package set " <> name
  commitResult <- Git.writeCommitPush (Git.CommitPackageSet version) \registryPath -> do
    let path = Path.concat [ registryPath, Constants.packageSetsDirectory, name <> ".json" ]
    let couldNotWriteError = "Could not write package set " <> name
    liftAff (Aff.attempt (writeJsonFile PackageSet.codec path set)) >>= case _ of
      Left fsError -> do
        Log.error $ "Failed to write package set " <> name <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Except.throwError couldNotWriteError
      Right _ -> pure $ Just message
  case commitResult of
    Left error -> Except.throwError $ "Failed to write and commit package set: " <> error
    Right Git.NoChange -> Log.info "Did not commit package set because it was unchanged."
    Right Git.Changed -> Log.info "Wrote and committed package set."

handleReadAllPackageSets
  :: forall m
   . MonadGit m
  => MonadLog m
  => MonadAff m
  => m (Either String (Map Version PackageSet))
handleReadAllPackageSets = Except.runExceptT do
  registryPath <- Git.getPath Git.RegistryRepo
  let packageSetsDir = Path.concat [ registryPath, Constants.packageSetsDirectory ]
  Log.info $ "Reading all package sets from directory " <> packageSetsDir
  versions <- Except.ExceptT $ listPackageSetVersions packageSetsDir
  decoded <- for versions \version -> do
    let printed = Version.print version
    let path = Path.concat [ packageSetsDir, printed <> ".json" ]
    map (bimap (Tuple version) (Tuple version)) $ liftAff (readJsonFile PackageSet.codec path)
  let results = partitionEithers decoded
  case results.fail of
    [] -> do
      Log.debug "Successfully read all package sets."
      pure $ Map.fromFoldable results.success
    xs -> do
      let format (Tuple v err) = "\n  - " <> Version.print v <> ": " <> err
      Log.warn $ "Some package sets could not be read and were skipped: " <> Array.foldMap format xs
      pure $ Map.fromFoldable results.success

handleReadLegacyRegistry
  :: forall m
   . MonadGit m
  => MonadLog m
  => MonadAff m
  => m (Either String { bower :: Map String String, new :: Map String String })
handleReadLegacyRegistry = Except.runExceptT do
  registryPath <- Git.getPath Git.RegistryRepo
  Log.info $ "Reading legacy registry from " <> registryPath
  let readRegistryFile path = readJsonFile (CA.Common.strMap CA.string) (Path.concat [ registryPath, path ])
  bower <- liftAff (readRegistryFile "bower-packages.json") >>= case _ of
    Left _ -> Except.throwError "Failed to read bower-packages.json file."
    Right packages -> pure packages
  new <- liftAff (readRegistryFile "new-packages.json") >>= case _ of
    Left _ -> Except.throwError "Failed to read new-packages.json file."
    Right packages -> pure packages
  pure { bower, new }

-- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/release.sh
-- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/update-latest-compatible-sets.sh
handleMirrorPackageSet
  :: forall m
   . MonadCache RegistryCache m
  => MonadGitHub m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageSet
  -> m (Either String Unit)
handleMirrorPackageSet set@(PackageSet { version }) = Except.runExceptT do
  let name = Version.print version
  Log.info $ "Mirroring legacy package set " <> name <> " to the legacy package sets repo"

  manifests <- Except.ExceptT handleReadAllManifests
  metadata <- Except.ExceptT handleReadAllMetadata

  Log.debug $ "Converting package set..."
  converted <- case Legacy.Manifest.convertPackageSet manifests metadata set of
    Left error -> do
      Log.error $ "Failed to convert package set " <> name <> " to a legacy package set: " <> error
      Except.throwError $ "Could not produce a legacy package set from package set " <> name
    Right converted -> pure converted

  let printedTag = Legacy.PackageSet.printPscTag converted.tag
  legacyRepo <- Git.getAddress Git.LegacyPackageSetsRepo

  packageSetsTags <- GitHub.listTags legacyRepo >>= case _ of
    Left githubError -> do
      Log.error $ Array.fold
        [ "Fetching tags for the repo " <> legacyRepo.owner <> "/" <> legacyRepo.repo
        , " failed: " <> Octokit.printGitHubError githubError
        ]
      Except.throwError "Could not mirror package set because we were unable to fetch tags from the legacy package-sets repo."
    Right tags -> pure $ Set.fromFoldable $ map _.name tags

  when (Set.member printedTag packageSetsTags) do
    Log.error $ printedTag <> " already exists."
    Except.throwError $ "Could not mirror package set because the tag " <> printedTag <> " already exists."

  -- stores a mapping of compiler versions to their highest compatible tag
  let latestSetsPath = "latest-compatible-sets.json"
  -- stores the JSON representation of the latest package set
  let packagesJsonPath = "packages.json"
  -- stores the Dhall representation of the latest package set
  let dhallPath = Path.concat [ "src", "packages.dhall" ]

  let files = [ latestSetsPath, packagesJsonPath, dhallPath ]
  let compilerKey = (un PscTag converted.tag).compiler

  commitFilesResult <- Git.writeCommitPush (Git.CommitLegacyPackageSets files) \legacyPath -> do
    latestCompatibleSets <- do
      latestSets <- liftAff (readJsonFile Legacy.PackageSet.latestCompatibleSetsCodec latestSetsPath) >>= case _ of
        Left err -> do
          Log.error $ "Could not read latest-compatible-sets from " <> latestSetsPath <> " due to an error: " <> err
          Except.throwError $ "Could not mirror package set because we could not read the latest-compatible-sets.json file."
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
    liftAff $ FS.Aff.writeTextFile UTF8 fullDhallPath (Legacy.PackageSet.printDhall converted.packageSet)

    Log.debug $ "Writing " <> packagesJsonPath
    let fullPackagesJsonPath = Path.concat [ legacyPath, packagesJsonPath ]
    liftAff $ writeJsonFile legacyPackageSetCodec fullPackagesJsonPath converted.packageSet

    Log.debug $ "Writing " <> latestSetsPath
    let fullLatestSetsPath = Path.concat [ legacyPath, latestSetsPath ]
    liftAff $ writeJsonFile Legacy.PackageSet.latestCompatibleSetsCodec fullLatestSetsPath latestCompatibleSets

    pure $ Just $ "Update to the " <> name <> " package set."

  case commitFilesResult of
    Left error -> Except.throwError $ "Failed to commit to legacy registry:" <> error
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
          Except.throwError $ "Failed to push tags to legacy registry: " <> error
        Right Git.NoChange ->
          Log.warn $ "Tried to push tags to legacy registry, but there was no effect (they already existed)."
        Right Git.Changed ->
          Log.info "Pushed new tags to legacy registry."

handleMirrorLegacyRegistry
  :: forall m
   . MonadCache RegistryCache m
  => MonadGitHub m
  => MonadGit m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Location
  -> m (Either String Unit)
handleMirrorLegacyRegistry name location = Except.runExceptT do
  Log.debug $ "Mirroring package " <> PackageName.print name <> " to location " <> stringifyJson Location.codec location
  url <- case location of
    GitHub { owner, repo, subdir: Nothing } ->
      pure $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
    GitHub { owner, repo, subdir: Just dir } -> do
      Log.error $ "Cannot mirror location " <> owner <> "/" <> repo <> " because it specifies a subdirectory (" <> dir <> ")."
      Except.throwError "Could not sync package with the legacy registry because it uses a 'subdir' key."
    Git { url } -> do
      Log.error $ "Cannot mirror location " <> url <> " because it is a Git location, and only GitHub is supported in the legacy registry."
      Except.throwError "Could not sync package with the legacy registry because only GitHub packages are supported."

  { bower, new } <- Except.ExceptT handleReadLegacyRegistry

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
      liftAff $ writeJsonFile (CA.Common.strMap CA.string) path packages
    pure $ Just $ "Sync " <> PackageName.print name <> " with legacy registry."

  case result of
    Left error ->
      Except.throwError $ "Failed to commit and push legacy registry files: " <> error
    Right Git.NoChange ->
      Log.info $ "Did not commit and push legacy registry files because there was no change."
    Right Git.Changed ->
      Log.info "Wrote and committed legacy registry files."

-- | Given the file path of a local manifest index on disk, read its contents.
readManifestIndexFromDisk
  :: forall m
   . MonadLog m
  => MonadAff m
  => FilePath
  -> m (Either String ManifestIndex)
readManifestIndexFromDisk root = Except.runExceptT do
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
        Except.throwError "Failed to read the manifest index."

      Right index -> do
        Log.debug "Successfully read manifest index."
        pure index

    failed -> do
      Log.error $ append "Unable to read manifest index (some package entries cannot be decoded): " $ Array.foldMap (append "\n  - ") failed
      Except.throwError "Failed to read the manifest index."

-- | Given the file path of a directory of metadata on disk, read its contents.
readAllMetadataFromDisk
  :: forall m
   . MonadLog m
  => MonadAff m
  => FilePath
  -> m (Either String (Map PackageName Metadata))
readAllMetadataFromDisk metadataDir = Except.runExceptT do
  let exitMessage = "Could not read metadata for all packages."
  files <- liftAff (Aff.attempt (FS.Aff.readdir metadataDir)) >>= case _ of
    Left err -> do
      Log.error $ "Could not read the metadata directory at path " <> metadataDir <> " due to an fs error: " <> Aff.message err
      Except.throwError exitMessage
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
    Except.throwError exitMessage

  entries <- liftAff $ map partitionEithers $ for packages.success \name -> do
    result <- readJsonFile Metadata.codec (Path.concat [ metadataDir, PackageName.print name <> ".json" ])
    pure $ map (Tuple name) result

  unless (Array.null entries.fail) do
    Log.error $ append "The metadata directory is invalid (some package metadata cannot be decoded):" $ Array.foldMap (append "\n  - ") entries.fail
    Except.throwError exitMessage

  Log.debug "Successfully read metadata entries."
  pure $ Map.fromFoldable entries.success

-- List all package set versions found in the package sets directory by reading
-- each package set filename.
listPackageSetVersions
  :: forall m
   . MonadLog m
  => MonadAff m
  => FilePath
  -> m (Either String (Array Version))
listPackageSetVersions packageSetsDir = Except.runExceptT do
  Log.debug "Reading all package set versions..."
  files <- liftAff (Aff.attempt (FS.Aff.readdir packageSetsDir)) >>= case _ of
    Left fsError -> do
      Log.error $ "Failed to read package set directory at path " <> packageSetsDir <> " due to an fs error: " <> Aff.message fsError
      Except.throwError "Could not read package sets."
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
