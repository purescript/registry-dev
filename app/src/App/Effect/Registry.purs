-- | An effect for interacting with registry data, such as metadata, manifests,
-- | and package sets. The default handler uses local checkouts of the Git
-- | repositories for each and interactions are done on the file system.
module Registry.App.Effect.Registry where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Parallel as Parallel
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Exists as Exists
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration as Duration
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Ref as Ref
import JSON as JSON
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Git (GitResult)
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache (class MemoryEncodable, Cache, CacheRef, MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, Log)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.PackageSet (PscTag(..))
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Legacy.Types (legacyPackageSetCodec)
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (Address)
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
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

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

-- | A legend for repositories that can be fetched and committed to.
data RepoKey
  = RegistryRepo
  | ManifestIndexRepo
  | LegacyPackageSetsRepo

derive instance Eq RepoKey
derive instance Ord RepoKey

-- | Identifies which process is using the registry, for lock ownership tracking.
data Process
  = Scheduler
  | JobExecutor
  | API
  | ScriptLegacyImporter
  | ScriptPackageDeleter
  | ScriptSolver
  | ScriptVerifyIntegrity
  | ScriptCompilerVersions
  | ScriptArchiveSeeder

derive instance Eq Process

printProcess :: Process -> String
printProcess = case _ of
  Scheduler -> "Scheduler"
  JobExecutor -> "JobExecutor"
  API -> "API"
  ScriptLegacyImporter -> "ScriptLegacyImporter"
  ScriptPackageDeleter -> "ScriptPackageDeleter"
  ScriptSolver -> "ScriptSolver"
  ScriptVerifyIntegrity -> "ScriptVerifyIntegrity"
  ScriptCompilerVersions -> "ScriptCompilerVersions"
  ScriptArchiveSeeder -> "ScriptArchiveSeeder"

-- | A lock for a single repository, tracking both the mutex and the owner.
type RepoLock = { lock :: AVar Unit, owner :: Ref (Maybe Process) }

-- | Per-repository locks to prevent concurrent access.
type RepoLocks = Ref (Map RepoKey RepoLock)

-- | Create a new empty set of repo locks.
newRepoLocks :: forall m. MonadEffect m => m RepoLocks
newRepoLocks = liftEffect $ Ref.new Map.empty

-- | Get or create a lock for a repository.
getOrCreateLock :: RepoLocks -> RepoKey -> Aff RepoLock
getOrCreateLock locksRef key = do
  locks <- liftEffect $ Ref.read locksRef
  case Map.lookup key locks of
    Just lock -> pure lock
    Nothing -> do
      lock <- AVar.new unit
      owner <- liftEffect $ Ref.new Nothing
      let repoLock = { lock, owner }
      liftEffect $ Ref.modify_ (Map.insert key repoLock) locksRef
      pure repoLock

-- | Acquire a repository lock, run an action, and release the lock.
-- | The lock prevents concurrent access to the same repository.
-- | Defaults to a 60-second timeout.
withRepoLock
  :: forall r a
   . Process
  -> RepoLocks
  -> RepoKey
  -> Run (LOG + AFF + EFFECT + EXCEPT String + r) a
  -> Run (LOG + AFF + EFFECT + EXCEPT String + r) a
withRepoLock = withRepoLockTimeout (Milliseconds 60_000.0)

-- | Acquire a repository lock, run an action, and release the lock.
-- | The lock prevents concurrent access to the same repository.
withRepoLockTimeout
  :: forall r a
   . Milliseconds
  -> Process
  -> RepoLocks
  -> RepoKey
  -> Run (LOG + AFF + EFFECT + EXCEPT String + r) a
  -> Run (LOG + AFF + EFFECT + EXCEPT String + r) a
withRepoLockTimeout timeout process locks key action = do
  repoLock <- Run.liftAff $ getOrCreateLock locks key

  -- It isn't possible to run exception-safe Aff code like `bracket` within
  -- the extensible effects system. For the actions we need to support
  -- behind a lock we only need to support the LOG effect, so we lower to
  -- Aff, run the lock-guarded code safely, and aggregate the logs to be
  -- flushed afterwards.
  { logs, outcome } <- Run.liftAff do
    logsRef <- liftEffect $ Ref.new []
    outcome <- withRepoLockAff repoLock timeout (runWithLogs logsRef action)
    logs <- liftEffect $ Ref.read logsRef
    pure { logs, outcome }

  -- We replay the collected logs
  for_ logs \log ->
    Run.lift Log._log log

  case outcome of
    Nothing -> do
      Log.warn $ "Repo lock timed out for " <> printProcess process
      Run.liftAff $ Aff.throwError $ Aff.error "Repo lock timed out."
    Just (Left err) ->
      Run.liftAff $ Aff.throwError err
    Just (Right value) ->
      pure value
  where
  runWithLogs :: Ref (Array (Log Unit)) -> Run (LOG + AFF + EFFECT + EXCEPT String + r) a -> Aff (Either Aff.Error a)
  runWithLogs ref = Aff.attempt <<< Run.runCont step pure
    where
    step =
      Run.on Log._log handleLog
        $ Run.on (Proxy @"aff") (\k -> k >>= identity)
        $ Run.on (Proxy @"effect") (\k -> liftEffect k >>= identity)
        $ Run.on Except._except (\k -> Aff.throwError (Aff.error (coerce k)))
        $ Run.default (Aff.throwError $ Aff.error "withRepoLock: unexpected effect")

    handleLog (Log.Log level message next) = do
      liftEffect $ Ref.modify_ (\logs -> Array.snoc logs (Log.Log level message unit)) ref
      next

  -- | Acquire a lock, run the action, and release the lock, guarded by a bracket to clean the
  -- | locks on exception. Action is cancelled after a configurable timeout
  withRepoLockAff :: RepoLock -> Milliseconds -> Aff (Either Aff.Error a) -> Aff (Maybe (Either Aff.Error a))
  withRepoLockAff repoLock lockTimeout aff =
    Aff.bracket acquire release \_ -> do
      let race = Parallel.parallel (Just <$> aff) <|> Parallel.parallel (Aff.delay lockTimeout $> Nothing)
      Parallel.sequential race
    where
    acquire = do
      AVar.take repoLock.lock
      liftEffect $ Ref.write (Just process) repoLock.owner

    release _ = do
      liftEffect $ Ref.write Nothing repoLock.owner
      AVar.put unit repoLock.lock

-- | Validate that a repository is in a valid state.
-- | If the repo is corrupted (e.g., from an interrupted clone), delete it.
validateRepo :: forall r. FilePath -> Run (LOG + AFF + EFFECT + r) Unit
validateRepo path = do
  exists <- Run.liftAff $ Aff.attempt (FS.Aff.stat path)
  case exists of
    Left _ -> pure unit -- Doesn't exist, nothing to validate
    Right _ -> do
      result <- Run.liftAff $ Git.gitCLI [ "rev-parse", "--git-dir" ] (Just path)
      case result of
        Left _ -> do
          Log.warn $ "Detected corrupted repo at " <> path <> ", deleting"
          Run.liftAff $ FS.Extra.remove path
        Right _ -> pure unit

-- | A legend for values that can be committed. We know where each kind of value
-- | ought to exist, so we can create a correct path for any given type ourselves.
data CommitKey
  = CommitManifestEntry PackageName
  | CommitMetadataEntry PackageName
  | CommitManifestIndex
  | CommitMetadataIndex
  | CommitPackageSet Version
  | CommitLegacyRegistry
  | CommitLegacyPackageSets (Array FilePath)

-- | Get the pattern representing the paths that should be committed for each
-- | commit key, relative to the root of the repository. Suitable to be passed
-- | to a git 'add' command executed in the checkout.
commitKeyToPaths :: CommitKey -> Array String.Pattern
commitKeyToPaths = coerce <<< case _ of
  CommitManifestEntry name ->
    [ ManifestIndex.packageEntryFilePath name ]
  CommitMetadataEntry name ->
    [ Path.concat [ Constants.metadataDirectory, PackageName.print name <> ".json" ] ]
  CommitManifestIndex ->
    [ "." ]
  CommitMetadataIndex ->
    [ Constants.metadataDirectory <> Path.sep <> "*.json" ]
  CommitPackageSet version ->
    [ Path.concat [ Constants.packageSetsDirectory, Version.print version <> ".json" ] ]
  CommitLegacyRegistry ->
    [ "bower-packages.json", "new-packages.json" ]
  CommitLegacyPackageSets paths ->
    paths

commitKeyToRepoKey :: CommitKey -> RepoKey
commitKeyToRepoKey = case _ of
  CommitManifestEntry _ -> ManifestIndexRepo
  CommitMetadataEntry _ -> RegistryRepo
  CommitManifestIndex -> ManifestIndexRepo
  CommitMetadataIndex -> RegistryRepo
  CommitPackageSet _ -> RegistryRepo
  CommitLegacyRegistry -> RegistryRepo
  CommitLegacyPackageSets _ -> LegacyPackageSetsRepo

data WriteMode = ReadOnly | CommitAs Git.Committer

derive instance Eq WriteMode

type RegistryEnv =
  { repos :: Repos
  , workdir :: FilePath
  , pull :: Git.PullMode
  , write :: WriteMode
  , debouncer :: Debouncer
  , cacheRef :: CacheRef
  , repoLocks :: RepoLocks
  , process :: Process
  }

type Debouncer = Ref (Map FilePath DateTime)

newDebouncer :: forall m. MonadEffect m => m Debouncer
newDebouncer = liftEffect $ Ref.new Map.empty

type Repos =
  { registry :: Address
  , manifestIndex :: Address
  , legacyPackageSets :: Address
  }

-- | The default repos to use with the Registry effect handler
defaultRepos :: Repos
defaultRepos =
  { registry: Constants.registry
  , manifestIndex: Constants.manifestIndex
  , legacyPackageSets: Legacy.PackageSet.legacyPackageSetsRepo
  }

-- | Handle the REGISTRY effect by downloading the registry and registry-index
-- | repositories locally and reading and writing their contents from disk.
-- | Writes can optionally commit and push to the upstream Git repository.
-- |
-- | This handler enforces a memory-only cache: we do not want to cache on the
-- | file system or other storage because this handler relies on the registry
-- | Git repositories instead.
handle :: forall r a. RegistryEnv -> Registry a -> Run (GITHUB + LOG + AFF + EFFECT + r) a
handle env = Cache.interpret _registryCache (Cache.handleMemory env.cacheRef) <<< case _ of
  ReadManifest name version reply -> do
    let formatted = formatPackageVersion name version
    handle env (ReadAllManifests identity) >>= case _ of
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
    index <- Except.rethrow =<< handle env (ReadAllManifests identity)
    case ManifestIndex.insert ManifestIndex.ConsiderRanges manifest index of
      Left error ->
        Except.throw $ Array.fold
          [ "Can't insert " <> formatted <> " into manifest index because it has unsatisfied dependencies:"
          , printJson (Internal.Codec.packageMap Range.codec) error
          ]
      Right updated -> do
        result <- writeCommitPush (CommitManifestEntry name) \indexPath -> do
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
    index <- Except.rethrow =<< handle env (ReadAllManifests identity)
    case ManifestIndex.delete ManifestIndex.ConsiderRanges name version index of
      Left error ->
        Except.throw $ Array.fold
          [ "Can't delete " <> formatted <> " from manifest index because it would produce unsatisfied dependencies:"
          , printJson (Internal.Codec.packageMap (Internal.Codec.versionMap (Internal.Codec.packageMap Range.codec))) error
          ]
      Right updated -> do
        commitResult <- writeCommitPush (CommitManifestEntry name) \indexPath -> do
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
        let indexPath = repoPath ManifestIndexRepo
        index <- readManifestIndexFromDisk indexPath
        Cache.put _registryCache AllManifests index
        pure index

    pull ManifestIndexRepo >>= case _ of
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
    let dir = repoPath RegistryRepo

    let
      path = Path.concat [ dir, Constants.metadataDirectory, printedName <> ".json" ]

      -- Attempt to read and decode the metadata file from the local checkout.
      readMetadataFromDisk = do
        Log.debug $ "Reading metadata for " <> printedName <> " from disk because it is not available in cache."
        Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
          Left fsError -> do
            Log.debug $ "Could not find metadata file for package " <> printedName <> ": " <> Aff.message fsError
            pure Nothing
          Right contents -> case JSON.parse contents of
            Left jsonError ->
              Except.throw $ Array.fold
                [ "Found metadata file for " <> printedName <> " at path " <> path
                , ", but the file is not valid JSON: " <> jsonError
                , "\narising from contents:\n" <> contents
                ]
            Right parsed -> case CJ.decode Metadata.codec parsed of
              Left decodeError -> do
                Except.throw $ Array.fold
                  [ "Found metadata file for " <> printedName <> " at path " <> path
                  , ", but could not decode the JSON" <> CJ.DecodeError.print decodeError
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
          Log.debug $ "Setting metadata cache to singleton entry (as cache was previously empty)."
          Cache.put _registryCache AllMetadata (Map.singleton name metadata)
          pure $ Just metadata

    pull RegistryRepo >>= case _ of
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
        Cache.delete _registryCache AllMetadata
        resetFromDisk

  WriteMetadata name metadata reply -> map (map reply) Except.runExcept do
    let printedName = PackageName.print name
    Log.info $ "Writing metadata for " <> printedName
    Log.debug $ printJson Metadata.codec metadata
    commitResult <- writeCommitPush (CommitMetadataEntry name) \dir -> do
      let path = Path.concat [ dir, Constants.metadataDirectory, printedName <> ".json" ]
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
        let dir = repoPath RegistryRepo
        let metadataDir = Path.concat [ dir, Constants.metadataDirectory ]
        Log.info $ "Reading metadata for all packages from directory " <> metadataDir
        allMetadata <- readAllMetadataFromDisk metadataDir
        Cache.put _registryCache AllMetadata allMetadata
        pure allMetadata

    pull RegistryRepo >>= case _ of
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
    pull RegistryRepo >>= case _ of
      Left error -> Except.throw $ "Could not read package sets because the registry repo could not be checked: " <> error
      Right _ -> pure unit
    let dir = repoPath RegistryRepo
    let packageSetsDir = Path.concat [ dir, Constants.packageSetsDirectory ]
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
    pull RegistryRepo >>= case _ of
      Left error -> Except.throw $ "Could not read package sets because the registry repo could not be checked: " <> error
      Right _ -> pure unit
    let name = Version.print version
    Log.info $ "Writing package set " <> name
    commitResult <- writeCommitPush (CommitPackageSet version) \dir -> do
      let path = Path.concat [ dir, Constants.packageSetsDirectory, name <> ".json" ]
      Run.liftAff (Aff.attempt (writeJsonFile PackageSet.codec path set)) >>= case _ of
        Left fsError -> Except.throw $ "Failed to write package set " <> name <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Right _ -> pure $ Just message
    case commitResult of
      Left error -> Except.throw $ "Failed to write and commit package set: " <> error
      Right Git.NoChange -> Log.info "Did not commit package set because it was unchanged."
      Right Git.Changed -> Log.info "Wrote and committed package set."

  ReadAllPackageSets reply -> map (map reply) Except.runExcept do
    pull RegistryRepo >>= case _ of
      Left error -> Except.throw $ "Could not read package sets because the registry repo could not be checked: " <> error
      Right _ -> pure unit
    let dir = repoPath RegistryRepo
    let packageSetsDir = Path.concat [ dir, Constants.packageSetsDirectory ]
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

    manifests <- Except.rethrow =<< handle env (ReadAllManifests identity)

    Log.debug $ "Converting package set..."
    converted <- case Legacy.PackageSet.convertPackageSet manifests set of
      Left error -> Except.throw $ "Failed to convert package set " <> name <> " to a legacy package set: " <> error
      Right converted -> pure converted

    let printedTag = Legacy.PackageSet.printPscTag converted.tag
    let legacyRepo = repoAddress LegacyPackageSetsRepo

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

    commitFilesResult <- writeCommitPush (CommitLegacyPackageSets files) \legacyPath -> do
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
        tagResult <- tagAndPush LegacyPackageSetsRepo do
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
    let dir = repoPath RegistryRepo
    Log.info $ "Reading legacy registry from " <> dir
    let readRegistryFile path = readJsonFile (CJ.Common.strMap CJ.string) (Path.concat [ dir, path ])
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

    { bower, new } <- Except.rethrow =<< handle env (ReadLegacyRegistry identity)

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

    result <- writeCommitPush CommitLegacyRegistry \dir -> do
      for_ targetFile \file -> do
        let sourcePackages = if file == "new-packages.json" then new else bower
        let packages = Map.insert rawPackageName url sourcePackages
        let path = Path.concat [ dir, file ]
        Run.liftAff $ writeJsonFile (CJ.Common.strMap CJ.string) path packages
      pure $ Just $ "Sync " <> PackageName.print name <> " with legacy registry."

    case result of
      Left error ->
        Except.throw $ "Failed to commit and push legacy registry files: " <> error
      Right Git.NoChange ->
        Log.info $ "Did not commit and push legacy registry files because there was no change."
      Right Git.Changed ->
        Log.info "Wrote and committed legacy registry files."
  where
  -- | Get the upstream address associated with a repository key
  repoAddress :: RepoKey -> Address
  repoAddress = case _ of
    RegistryRepo -> env.repos.registry
    ManifestIndexRepo -> env.repos.manifestIndex
    LegacyPackageSetsRepo -> env.repos.legacyPackageSets

  -- | Get local filepath for the checkout associated with a repository key
  repoPath :: RepoKey -> FilePath
  repoPath = case _ of
    RegistryRepo -> Path.concat [ env.workdir, "registry" ]
    ManifestIndexRepo -> Path.concat [ env.workdir, "registry-index" ]
    LegacyPackageSetsRepo -> Path.concat [ env.workdir, "package-sets" ]

  -- | Write a file to the repository associated with the commit key, given a
  -- | callback that takes the file path of the repository on disk, writes the
  -- | file(s), and returns a commit message which is used to commit to the
  -- | repository. The result is pushed upstream.
  writeCommitPush :: CommitKey -> (FilePath -> Run _ (Maybe String)) -> Run _ (Either String GitResult)
  writeCommitPush commitKey write = do
    let repoKey = commitKeyToRepoKey commitKey
    pull repoKey >>= case _ of
      Left error -> pure (Left error)
      Right _ -> do
        let path = repoPath repoKey
        write path >>= case _ of
          Nothing -> pure $ Left $ "Failed to write file(s) to " <> path
          Just message -> commit commitKey message >>= case _ of
            Left error -> pure (Left error)
            Right _ -> push repoKey

  -- | Tag the repository with the given tags and push the result upstream.
  tagAndPush :: RepoKey -> Array String -> Run _ (Either String GitResult)
  tagAndPush key refs = do
    results <- traverse (tag key) refs
    let partition = partitionEithers results
    case Array.uncons partition.fail of
      Nothing | Array.any (_ == Git.Changed) partition.success -> pushTags key
      Nothing -> pure (Right Git.NoChange)
      Just { head } -> pure (Left head)

  -- | Get the repository at the given key, recording whether the pull or clone
  -- | had any effect (ie. if the repo was already up-to-date).
  -- | Uses per-repository locking to prevent race conditions during clone.
  pull :: RepoKey -> Run _ (Either String GitResult)
  pull repoKey = withRepoLock env.process env.repoLocks repoKey do
    let
      path = repoPath repoKey
      address = repoAddress repoKey
      fetchLatest = do
        Log.debug $ "Fetching repo at path " <> path
        -- First, we need to verify whether we should clone or pull.
        Run.liftAff (Aff.attempt (FS.Aff.stat path)) >>= case _ of
          -- When the repository doesn't exist at the given file path we can go
          -- straight to a clone.
          Left _ -> do
            let formatted = address.owner <> "/" <> address.repo
            let url = "https://github.com/" <> formatted <> ".git"
            Log.debug $ "Didn't find " <> formatted <> " locally, cloning..."
            Run.liftAff (Git.gitCLI [ "clone", url, path ] Nothing) >>= case _ of
              Left err -> do
                Log.error $ "Failed to git clone repo " <> url <> " due to a git error: " <> err
                pure $ Left $ "Could not read the repository at " <> formatted
              Right _ ->
                pure $ Right Git.Changed

          -- If it does, then we should pull.
          Right _ -> do
            Log.debug $ "Found repo at path " <> path <> ", pulling latest."
            result <- Git.gitPull { address, pullMode: env.pull } path
            pure result

    -- Check if the repo directory exists before consulting the debouncer.
    -- This ensures that if the scratch directory is deleted (e.g., for test
    -- isolation), we always re-clone rather than returning a stale NoChange.
    repoExists <- Run.liftAff $ Aff.attempt (FS.Aff.stat path)
    case repoExists of
      Left _ -> do
        -- Repo doesn't exist, bypass debouncer entirely and clone fresh
        result <- fetchLatest
        now <- nowUTC
        Run.liftEffect $ Ref.modify_ (Map.insert path now) env.debouncer
        pure result
      Right _ -> do
        -- Repo exists, check debouncer
        now <- nowUTC
        debouncers <- Run.liftEffect $ Ref.read env.debouncer
        case Map.lookup path debouncers of
          -- We will be behind the upstream by at most this amount of time.
          Just prev | DateTime.diff now prev <= Duration.Minutes 1.0 ->
            pure $ Right Git.NoChange
          -- If we didn't debounce, then we should fetch the upstream.
          _ -> do
            result <- fetchLatest
            Run.liftEffect $ Ref.modify_ (Map.insert path now) env.debouncer
            pure result

  -- | Commit the file(s) indicated by the commit key with a commit message.
  commit :: CommitKey -> String -> Run _ (Either String GitResult)
  commit commitKey message = do
    let repoKey = commitKeyToRepoKey commitKey
    let address = repoAddress repoKey
    let formatted = address.owner <> "/" <> address.repo
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping commit to repo " <> formatted <> " because write mode is 'ReadOnly'."
        pure $ Right Git.NoChange
      CommitAs committer -> do
        result <- Git.gitCommit { committer, address, commit: commitKeyToPaths commitKey, message } (repoPath repoKey)
        pure result

  -- | Push the repository at the given key, recording whether the push had any
  -- | effect (ie. if the repo was already up-to-date).
  push :: RepoKey -> Run _ (Either String GitResult)
  push repoKey = do
    let address = repoAddress repoKey
    let formatted = address.owner <> "/" <> address.repo
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping push to repo " <> formatted <> " because write mode is 'ReadOnly'."
        pure $ Right Git.NoChange
      CommitAs committer -> do
        result <- Git.gitPush { address, committer } (repoPath repoKey)
        pure result

  -- | Tag the repository at the given key at its current commit with the tag
  tag :: RepoKey -> String -> Run _ (Either String GitResult)
  tag repoKey ref = do
    let address = repoAddress repoKey
    let formatted = address.owner <> "/" <> address.repo
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping push to repo " <> formatted <> " because write mode is 'ReadOnly'."
        pure $ Right Git.NoChange
      CommitAs committer -> do
        result <- Except.runExcept do
          let cwd = repoPath repoKey
          existingTags <- Git.withGit cwd [ "tag", "--list" ] \error ->
            "Failed to list tags in local checkout " <> cwd <> ": " <> error
          if Array.elem ref $ String.split (String.Pattern "\n") existingTags then do
            Log.warn $ "Tag " <> ref <> " already exists."
            pure Git.NoChange
          else do
            _ <- Git.withGit cwd [ "config", "user.name", committer.name ] \error ->
              "Failed to configure git user name as " <> committer.name <> " in " <> cwd <> ": " <> error
            _ <- Git.withGit cwd [ "config", "user.email", "<" <> committer.email <> ">" ] \error ->
              "Failed to configure git user email as " <> committer.email <> " in " <> cwd <> ": " <> error
            _ <- Git.withGit cwd [ "tag", ref ] \error ->
              "Failed to create new tag " <> ref <> " in local checkout " <> cwd <> ": " <> error
            pure Git.Changed
        pure result

  -- | Push the repository tags at the given key to its upstream.
  pushTags :: RepoKey -> Run _ (Either String GitResult)
  pushTags repoKey = do
    let address = repoAddress repoKey
    let formatted = address.owner <> "/" <> address.repo
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping push to repo " <> formatted <> " because write mode is 'ReadOnly'."
        pure $ Right Git.NoChange
      CommitAs committer -> do
        result <- Except.runExcept do
          let cwd = repoPath repoKey
          let Git.AuthOrigin authOrigin = Git.mkAuthOrigin address committer
          output <- Git.withGit cwd [ "push", "--tags", authOrigin ] \error ->
            "Failed to push tags in local checkout " <> cwd <> ": " <> error
          if String.contains (String.Pattern "Everything up-to-date") output then pure Git.NoChange else pure Git.Changed
        pure result

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
    [] -> case ManifestIndex.fromSet ManifestIndex.ConsiderRanges $ Set.fromFoldable $ Array.foldMap NonEmptyArray.toArray entries.success of
      Left errors -> do
        Log.debug $ "Could not read a valid manifest index from entry files: " <> Array.foldMap (Array.foldMap (\(Manifest { name, version }) -> "\n  - " <> formatPackageVersion name version) <<< NonEmptyArray.toArray) entries.success
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
    pure $ bimap (Tuple name) (Tuple name) result

  unless (Array.null entries.fail) do
    Except.throw $ append "Could not read metadata for all packages because the metadata directory is invalid (some package metadata cannot be decoded):" $ Array.foldMap (\(Tuple name err) -> "\n  - " <> PackageName.print name <> ": " <> err) entries.fail

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
