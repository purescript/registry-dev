-- | An effect for interacting with registry data, such as metadata, manifests,
-- | and package sets.
module Registry.App.Effect.Registry where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.DateTime (DateTime, diff)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.CLI.Git (CommitMode(..))
import Registry.App.CLI.Git as Git
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.PackageSet (PscTag(..))
import Registry.App.Legacy.PackageSet as Legacy.Manifest
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Legacy.Types (legacyPackageSetCodec)
import Registry.Constants as Constants
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (Address, GitHubToken(..))
import Registry.Foreign.Octokit as Octokit
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

-- | How to persist on write. Write will persist the change, but will not commit
-- | the data. CommitNoPush will write and commit the data, but not push the
-- | result to an upstream repository. CommitPush will write the data, commit,
-- | and push the result.
data WriteStrategy = Write | WriteCommit GitHubToken | WriteCommitPush GitHubToken

derive instance Eq WriteStrategy

type RegistryEnv =
  { registry :: FilePath
  , registryIndex :: FilePath
  , legacyPackageSets :: FilePath
  , writeStrategy :: WriteStrategy
  , pullMode :: PullMode
  -- TODO: Replace with a time-limited cache.
  , timer :: Ref (Maybe DateTime)
  }

-- | Handle the REGISTRY effect by downloading the registry and registry-index
-- | repositories locally and reading and writing their contents from disk.
-- | Writes can optionally commit and push to the uptsream Git repository. This
-- | implementation uses an in-memory, short-duration cache to avoid excessive
-- | reads from disk.
--
-- TODO: Implement caching!
handleRegistryGit :: forall r a. RegistryEnv -> Registry a -> Run (GITHUB + LOG + LOG_EXCEPT + AFF + EFFECT + r) a
handleRegistryGit env = case _ of
  ReadManifest name version reply -> do
    let formatted = formatPackageVersion name version
    Log.info $ "Reading manifest for " <> formatted <> "..."
    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.manifestIndex env.pullMode env.registryIndex
    Run.liftAff (ManifestIndex.readEntryFile env.registryIndex name) >>= case _ of
      Left error -> do
        Log.warn $ "Could not read manifest index entry for package " <> PackageName.print name <> ": " <> error
        pure $ reply Nothing
      Right entries -> case NonEmptyArray.find (\(Manifest m) -> m.name == name && m.version == version) entries of
        Nothing -> do
          Log.warn $ "Found manifest index entry for package " <> PackageName.print name <> " but none for version " <> Version.print version
          pure $ reply Nothing
        Just entry -> do
          Log.debug $ "Found manifest index entry for " <> formatted
          pure $ reply $ Just entry

  WriteManifest manifest@(Manifest { name, version }) next -> do
    let formatted = formatPackageVersion name version
    Log.info $ "Writing manifest for " <> formatted <> ":\n" <> printJson Manifest.codec manifest
    fetchGitHubRepo Constants.manifestIndex env.pullMode env.registryIndex
    let exitMessage = "Failed to write manifest for " <> formatted <> " to the manifest index."
    Run.liftAff (ManifestIndex.insertIntoEntryFile env.registryIndex manifest) >>= case _ of
      Left error -> do
        Log.error $ "Could not insert manifest for " <> formatted <> " into its entry file in WriteManifest: " <> error
        Log.exit exitMessage
      Right _ -> do
        let message = "Update manifest for " <> formatted
        let path = Path.concat [ env.registryIndex, ManifestIndex.packageEntryFilePath name ]
        let commitArgs token mode = { token, paths: String.Pattern path, message, mode }
        case env.writeStrategy of
          Write -> do
            Log.info $ "Wrote manifest for " <> formatted <> " but not committing because write strategy is Write."
            pure next
          WriteCommit token ->
            Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistryIndex env.registryIndex (commitArgs token CommitOnly))) >>= case _ of
              Left error -> do
                Log.error $ "Could not commit manifest in WriteCommit for " <> formatted <> " due to a Git error: " <> error
                Log.exit exitMessage
              Right _ -> do
                Log.debug $ "Successfully committed manifest for " <> formatted <> " (did not push)"
                pure next
          WriteCommitPush token ->
            Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistryIndex env.registryIndex (commitArgs token CommitAndPush))) >>= case _ of
              Left error -> do
                Log.error $ "Could not commit and push manifest in WriteCommitPush for " <> formatted <> " due to a Git error: " <> error
                Log.exit exitMessage
              Right _ -> do
                Log.debug $ "Successfully committed and pushed manifest for " <> formatted
                pure next

  DeleteManifest name version next -> do
    let formatted = formatPackageVersion name version
    Log.info $ "Deleting manifest for " <> formatted
    fetchGitHubRepo Constants.manifestIndex env.pullMode env.registryIndex
    let exitMessage = "Failed to delete manifest for " <> formatted <> " from the manifest index."
    Run.liftAff (ManifestIndex.removeFromEntryFile env.registryIndex name version) >>= case _ of
      Left error -> do
        Log.error $ "Could not remove manifest for " <> formatted <> " from its entry file in DeleteManifest: " <> error
        Log.exit exitMessage
      Right _ -> do
        let message = "Update manifest for " <> formatted
        let path = Path.concat [ env.registryIndex, ManifestIndex.packageEntryFilePath name ]
        let commitArgs token mode = { token, mode, paths: String.Pattern path, message }
        case env.writeStrategy of
          Write -> do
            Log.info "Deleted manifest, but not committing because write strategy is Write."
            pure next
          WriteCommit token ->
            Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistryIndex env.registryIndex (commitArgs token CommitOnly))) >>= case _ of
              Left error -> do
                Log.error $ "Could not commit manifest deletion in WriteCommit for " <> formatted <> " due to a Git error: " <> error
                Log.exit exitMessage
              Right _ -> do
                Log.debug $ "Successfully committed manifest deletion for " <> formatted <> " (did not push)"
                pure next
          WriteCommitPush token ->
            Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistryIndex env.registryIndex (commitArgs token CommitAndPush))) >>= case _ of
              Left error -> do
                Log.error $ "Could not commit manifest deletion and push in WriteCommitPush for " <> formatted <> " due to a Git error: " <> error
                Log.exit exitMessage
              Right _ -> do
                Log.debug $ "Successfully committend and pushed manifest deletion for " <> formatted
                pure next

  ReadAllManifests reply -> do
    Log.info $ "Reading manifest index from " <> env.registryIndex
    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.manifestIndex env.pullMode env.registryIndex
    index <- readManifestIndexFromDisk env.registryIndex
    pure $ reply index

  ReadMetadata name reply -> do
    let printedName = PackageName.print name
    let path = Path.concat [ env.registry, Constants.metadataDirectory, printedName <> ".json" ]
    Log.info $ "Reading metadata for " <> printedName <> " from path " <> path
    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.registry env.pullMode env.registry
    let exitMessage = "Found metadata for " <> printedName <> ", but it could not be read."
    Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
      Left fsError -> do
        Log.warn $ "Could not find metadata file for package " <> printedName <> ": " <> Aff.message fsError
        pure $ reply Nothing
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
            Log.debug $ "Successfully read metadata for " <> printedName
            pure $ reply $ Just metadata

  WriteMetadata name metadata next -> do
    let printedName = PackageName.print name
    Log.info $ "Writing metadata for " <> printedName <> ":\n" <> printJson Metadata.codec metadata
    fetchGitHubRepo Constants.registry env.pullMode env.registry
    let path = Path.concat [ env.registry, Constants.metadataDirectory, printedName <> ".json" ]
    let couldNotWriteError = "Could not write metadata for " <> printedName
    Run.liftAff (Aff.attempt (writeJsonFile Metadata.codec path metadata)) >>= case _ of
      Left fsError -> do
        Log.error $ "Failed to write metadata for " <> printedName <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Log.exit couldNotWriteError
      Right _ -> do
        let message = "Update metadata for " <> printedName
        let commitArgs token mode = { token, mode, paths: String.Pattern path, message }
        case env.writeStrategy of
          Write -> do
            Log.info $ "Wrote metadata for " <> printedName <> " but not committing because write strategy is Write."
            pure next
          WriteCommit token ->
            Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistry env.registry (commitArgs token CommitOnly))) >>= case _ of
              Left error -> do
                Log.error $ "Could not commit updated metadata in WriteCommit for " <> printedName <> " due to a git error: " <> error
                Log.exit couldNotWriteError
              Right _ -> do
                Log.debug $ "Successfully committed updated metadata for " <> printedName <> " (did not push)"
                pure next
          WriteCommitPush token ->
            Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistry env.registry (commitArgs token CommitAndPush))) >>= case _ of
              Left error -> do
                Log.error $ "Could not commit and push updated metadata WriteCommitPush for " <> printedName <> " due to a git error: " <> error
                Log.exit couldNotWriteError
              Right _ -> do
                Log.debug $ "Successfully committend and pushed updated metadata for " <> printedName
                pure next

  ReadAllMetadata reply -> do
    let metadataDir = Path.concat [ env.registry, Constants.metadataDirectory ]
    Log.info $ "Reading metadata for all packages from directory " <> metadataDir
    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.registry env.pullMode env.registry
    allMetadata <- readAllMetadataFromDisk metadataDir
    pure $ reply allMetadata

  ReadLatestPackageSet reply -> do
    let packageSetsDir = Path.concat [ env.registry, Constants.packageSetsDirectory ]
    Log.info $ "Reading latest package set from directory " <> packageSetsDir
    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.registry env.pullMode env.registry
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
    let path = Path.concat [ env.registry, Constants.packageSetsDirectory, name <> ".json" ]
    Log.info $ "Writing package set to " <> path
    fetchGitHubRepo Constants.registry env.pullMode env.registry
    let couldNotWriteError = "Could not write package set " <> name
    let commitArgs token mode = { token, mode, paths: String.Pattern path, message }
    Run.liftAff (Aff.attempt (writeJsonFile PackageSet.codec path set)) >>= case _ of
      Left fsError -> do
        Log.error $ "Failed to write package set " <> name <> " to path " <> path <> " do to an fs error: " <> Aff.message fsError
        Log.exit couldNotWriteError
      Right _ -> case env.writeStrategy of
        Write -> do
          Log.info $ "Wrote packages set " <> name <> " but not committing because write strategy is Write."
          pure next
        WriteCommit token ->
          Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistry env.registry (commitArgs token CommitOnly))) >>= case _ of
            Left error -> do
              Log.error $ "Could not commit package set " <> name <> " in WriteCommit due to a git error: " <> error
              Log.exit couldNotWriteError
            Right _ -> do
              Log.debug $ "Successfully committed package set " <> name <> " (did not push)"
              pure next
        WriteCommitPush token ->
          Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistry env.registry (commitArgs token CommitAndPush))) >>= case _ of
            Left error -> do
              Log.error $ "Could not commit and push package set " <> name <> " in WriteCommitPush due to a git error: " <> error
              Log.exit couldNotWriteError
            Right _ -> do
              Log.debug $ "Successfully committed and pushed package set " <> name
              pure next

  ReadAllPackageSets reply -> do
    let packageSetsDir = Path.concat [ env.registry, Constants.packageSetsDirectory ]
    Log.info $ "Reading all package sets from directory " <> packageSetsDir
    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.registry env.pullMode env.registry
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
    Log.info $ "Mirroring legacy package set " <> name <> " to the legacy package sets repo " <> show Legacy.PackageSet.legacyPackageSetsRepo
    manifests <- handleRegistryGit env (ReadAllManifests identity)
    metadata <- handleRegistryGit env (ReadAllMetadata identity)

    Log.debug $ "Converting package set..."
    converted <- case Legacy.Manifest.convertPackageSet manifests metadata set of
      Left error -> do
        Log.error $ "Failed to convert package set " <> name <> " to a legacy package set: " <> error
        Log.exit $ "Could not produce a legacy package set from package set " <> name
      Right converted -> pure converted

    let printedTag = Legacy.PackageSet.printPscTag converted.tag
    packageSetsTags <- GitHub.listTags Legacy.PackageSet.legacyPackageSetsRepo >>= case _ of
      Left githubError -> do
        Log.error $ "Fetching tags for the package-sets repo failed: " <> Octokit.printGitHubError githubError
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

    fetchGitHubRepo Legacy.PackageSet.legacyPackageSetsRepo ForceClean env.legacyPackageSets
    let latestSetsPath = Path.concat [ env.legacyPackageSets, "latest-compatible-sets.json" ]
    let compilerKey = (un PscTag converted.tag).compiler
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
    let dhallPath = Path.concat [ env.legacyPackageSets, "src", "packages.dhall" ]
    Log.debug $ "Writing " <> dhallPath
    Run.liftAff $ FS.Aff.writeTextFile UTF8 dhallPath (Legacy.PackageSet.printDhall converted.packageSet)

    let packagesJsonPath = Path.concat [ env.legacyPackageSets, "packages.json" ]
    Log.debug $ "Writing " <> packagesJsonPath
    Run.liftAff $ writeJsonFile legacyPackageSetCodec packagesJsonPath converted.packageSet

    Log.debug $ "Writing " <> latestSetsPath
    Run.liftAff $ writeJsonFile Legacy.PackageSet.latestCompatibleSetsCodec latestSetsPath latestCompatibleSets

    let
      message = "Update to the " <> name <> " package set."
      files = [ dhallPath, packagesJsonPath, latestSetsPath ]
      -- We push the stable tag (ie. just a compiler version) if one does not yet
      -- exist. We always push the full tag.
      printedCompiler = Version.print compilerKey
      tags = Array.catMaybes
        [ if Set.member printedCompiler packageSetsTags then Nothing else Just printedCompiler
        , Just printedTag
        ]

    case env.writeStrategy of
      Write -> do
        Log.info $ "Created a legacy package set, but not committing because write mode was set to Write"
        pure next
      WriteCommit token -> Run.liftAff (commitLegacyPackageSets { token, files, tags, message, push: false }) >>= case _ of
        Left error -> do
          Log.error $ "Could not commit legacy package set " <> name <> " due to a git error: " <> error
          Log.exit "Could not commit legacy package set."
        Right _ -> do
          Log.debug $ "Successfully committed package set " <> name <> " (did not push)"
          pure next
      WriteCommitPush token -> Run.liftAff (commitLegacyPackageSets { token, files, tags, message, push: true }) >>= case _ of
        Left error -> do
          Log.error $ "Could not commit and mirror legacy package set " <> name <> " due to a git error: " <> error
          Log.exit $ "Could not mirror legacy package set."
        Right _ -> do
          Log.info $ "Successfully committed and pushed package set " <> name
          pure next

  ReadLegacyRegistry reply -> do
    Log.info "Reading legacy registry."

    ifElapsed env.timer (Minutes 5.0) do
      Log.debug "Timer expired on repo fetch."
      fetchGitHubRepo Constants.registry ForceClean env.registry

    let readRegistryFile path = readJsonFile (CA.Common.strMap CA.string) (Path.concat [ env.registry, path ])

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

    { bower, new } <- handleRegistryGit env $ ReadLegacyRegistry identity

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

    for_ targetFile \file -> do
      let sourcePackages = if file == "new-packages.json" then new else bower
      let packages = Map.insert rawPackageName url sourcePackages
      let path = Path.concat [ env.registry, file ]
      let message = "Sync " <> PackageName.print name <> " with legacy registry."
      let commitArgs token mode = { token, mode, paths: String.Pattern path, message }
      let couldNotSyncError = "Could not sync " <> PackageName.print name <> " with the legacy registry files."
      Run.liftAff $ writeJsonFile (CA.Common.strMap CA.string) path packages
      case env.writeStrategy of
        Write ->
          Log.info $ "Created legacy registry file, but did not commit because write mode was set to Write"
        WriteCommit token ->
          Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistry env.registry (commitArgs token CommitOnly))) >>= case _ of
            Left error -> do
              Log.error $ "Could not commit legacy registry file " <> file <> " due to a git error: " <> error
              Log.exit couldNotSyncError
            Right _ -> do
              Log.info $ "Successfully committed legacy registry file " <> file <> " (did not push)"
        WriteCommitPush token ->
          Run.liftAff (Except.runExceptT (Git.pacchettiBottiCommitRegistry env.registry (commitArgs token CommitAndPush))) >>= case _ of
            Left error -> do
              Log.error $ "Could not commit and push legacy registry file " <> file <> " due to a git error: " <> error
              Log.exit couldNotSyncError
            Right _ -> do
              Log.info $ "Successfully committed and pushed legacy registry file " <> file
    pure next

  where
  ifElapsed :: Ref (Maybe DateTime) -> Minutes -> Run _ Unit -> Run _ Unit
  ifElapsed timer minutes k = do
    now <- Run.liftEffect nowUTC
    Run.liftEffect (Ref.read timer) >>= case _ of
      Just prev | diff now prev < minutes -> pure unit
      _ -> Run.liftEffect (Ref.write (Just now) timer) *> k

  commitLegacyPackageSets :: { token :: GitHubToken, tags :: Array String, files :: Array FilePath, message :: String, push :: Boolean } -> Aff (Either String Unit)
  commitLegacyPackageSets { token, tags, files, message, push } = Except.runExceptT do
    let upstream = Legacy.PackageSet.legacyPackageSetsRepo.owner <> "/" <> Legacy.PackageSet.legacyPackageSetsRepo.repo
    let origin = "https://pacchettibotti:" <> un GitHubToken token <> "@github.com/" <> upstream <> ".git"
    for_ files \file -> Git.runGit_ [ "add", file ] (Just env.legacyPackageSets)
    Git.runGit_ [ "commit", "-m", message ] (Just env.legacyPackageSets)
    when push do
      Git.runGit_ [ "push", origin, "master" ] (Just env.legacyPackageSets)
      for_ tags \tag -> do
        Git.runGit_ [ "tag", tag ] (Just env.legacyPackageSets)
        Git.runGit_ [ "push", origin, tag ] (Just env.legacyPackageSets)

-- | How to sync a locally-checked-out repository.
-- |
-- | - Autostash will stash any changes you have, pull, then apply your changes.
-- | - ForceClean will remove untracked files and hard reset before pulling.
-- | - OnlyClean will abort in the presence of untracked or modified files.
data PullMode = Autostash | OnlyClean | ForceClean

derive instance Eq PullMode

-- | Attempt to fetch a repository to the given file path; if it already exists
-- | there, use the provided pull mode to resolve changes.
fetchGitHubRepo :: forall r. Address -> PullMode -> FilePath -> Run (LOG + LOG_EXCEPT + AFF + EFFECT + r) Unit
fetchGitHubRepo address mode path = Run.liftEffect (FS.Sync.exists path) >>= case _ of
  -- When the repository doesn't exist at the given file path, we can ignore the
  -- mode and go straight to the checkout.
  false -> do
    let prettyRepo = address.owner <> "/" <> address.repo
    let clonePath = "https://github.com/" <> prettyRepo <> ".git"
    Log.debug $ "Didn't find " <> prettyRepo <> " locally, cloning..."
    Run.liftAff (Except.runExceptT (Git.runGit_ [ "clone", clonePath, path ] Nothing)) >>= case _ of
      Left err -> do
        Log.error $ "Failed to git clone repo " <> clonePath <> " due to a git error: " <> err
        Log.exit $ "Could not read the repository at " <> prettyRepo
      Right _ -> pure unit

  true -> do
    let prettyRepo = address.owner <> "/" <> address.repo
    Log.debug $ "Found the " <> prettyRepo <> " repo locally, fetching latest changes..."
    Run.liftAff (Except.runExceptT (Git.runGit [ "rev-parse", "--abbrev-ref", "HEAD" ] (Just path))) >>= case _ of
      Left error -> do
        Log.error $ "Failed to check the local branch in use for repo " <> prettyRepo <> " due to a git error: " <> error
        Log.exit $ "Could not read the branch of local checkout of " <> prettyRepo
      Right branch ->
        unless (branch == "main" || branch == "master") do
          Log.warn $ prettyRepo <> " is on a branch other than main or master, you may have unexpected results!"

    isClean <- Run.liftAff (Except.runExceptT (Git.runGit [ "status", "--short" ] (Just path))) >>= case _ of
      Left error -> do
        Log.error $ "Failed to check the local git status for " <> prettyRepo <> " due to a git error: " <> error
        Log.exit $ "Could not read the status of the local checkout of " <> prettyRepo
      Right "" -> do
        Log.debug $ "Local checkout of " <> prettyRepo <> " has no untracked or dirty files, it is safe to pull the latest."
        pure true
      Right files -> do
        Log.warn $ "Local checkout of " <> prettyRepo <> " contains untracked or dirty files, and may not be safe to fetch:\n" <> files
        pure false

    case mode of
      Autostash -> do
        Log.debug $ "Pulling " <> prettyRepo <> " in autostash mode, which preserves local changes."
        Run.liftAff (Except.runExceptT (Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just path))) >>= case _ of
          Left error -> do
            Log.error $ "Failed to pull the latest changes for repo " <> prettyRepo <> " due to a git error: " <> error
            Log.exit $ "Could not fetch the latest changes for " <> prettyRepo
          Right _ -> pure unit

      OnlyClean | not isClean -> do
        Log.error $ "Not pulling changes for " <> prettyRepo <> " because the local checkout is dirty and only-clean mode was specified."
        Log.exit $ "Could not fetch the latest changes for " <> prettyRepo

      OnlyClean -> do
        Log.debug $ "Pulling " <> address.repo <> " in only-clean mode."
        Run.liftAff (Except.runExceptT (Git.runGit_ [ "pull" ] (Just path))) >>= case _ of
          Left error -> do
            Log.error $ "Failed to pull the latest changes for " <> prettyRepo <> " due to a git error: " <> error
            Log.exit $ "Could not fetch the latest changes for " <> prettyRepo
          Right _ -> pure unit

      ForceClean -> do
        unless isClean do
          Log.info $ "Cleaning local checkout of " <> prettyRepo <> " because force-clean mode was specified."

        let
          cleanLocal = do
            -- First, we hard-reset to clear modified files
            Git.runGit_ [ "reset", "--hard" ] (Just path)
            -- Second, we clean to remove untracked files
            Git.runGit_ [ "clean", "-d", "-x", "--force" ] (Just path)

        Run.liftAff (Except.runExceptT cleanLocal) >>= case _ of
          Left error -> do
            Log.error $ "Failed to clean the local checkout of " <> prettyRepo <> " due to a git error: " <> error
            Log.exit $ "Could not fetch the latest changes for " <> prettyRepo
          Right _ -> pure unit

        Log.debug $ "Cleaned local checkout, now pulling " <> prettyRepo <> " in force-clean mode."
        Run.liftAff (Except.runExceptT (Git.runGit_ [ "pull" ] (Just path))) >>= case _ of
          Left error -> do
            Log.error $ "Failed to pull the latest changes for " <> prettyRepo <> " due to a git error: " <> error
            Log.exit $ "Could not fetch the latest changes for " <> prettyRepo
          Right _ -> pure unit

-- | Given the file path of a local manifest index on disk, read its contents.
readManifestIndexFromDisk :: forall r. FilePath -> Run (LOG + LOG_EXCEPT + AFF + r) ManifestIndex
readManifestIndexFromDisk root = do
  paths <- Run.liftAff $ FastGlob.match' root [ "**/*" ] { include: FastGlob.FilesOnly, ignore: [ "config.json", "README.md" ] }

  let
    packages = do
      let parsePath = Path.basename >>> \path -> lmap (Tuple path) (PackageName.parse path)
      partitionEithers $ map parsePath paths.succeeded

  unless (Array.null packages.fail) do
    Log.warn $ Array.fold
      [ "Some entries in the manifest index are not valid package names: "
      , Array.foldMap (\(Tuple path err) -> "\n  - " <> path <> ": " <> err) packages.fail
      ]

  entries <- Run.liftAff $ map partitionEithers $ for packages.success (ManifestIndex.readEntryFile root)
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
