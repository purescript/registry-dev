-- | This script attempts to import all package versions for packages listed in
-- | the legacy registry files (ie. bower-packages.json and new-packages.json).
-- |
-- | It can be run in different modes depending on whether you want to generate
-- | the registry from scratch, including uploading packages to the backend or
-- | you just want to iteratively pick up new releases.
module Registry.Scripts.LegacyImporter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Alternative (guard)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.Compactable (separate)
import Data.Filterable (partition)
import Data.Foldable (foldMap)
import Data.Foldable as Foldable
import Data.Formatter.DateTime as Formatter.DateTime
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map as Map
import Data.Ordering (invert)
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.Variant as Variant
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process as Process
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT, LogVerbosity(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify as Notify
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (PullMode(..), REGISTRY, RegistryEnv, WriteStrategy(..))
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion (LenientVersion)
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Manifest (LegacyManifestError(..), LegacyManifestValidationError)
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), rawPackageNameMapCodec, rawVersionMapCodec)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (Address, Tag)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Location as Location
import Registry.ManifestIndex as ManifestIndex
import Registry.Operation (PublishData)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (Run)
import Run as Run
import Run.Except (Except)
import Run.Except as Run.Except
import Type.Proxy (Proxy(..))

data ImportMode = DryRun | GenerateRegistry | UpdateRegistry

derive instance Eq ImportMode

parser :: ArgParser ImportMode
parser = Arg.choose "command"
  [ Arg.flag [ "dry-run" ]
      "Run the registry importer without uploading packages or committing files."
      $> DryRun
  , Arg.flag [ "generate-registry" ]
      "Run the registry importer, uploading packages but not committing to metadata or the index."
      $> GenerateRegistry
  , Arg.flag [ "update-registry" ]
      "Run the registry importer, uploading packages and committing to metadata and the index."
      $> UpdateRegistry
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "A script for uploading legacy registry packages."
  mode <- case Arg.parseArgs "legacy-importer" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  FS.Extra.ensureDirectory scratchDir

  -- Set up interpreters according to the import mode. In dry-run mode we don't
  -- allow anyting to be committed or pushed, but data is still written to the
  -- local repository checkouts on disk. In generate-registry mode, tarballs are
  -- uploaded, but nothing is committed. In update-registry mode, tarballs are
  -- uploaded and manifests and metadata are written, committed, and pushed.

  -- TODO TODO TODO
  -- Well, shit, I guess we need to bump 'publishPursuit' into its own effect
  -- too, or else everything requires pacchettibotti tokens now. Annoying.
  -- case mode of
  --   DryRun ->

  -- Environment setup
  Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret
  let pacchettibottiEnv = { publicKey, privateKey, token }

  -- Registry setup
  let
    registryEnv :: RegistryEnv
    registryEnv =
      { legacyPackageSets: Path.concat [ scratchDir, "package-sets" ]
      , registry: Path.concat [ scratchDir, "registry" ]
      , registryIndex: Path.concat [ scratchDir, "registry-index" ]
      , timers: unsafePerformEffect Registry.newTimers
      , writeStrategy: WriteCommitPush token
      , pullMode: ForceClean
      }

  -- Storage setup
  let storageEnv = { key: spacesKey, secret: spacesSecret }

  -- Package sets setup
  let packageSetsEnv = { workdir: Path.concat [ scratchDir, "package-sets-work" ] }

  -- GitHub setup
  octokit <- liftEffect $ Octokit.newOctokit token

  -- Caching setup
  githubCacheRef <- Cache.newCacheRef
  legacyCacheRef <- Cache.newCacheRef
  let cacheDir = Path.concat [ scratchDir, ".cache" ]

  -- Logging setup
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  now <- liftEffect nowUTC
  let logFile = "legacy-importer-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  runLegacyImport mode
    -- Environment
    # Env.runPacchettiBottiEnv pacchettibottiEnv
    -- App effects
    # PackageSets.runPackageSets (PackageSets.handlePackageSetsAff packageSetsEnv)
    # Registry.runRegistry (Registry.handleRegistryGit registryEnv)
    # Storage.runStorage (Storage.handleStorageS3 storageEnv)
    # GitHub.runGitHub (GitHub.handleGitHubOctokit octokit)
    -- Caching
    # Storage.runStorageCacheFs cacheDir
    # GitHub.runGitHubCacheMemoryFs githubCacheRef cacheDir
    # Legacy.Manifest.runLegacyCacheMemoryFs legacyCacheRef cacheDir
    -- Logging
    # Notify.runNotify Notify.handleNotifyLog
    # Run.Except.catchAt Log._logExcept (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Log.runLog (\log -> Log.handleLogTerminal Normal log *> Log.handleLogFs Verbose logPath log)
    -- Base effects
    # Run.runBaseAff'

runLegacyImport :: forall r. ImportMode -> Run (API.PublishEffects + r) Unit
runLegacyImport mode = do
  Log.info "Ensuring the registry is well-formed..."

  let
    hasMetadata allMetadata package version = case Map.lookup package allMetadata of
      Nothing -> false
      Just (Metadata m) -> isJust (Map.lookup version m.published) || isJust (Map.lookup version m.unpublished)

  Log.info "Removing entries from the manifest index that don't have accompanying metadata..."
  _ <- do
    allManifests <- Registry.readAllManifests
    allMetadata <- Registry.readAllMetadata
    -- To ensure the metadata and registry index are always in sync, we remove
    -- any entries from the registry index that don't have accompanying metadata
    let mismatched = mapWithIndex (Map.filterKeys <<< not <<< hasMetadata allMetadata) $ ManifestIndex.toMap allManifests
    forWithIndex mismatched \package versions ->
      forWithIndex versions \version _ -> do
        Log.debug $ "Found mismatch: " <> formatPackageVersion package version
        Registry.deleteManifest package version

  Log.info "Reading legacy registry files..."
  legacyRegistry <- do
    { bower, new } <- Registry.readLegacyRegistry
    let allPackages = Map.union bower new
    let fixupNames = mapKeys (RawPackageName <<< stripPureScriptPrefix)
    pure $ fixupNames allPackages

  Log.info "Importing packages from the legacy registry that have not already been uploaded..."
  importedIndex <- importLegacyRegistry legacyRegistry

  Log.info $ formatImportStats $ calculateImportStats legacyRegistry importedIndex

  Log.info "Writing package and version failures to disk..."
  Run.liftAff $ writePackageFailures importedIndex.failedPackages
  Run.liftAff $ writeVersionFailures importedIndex.failedVersions

  Log.info "Writing empty metadata files for legacy packages that can't be registered..."
  void $ forWithIndex importedIndex.reservedPackages \package location -> do
    Registry.readMetadata package >>= case _ of
      Nothing -> do
        let metadata = Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
        Registry.writeMetadata package metadata
      Just _ -> pure unit

  Log.info "Sorting packages for upload..."
  let indexPackages = ManifestIndex.toSortedArray importedIndex.registryIndex

  allMetadata <- Registry.readAllMetadata

  let
    isPublished { name, version } = hasMetadata allMetadata name version
    notPublished = indexPackages # Array.filter \(Manifest manifest) -> not (isPublished manifest)

    mkOperation :: Manifest -> Run _ PublishData
    mkOperation (Manifest manifest) =
      case Map.lookup manifest.version =<< Map.lookup manifest.name importedIndex.packageRefs of
        Nothing -> do
          let formatted = formatPackageVersion manifest.name manifest.version
          Log.error $ "Unable to recover package ref for " <> formatted
          Log.exit $ "Failed to create publish operation for " <> formatted
        Just ref ->
          pure
            { location: Just manifest.location
            , name: manifest.name
            , ref: un RawVersion ref
            , compiler: unsafeFromRight $ Version.parse "0.15.4"
            , resolutions: Nothing
            }

  case notPublished of
    [] -> Log.info "No packages to publish."
    manifests -> do
      let printPackage (Manifest { name, version }) = formatPackageVersion name version
      Log.info $ Array.foldMap (append "\n")
        [ "----------"
        , "AVAILABLE TO PUBLISH"
        , "----------"
        , Array.foldMap (append "\n  - " <<< printPackage) manifests
        ]

      let
        source = case mode of
          DryRun -> Current
          GenerateRegistry -> Legacy
          UpdateRegistry -> Current

      void $ for notPublished \(Manifest manifest) -> do
        Log.info $ Array.foldMap (append "\n")
          [ "----------"
          , "PUBLISHING: " <> formatPackageVersion manifest.name manifest.version
          , stringifyJson Location.codec manifest.location
          , "----------"
          ]
        operation <- mkOperation (Manifest manifest)
        API.publish source operation

-- | Record all package failures to the 'package-failures.json' file.
writePackageFailures :: Map RawPackageName PackageValidationError -> Aff Unit
writePackageFailures =
  writeJsonFile (rawPackageNameMapCodec jsonValidationErrorCodec) (Path.concat [ scratchDir, "package-failures.json" ])
    <<< map formatPackageValidationError

-- | Record all version failures to the 'version-failures.json' file.
writeVersionFailures :: Map RawPackageName (Map RawVersion VersionValidationError) -> Aff Unit
writeVersionFailures =
  writeJsonFile (rawPackageNameMapCodec (rawVersionMapCodec jsonValidationErrorCodec)) (Path.concat [ scratchDir, "version-failures.json" ])
    <<< map (map formatVersionValidationError)

type LegacyRegistry = Map RawPackageName String

type ImportedIndex =
  { failedPackages :: Map RawPackageName PackageValidationError
  , failedVersions :: Map RawPackageName (Map RawVersion VersionValidationError)
  , reservedPackages :: Map PackageName Location
  , registryIndex :: ManifestIndex
  , packageRefs :: Map PackageName (Map Version RawVersion)
  }

-- | Construct a valid registry index containing manifests for all packages from
-- | the legacy registry files. This function also collects import errors for
-- | packages and package versions and reports packages that are present in the
-- | legacy registry but not in the resulting registry.
importLegacyRegistry :: forall r. LegacyRegistry -> Run (REGISTRY + GITHUB + LOG + LOG_EXCEPT + r) ImportedIndex
importLegacyRegistry legacyRegistry = do
  manifests <- forWithIndex legacyRegistry \name address ->
    Run.Except.runExceptAt _exceptPackage (buildLegacyPackageManifests name address)

  let
    separatedPackages :: { left :: Map RawPackageName PackageValidationError, right :: Map RawPackageName (Map RawVersion _) }
    separatedPackages = separate manifests

    separatedVersions :: { left :: Map RawPackageName (Map RawVersion VersionValidationError), right :: Map RawPackageName (Map RawVersion Manifest) }
    separatedVersions =
      separatedPackages.right # flip foldlWithIndex { left: Map.empty, right: Map.empty } \key acc next -> do
        let { left, right } = separate next
        { left: if Map.isEmpty left then acc.left else Map.insert key left acc.left
        , right: if Map.isEmpty right then acc.right else Map.insert key right acc.right
        }

    validLegacyManifests :: Set Manifest
    validLegacyManifests = Set.fromFoldable $ foldMap Map.values $ Map.values separatedVersions.right

    -- The raw ref strings associated with the input package names and versions
    packageRefs :: Map PackageName (Map Version RawVersion)
    packageRefs = Map.fromFoldableWith Map.union do
      Tuple _ rawVersions <- Map.toUnfoldable separatedVersions.right
      Tuple rawVersion (Manifest manifest) <- Map.toUnfoldable rawVersions
      [ Tuple manifest.name (Map.singleton manifest.version rawVersion) ]

    -- A 'checked' index is one where we have verified that all dependencies
    -- are self-contained within the registry.
    Tuple unsatisfied validIndex = ManifestIndex.maximalIndex validLegacyManifests

    -- The list of all packages that were present in the legacy registry files,
    -- but which have no versions present in the fully-imported registry. These
    -- packages still need to have empty metadata files written for them.
    reservedPackages :: Map PackageName Location
    reservedPackages =
      Map.fromFoldable $ Array.mapMaybe reserved $ Map.toUnfoldable legacyRegistry
      where
      reserved (Tuple (RawPackageName name) address) = do
        packageName <- hush $ PackageName.parse name
        guard $ isNothing $ Map.lookup packageName $ ManifestIndex.toMap validIndex
        { owner, repo } <- hush $ Parsing.runParser address legacyRepoParser
        pure (Tuple packageName (GitHub { owner, repo, subdir: Nothing }))

    -- The list of all packages that could not be included because of an error
    -- with the overall package, prior to fetching any versions.
    packageFailures :: Map RawPackageName PackageValidationError
    packageFailures = separatedPackages.left

    -- The list of all package versions that could not be included because of
    -- an error with the specific version. Includes failures to fetch or parse
    -- manifest files as well as valid manifests that contain dependencies that
    -- are not in the registry.
    versionFailures :: Map RawPackageName (Map RawVersion VersionValidationError)
    versionFailures = do
      let
        foldFn acc fail = do
          let error = { error: UnregisteredDependencies fail.dependencies, reason: "Contains dependencies that are not registered." }
          Map.insertWith Map.union fail.package (Map.singleton fail.version error) acc
        dependencyFailures =
          Array.foldl foldFn Map.empty do
            Tuple name versions <- Map.toUnfoldable unsatisfied
            Tuple version deps <- Map.toUnfoldable versions
            let ref = unsafeFromJust (Map.lookup name packageRefs >>= Map.lookup version)
            [ { package: RawPackageName (PackageName.print name), version: ref, dependencies: Array.fromFoldable $ Map.keys deps } ]
      Map.unionWith Map.union separatedVersions.left dependencyFailures

  pure
    { failedPackages: packageFailures
    , failedVersions: versionFailures
    , reservedPackages: reservedPackages
    , registryIndex: validIndex
    , packageRefs
    }

-- | Attempt to build valid manifests for all releases associated with the given
-- | legacy package. This will result in a package error if versions could not
-- | be fetched in the first place. Otherwise, it will produce errors for all
-- | versions that don't produce valid manifests, and manifests for all that do.
buildLegacyPackageManifests
  :: forall r
   . RawPackageName
  -> String
  -> Run (EXCEPT_PACKAGE + REGISTRY + GITHUB + LOG + LOG_EXCEPT + r) (Map RawVersion (Either VersionValidationError Manifest))
buildLegacyPackageManifests rawPackage rawUrl = do
  package <- validatePackage rawPackage rawUrl

  let
    location :: Location
    location = GitHub { owner: package.address.owner, repo: package.address.repo, subdir: Nothing }

    buildManifestForVersion :: Tag -> Run _ (Either VersionValidationError Manifest)
    buildManifestForVersion tag = Run.Except.runExceptAt _exceptVersion do
      version <- exceptVersion $ validateVersion tag

      let
        buildManifest = do
          exceptVersion $ validateVersionDisabled package.name version
          legacyManifest <- do
            Legacy.Manifest.fetchLegacyManifest package.name package.address (RawVersion tag.name) >>= case _ of
              Left error -> throwVersion { error: InvalidManifest error, reason: "Legacy manifest could not be parsed." }
              Right result -> pure result
          pure $ Legacy.Manifest.toManifest package.name (LenientVersion.version version) location legacyManifest

      -- TODO: This will use the manifest for the package version from the
      -- registry, without trying to produce a legacy manifest. However,  we may
      -- want to always produce a legacy manifest and then compare it to the
      -- existing entry, possibly failing so we can diagnose the difference.
      Registry.readManifest package.name (LenientVersion.version version) >>= case _ of
        Just manifest -> pure manifest
        _ -> do
          -- TODO: How should possibly-failing manifests be cached?
          Log.debug $ "Building manifest in legacy import because it was not found in the registry: " <> formatPackageVersion package.name (LenientVersion.version version)
          -- let codec = CA.Common.either (CA.Record.object "Error" { error: versionErrorCodec, reason: CA.string }) Manifest.codec
          -- Cache.get
          -- liftEffect (Cache.readJsonEntry codec key cache) >>= case _ of
          --   Left _ -> ExceptT do
          --     Console.log $ "CACHE MISS: Building manifest for " <> PackageName.print package.name <> "@" <> tag.name
          --     manifest <- Except.runExceptT buildManifest
          --     liftEffect $ Cache.writeJsonEntry codec key manifest cache
          --     pure manifest
          --   Right contents ->
          --     Except.except contents.value
          -- TODO TODO TODO
          unsafeCrashWith "unimplemented"

  manifests <- for package.tags \tag -> do
    manifest <- buildManifestForVersion tag
    pure (Tuple (RawVersion tag.name) manifest)

  pure $ Map.fromFoldable manifests

type EXCEPT_VERSION :: Row (Type -> Type) -> Row (Type -> Type)
type EXCEPT_VERSION r = (exceptVersion :: Except VersionValidationError | r)

_exceptVersion = Proxy :: Proxy "exceptVersion"

throwVersion :: forall r a. VersionValidationError -> Run (EXCEPT_VERSION + r) a
throwVersion = Run.Except.throwAt _exceptVersion

exceptVersion :: forall r a. Either VersionValidationError a -> Run (EXCEPT_VERSION + r) a
exceptVersion = Run.Except.rethrowAt _exceptVersion

type VersionValidationError = { error :: VersionError, reason :: String }

versionValidationErrorCodec :: JsonCodec VersionValidationError
versionValidationErrorCodec = CA.Record.object "VersionValidationError"
  { error: versionErrorCodec
  , reason: CA.string
  }

-- | An error that affects a specific package version
data VersionError
  = InvalidTag Tag
  | DisabledVersion
  | InvalidManifest LegacyManifestValidationError
  | UnregisteredDependencies (Array PackageName)

versionErrorCodec :: JsonCodec VersionError
versionErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { invalidTag: Right $ CA.Record.object "Tag"
      { name: CA.string
      , sha: CA.string
      , url: CA.string
      }
  , disabledVersion: Left unit
  , invalidManifest: Right $ CA.Record.object "LegacyManifestValidationError"
      { error: Legacy.Manifest.legacyManifestErrorCodec
      , reason: CA.string
      }
  , unregisteredDependencies: Right (CA.array PackageName.codec)
  }
  where
  toVariant = case _ of
    InvalidTag tag -> Variant.inj (Proxy :: _ "invalidTag") tag
    DisabledVersion -> Variant.inj (Proxy :: _ "disabledVersion") unit
    InvalidManifest inner -> Variant.inj (Proxy :: _ "invalidManifest") inner
    UnregisteredDependencies inner -> Variant.inj (Proxy :: _ "unregisteredDependencies") inner

  fromVariant = Variant.match
    { invalidTag: InvalidTag
    , disabledVersion: \_ -> DisabledVersion
    , invalidManifest: InvalidManifest
    , unregisteredDependencies: UnregisteredDependencies
    }

validateVersionDisabled :: PackageName -> LenientVersion -> Either VersionValidationError Unit
validateVersionDisabled package version =
  case Map.lookup (Tuple package (LenientVersion.raw version)) disabledPackageVersions of
    Nothing -> pure unit
    Just reason -> Left { error: DisabledVersion, reason }
  where
  disabledPackageVersions :: Map (Tuple PackageName String) String
  disabledPackageVersions = Map.fromFoldable
    [ Tuple (disabled "concur-core" "v0.3.9") noSrcDirectory
    , Tuple (disabled "concur-react" "v0.3.9") noSrcDirectory
    , Tuple (disabled "pux-devtool" "v5.0.0") noSrcDirectory
    , Tuple (disabled "endpoints-express" "0.0.1") noSrcDirectory
    ]
    where
    noSrcDirectory = "Does not contain a 'src' directory."
    disabled name = Tuple (unsafeFromRight $ PackageName.parse name)

validateVersion :: Tag -> Either VersionValidationError LenientVersion
validateVersion tag =
  LenientVersion.parse tag.name # lmap \parseError ->
    { error: InvalidTag tag
    , reason: parseError
    }

type EXCEPT_PACKAGE :: Row (Type -> Type) -> Row (Type -> Type)
type EXCEPT_PACKAGE r = (exceptPackage :: Except PackageValidationError | r)

_exceptPackage = Proxy :: Proxy "exceptPackage"

throwPackage :: forall r a. PackageValidationError -> Run (EXCEPT_PACKAGE + r) a
throwPackage = Run.Except.throwAt _exceptPackage

exceptPackage :: forall r a. Either PackageValidationError a -> Run (EXCEPT_PACKAGE + r) a
exceptPackage = Run.Except.rethrowAt _exceptPackage

type PackageValidationError = { error :: PackageError, reason :: String }

-- | An error that affects an entire package
data PackageError
  = InvalidPackageName
  | InvalidPackageURL String
  | PackageURLRedirects { registered :: Address, received :: Address }
  | CannotAccessRepo Address
  | DisabledPackage

derive instance Eq PackageError

type PackageResult =
  { name :: PackageName
  , address :: Address
  , tags :: Array Tag
  }

validatePackage :: forall r. RawPackageName -> String -> Run (EXCEPT_PACKAGE + GITHUB + LOG_EXCEPT + r) PackageResult
validatePackage rawPackage rawUrl = do
  name <- exceptPackage $ validatePackageName rawPackage
  exceptPackage $ validatePackageDisabled name
  address <- exceptPackage $ validatePackageAddress rawUrl
  tags <- fetchPackageTags address
  -- We do not allow packages that redirect from their registered location elsewhere. The package
  -- transferrer will handle automatically transferring these packages.
  case Array.head tags of
    Nothing -> pure { name, address, tags }
    Just tag -> do
      tagAddress <- exceptPackage case tagUrlToRepoUrl tag.url of
        Nothing -> Left { error: InvalidPackageURL tag.url, reason: "Failed to format redirected " <> tag.url <> " as a GitHub.Address." }
        Just formatted -> Right formatted
      exceptPackage $ validatePackageLocation { registered: address, received: tagAddress }
      pure { name, address, tags }

fetchPackageTags :: forall r. Address -> Run (EXCEPT_PACKAGE + GITHUB + LOG_EXCEPT + r) (Array Tag)
fetchPackageTags address = GitHub.listTags address >>= case _ of
  Left err -> case err of
    Octokit.APIError apiError | apiError.statusCode >= 400 -> do
      let error = CannotAccessRepo address
      let reason = "GitHub API error with status code " <> show apiError.statusCode
      throwPackage { error, reason }
    _ ->
      Log.exit $ String.joinWith "\n"
        [ "Unexpected GitHub error with a status <= 400"
        , Octokit.printGitHubError err
        ]
  Right tags ->
    pure tags

validatePackageLocation :: { registered :: Address, received :: Address } -> Either PackageValidationError Unit
validatePackageLocation addresses = do
  let lower { owner, repo } = String.toLower owner <> "/" <> String.toLower repo
  if lower addresses.registered /= lower addresses.received then
    Left
      { error: PackageURLRedirects addresses
      , reason: "Registered address " <> show addresses.registered <> " redirects to another location " <> show addresses.received
      }
  else
    Right unit

validatePackageAddress :: String -> Either PackageValidationError Address
validatePackageAddress packageUrl =
  Parsing.runParser packageUrl legacyRepoParser # lmap \parserError ->
    { error: InvalidPackageURL packageUrl
    , reason: Parsing.parseErrorMessage parserError
    }

-- Example tag url:
-- https://api.github.com/repos/octocat/Hello-World/commits/c5b97d5ae6c19d5c5df71a34c7fbeeda2479ccbc
tagUrlToRepoUrl :: String -> Maybe Address
tagUrlToRepoUrl url = do
  noPrefix <- String.stripPrefix (String.Pattern "https://api.github.com/repos/") url
  let getOwnerRepoArray = Array.take 2 <<< String.split (String.Pattern "/")
  case getOwnerRepoArray noPrefix of
    [ owner, repo ] -> Just { owner, repo: String.toLower repo }
    _ -> Nothing

validatePackageDisabled :: PackageName -> Either PackageValidationError Unit
validatePackageDisabled package =
  case Map.lookup (PackageName.print package) disabledPackages of
    Nothing -> pure unit
    Just reason -> Left { error: DisabledPackage, reason }
  where
  -- These packages have no usable versions, but we've discovered by running the
  -- pipeline that they produce at least one manifest. To avoid processing these
  -- packages we manually disable them.
  disabledPackages :: Map String String
  disabledPackages = Map.fromFoldable
    [ Tuple "metadata" reservedPackage
    , Tuple "bitstrings" noSrcDirectory
    , Tuple "purveyor" noSrcDirectory
    , Tuple "styled-components" noSrcDirectory
    , Tuple "styled-system" noSrcDirectory
    ]
    where
    reservedPackage = "Reserved package which cannot be uploaded."
    noSrcDirectory = "No version contains a 'src' directory."

-- | Validate that a package name parses. Expects the package to already have
-- | had its 'purescript-' prefix removed.
validatePackageName :: RawPackageName -> Either PackageValidationError PackageName
validatePackageName (RawPackageName name) =
  PackageName.parse name # lmap \parserError ->
    { error: InvalidPackageName
    , reason: parserError
    }

type JsonValidationError =
  { tag :: String
  , value :: Maybe String
  , reason :: String
  }

jsonValidationErrorCodec :: JsonCodec JsonValidationError
jsonValidationErrorCodec = CA.Record.object "JsonValidationError"
  { tag: CA.string
  , value: CA.Record.optional CA.string
  , reason: CA.string
  }

formatPackageValidationError :: PackageValidationError -> JsonValidationError
formatPackageValidationError { error, reason } = case error of
  InvalidPackageName ->
    { tag: "InvalidPackageName", value: Nothing, reason }
  InvalidPackageURL url ->
    { tag: "InvalidPackageURL", value: Just url, reason }
  PackageURLRedirects { registered } ->
    { tag: "PackageURLRedirects", value: Just (registered.owner <> "/" <> registered.repo), reason }
  CannotAccessRepo address ->
    { tag: "CannotAccessRepo", value: Just (address.owner <> "/" <> address.repo), reason }
  DisabledPackage ->
    { tag: "DisabledPackage", value: Nothing, reason }

formatVersionValidationError :: VersionValidationError -> JsonValidationError
formatVersionValidationError { error, reason } = case error of
  InvalidTag tag ->
    { tag: "InvalidTag", value: Just tag.name, reason }
  DisabledVersion ->
    { tag: "DisabledVersion", value: Nothing, reason }
  InvalidManifest err -> do
    let errorValue = Legacy.Manifest.printLegacyManifestError err.error
    { tag: "InvalidManifest", value: Just errorValue, reason }
  UnregisteredDependencies names -> do
    let errorValue = String.joinWith ", " $ map PackageName.print names
    { tag: "UnregisteredDependencies", value: Just errorValue, reason }

type ImportStats =
  { packagesProcessed :: Int
  , versionsProcessed :: Int
  , packageNamesReserved :: Int
  , packageResults :: { success :: Int, partial :: Int, fail :: Int }
  , versionResults :: { success :: Int, fail :: Int }
  , packageErrors :: Map String Int
  , versionErrors :: Map String Int
  }

formatImportStats :: ImportStats -> String
formatImportStats stats = String.joinWith "\n"
  [ "\n----------\nIMPORT STATS\n----------\n"
  , show stats.packagesProcessed <> " packages processed:"
  , indent $ show stats.packageResults.success <> " fully successful"
  , indent $ show stats.packageResults.partial <> " partially successful"
  , indent $ show (stats.packageNamesReserved - stats.packageResults.fail) <> " reserved (no usable versions)"
  , indent $ show stats.packageResults.fail <> " fully failed"
  , indent "---"
  , formatErrors stats.packageErrors
  , ""
  , show stats.versionsProcessed <> " versions processed:"
  , indent $ show stats.versionResults.success <> " successful"
  , indent $ show stats.versionResults.fail <> " failed"
  , indent "---"
  , formatErrors stats.versionErrors
  , ""
  ]
  where
  indent contents = "  " <> contents
  formatErrors =
    String.joinWith "\n"
      <<< map (\(Tuple error count) -> indent (show count <> " " <> error))
      <<< Array.sortBy (\a b -> invert (compare (snd a) (snd b)))
      <<< Map.toUnfoldableUnordered

calculateImportStats :: LegacyRegistry -> ImportedIndex -> ImportStats
calculateImportStats legacyRegistry imported = do
  let
    registryIndex :: Map RawPackageName (Map RawVersion Manifest)
    registryIndex = Map.fromFoldableWith Map.union do
      Tuple name versions <- Map.toUnfoldable $ ManifestIndex.toMap imported.registryIndex
      Tuple version manifest <- Map.toUnfoldable versions
      let ref = unsafeFromJust (Map.lookup name imported.packageRefs >>= Map.lookup version)
      [ Tuple (RawPackageName (PackageName.print name)) (Map.singleton ref manifest) ]

    packagesProcessed =
      Map.size legacyRegistry

    packageNamesReserved =
      Map.size imported.reservedPackages

    packageResults = do
      let succeeded = Map.keys registryIndex
      let failedPackages = Map.keys imported.failedPackages
      let failedPackageVersions = Map.keys imported.failedVersions
      let both = partition (_ `Set.member` failedPackageVersions) (Array.fromFoldable succeeded)
      { success: Array.length both.no
      , partial: Array.length both.yes
      , fail: Set.size failedPackages
      }

    versionResults =
      { success: Foldable.sum (map Map.size (Map.values registryIndex))
      , fail: Foldable.sum (map Map.size (Map.values imported.failedVersions))
      }

    versionsProcessed =
      versionResults.success + versionResults.fail

    packageErrors =
      Array.foldl (\m error -> Map.insertWith (+) error 1 m) Map.empty
        $ map toKey
        $ Array.fromFoldable
        $ Map.values imported.failedPackages
      where
      toKey = _.error >>> case _ of
        InvalidPackageName -> "Invalid Package Name"
        InvalidPackageURL _ -> "Invalid Package URL"
        PackageURLRedirects _ -> "Package URL Redirects"
        CannotAccessRepo _ -> "Cannot Access Repo"
        DisabledPackage -> "Disabled Package"

    versionErrors =
      Array.foldl (\m error -> Map.insertWith (+) error 1 m) Map.empty
        $ Array.fromFoldable
        $ List.concatMap (map toKey <<< Map.values)
        $ Map.values imported.failedVersions
      where
      toKey = _.error >>> case _ of
        InvalidTag _ -> "Invalid Tag"
        DisabledVersion -> "Disabled Version"
        InvalidManifest err -> "Invalid Manifest (" <> innerKey err <> ")"
        UnregisteredDependencies _ -> "Unregistered Dependencies"

      innerKey = _.error >>> case _ of
        NoManifests -> "No Manifests"
        MissingLicense -> "Missing License"
        InvalidLicense _ -> "Invalid License"
        InvalidDependencies _ -> "Invalid Dependencies"

  { packagesProcessed
  , versionsProcessed
  , packageNamesReserved
  , packageResults
  , versionResults
  , packageErrors
  , versionErrors
  }

legacyRepoParser :: Parser String Address
legacyRepoParser = do
  _ <- Parsing.Combinators.choice
    [ Parsing.String.string "https://github.com/"
    , Parsing.String.string "git://github.com/"
    , Parsing.String.string "git@github.com/"
    ]

  owner <- do
    let
      ownerChoice = Parsing.Combinators.choice
        [ Parsing.String.Basic.alphaNum
        , Parsing.String.char '-'
        ]
    Tuple chars _ <- Parsing.Combinators.Array.manyTill_ ownerChoice (Parsing.String.char '/')
    pure $ String.CodeUnits.fromCharArray chars

  repoWithSuffix <- String.CodeUnits.fromCharArray <$> Array.many Parsing.String.anyChar
  let repo = fromMaybe repoWithSuffix (String.stripSuffix (String.Pattern ".git") repoWithSuffix)

  pure { owner, repo }
