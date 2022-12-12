-- | This script attempts to import all package versions for packages listed in
-- | the legacy registry files (ie. bower-packages.json and new-packages.json).
-- |
-- | It can be run in different modes depending on whether you want to generate
-- | the registry from scratch, including uploading packages to the backend or
-- | you just want to iteratively pick up new releases.
module Registry.Scripts.LegacyImporter where

import Registry.App.Prelude

import Affjax as Http
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Alternative (guard)
import Control.Monad.Except as Except
import Control.Monad.Reader (ask, asks)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.Compactable (separate)
import Data.Filterable (partition)
import Data.Foldable (foldMap)
import Data.Foldable as Foldable
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map as Map
import Data.Ordering (invert)
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Data.Variant as Variant
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Parsing as Parsing
import Registry.App.API (LegacyRegistryFile(..), Source(..))
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.Json (JsonCodec)
import Registry.App.Json as Json
import Registry.App.LenientVersion (LenientVersion)
import Registry.App.LenientVersion as LenientVersion
import Registry.App.PackageIndex as PackageIndex
import Registry.App.PackageStorage as PackageStorage
import Registry.App.RegistryM (RegistryM, commitMetadataFile, readPackagesMetadata, runRegistryM, throwWithComment)
import Registry.Legacy.Manifest (LegacyManifestError(..), LegacyManifestValidationError, LegacyPackageSetEntries)
import Registry.Legacy.Manifest as Legacy.Manifest
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.Operation (PackageOperation(..))
import Registry.PackageName as PackageName
import Registry.Version as Version
import Type.Proxy (Proxy(..))

data ImportMode = DryRun | GenerateRegistry | UpdateRegistry

derive instance Eq ImportMode

parser :: ArgParser ImportMode
parser = Arg.choose "command"
  [ Arg.argument [ "dry-run" ]
      "Run the registry importer without uploading packages or committing files."
      $> DryRun
  , Arg.argument [ "generate-registry" ]
      "Run the registry importer, uploading packages but not committing to metadata or the index."
      $> GenerateRegistry
  , Arg.argument [ "update-registry" ]
      "Run the registry importer, uploading packages and committing to metadata and the index."
      $> UpdateRegistry
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Node.Process.argv
  let description = "A script for uploading legacy registry packages."
  mode <- case Arg.parseArgs "legacy-importer" description parser args of
    Left err -> log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  log "Reading .env file..."
  _ <- API.loadEnv

  FS.Extra.ensureDirectory API.scratchDir

  octokit <- liftEffect do
    token <- do
      result <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
      maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) result
    GitHub.mkOctokit token

  cache <- Cache.useCache API.cacheDir

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env = case mode of
      DryRun ->
        { comment: \err -> error err
        , closeIssue: log "Skipping GitHub issue closing, this is a dry run..."
        , commitMetadataFile: \_ _ -> do
            log "Skipping committing to registry metadata, this is a dry run..."
            pure (Right unit)
        , commitIndexFile: \_ _ -> do
            log "Skipping committing to registry index, this is a dry run..."
            pure (Right unit)
        , commitPackageSetFile: \_ _ _ -> do
            log "Skipping committing to registry package sets, this is a dry run..."
            pure (Right unit)
        , uploadPackage: \_ _ -> log "Skipping upload, this is a dry run..."
        , deletePackage: \_ -> log "Skipping delete, this is a dry run..."
        , octokit
        , cache
        , username: "NO USERNAME"
        , packagesMetadata: metadataRef
        , registry: Path.concat [ API.scratchDir, "registry" ]
        , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
        }
      UpdateRegistry ->
        { comment: \comment -> log ("[COMMENT] " <> comment)
        , closeIssue: log "Running locally, not closing issue..."
        , commitMetadataFile: API.pacchettiBottiPushToRegistryMetadata
        , commitIndexFile: API.pacchettiBottiPushToRegistryIndex
        , commitPackageSetFile: \_ _ _ -> log "Not committing package set in legacy import." $> Right unit
        , uploadPackage: PackageStorage.upload
        , deletePackage: PackageStorage.delete
        , packagesMetadata: metadataRef
        , cache
        , octokit
        , username: mempty
        , registry: Path.concat [ API.scratchDir, "registry" ]
        , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
        }
      GenerateRegistry ->
        { comment: \err -> error err
        , closeIssue: log "Skipping GitHub issue closing, we're running locally.."
        , commitMetadataFile: \_ _ -> do
            log "Skipping committing to registry metadata..."
            pure (Right unit)
        , commitIndexFile: \_ _ -> do
            log "Skipping committing to registry index..."
            pure (Right unit)
        , commitPackageSetFile: \_ _ _ -> do
            log "Skipping committing to registry package sets..."
            pure (Right unit)
        , uploadPackage: PackageStorage.upload
        , deletePackage: PackageStorage.delete
        , octokit
        , cache
        , username: ""
        , packagesMetadata: metadataRef
        , registry: Path.concat [ API.scratchDir, "registry" ]
        , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
        }

  runRegistryM env do
    API.fetchRegistry
    API.fetchRegistryIndex
    API.fillMetadataRef

    registryIndexPath <- asks _.registryIndex
    registryPath <- asks _.registry

    log "Reading existing registry index..."
    existingRegistry <- do
      registry <- PackageIndex.readManifestIndexFromDisk
      -- To ensure the metadata and registry index are always in sync, we remove
      -- any entries from the registry index that don't have accompanying metadata
      metadata <- liftEffect $ Ref.read metadataRef
      let hasMetadata package version = API.isPackageVersionInMetadata package version metadata
      let mismatched = mapWithIndex (Map.filterKeys <<< not <<< hasMetadata) $ ManifestIndex.toMap registry
      if Map.isEmpty mismatched then
        pure registry
      else do
        void $ forWithIndex mismatched \package versions ->
          forWithIndex versions \version _ ->
            ManifestIndex.removeFromEntryFile registryIndexPath package version
        PackageIndex.readManifestIndexFromDisk

    log "Reading legacy registry..."
    legacyRegistry <- readLegacyRegistryFiles

    log "Importing legacy registry packages..."
    importedIndex <- importLegacyRegistry existingRegistry legacyRegistry

    liftAff do
      logImportStats legacyRegistry importedIndex

      log "Writing package and version failures..."
      writePackageFailures importedIndex.failedPackages
      writeVersionFailures importedIndex.failedVersions

    log "Writing metadata for legacy packages that can't be registered..."
    void $ forWithIndex importedIndex.reservedPackages \package location -> do
      metadataMap <- liftEffect $ Ref.read metadataRef
      case Map.lookup package metadataMap of
        Nothing -> do
          let metadata = Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
          liftAff $ Json.writeJsonFile Metadata.codec (API.metadataFile registryPath package) metadata
          liftEffect $ Ref.modify_ (Map.insert package metadata) metadataRef
          commitMetadataFile package >>= case _ of
            Left err -> throwWithComment err
            Right _ -> pure unit
        Just _ -> pure unit

    log "Sorting packages for upload..."
    let indexPackages = ManifestIndex.toSortedArray importedIndex.registryIndex
    metadataMap <- readPackagesMetadata

    let
      isPublished { name, version } =
        API.isPackageVersionInMetadata name version metadataMap

      notPublished =
        indexPackages # Array.filter \(Manifest manifest) -> not (isPublished manifest)

      mkOperation :: Manifest -> PackageOperation
      mkOperation (Manifest manifest) =
        case Map.lookup manifest.version =<< Map.lookup manifest.name importedIndex.packageRefs of
          Nothing ->
            unsafeCrashWith $ "Unable to recover package ref for " <> PackageName.print manifest.name <> "@" <> Version.print manifest.version
          Just ref ->
            Publish
              { location: Just manifest.location
              , name: manifest.name
              , ref: un RawVersion ref
              , compiler: unsafeFromRight $ Version.parse "0.15.4"
              , resolutions: Nothing
              }

    case notPublished of
      [] -> log "No packages to publish."
      manifests -> do
        let printPackage (Manifest { name, version }) = PackageName.print name <> "@" <> Version.print version
        log "\n----------"
        log "AVAILABLE TO PUBLISH"
        log "----------"
        log $ "  " <> String.joinWith "\n  " (map printPackage manifests)

        let
          source = case mode of
            DryRun -> Importer
            UpdateRegistry -> API
            GenerateRegistry -> Importer

        void $ for notPublished \(Manifest manifest) -> do
          log "\n----------"
          log "UPLOADING"
          log $ PackageName.print manifest.name <> "@" <> Version.print manifest.version
          log $ Json.stringifyJson Location.codec manifest.location
          log "----------"
          API.runOperation source (Right (mkOperation (Manifest manifest)))

    when (mode == GenerateRegistry || mode == DryRun) do
      log "Regenerating registry metadata..."
      metadataResult <- readPackagesMetadata
      void $ forWithIndex metadataResult \name metadata -> do
        dir <- asks _.registry
        liftAff (Json.writeJsonFile Metadata.codec (API.metadataFile dir name) metadata)

      log "Regenerating registry index..."
      void $ for indexPackages (liftAff <<< ManifestIndex.insertIntoEntryFile registryIndexPath)

    log "Done!"

-- | Record all package failures to the 'package-failures.json' file.
writePackageFailures :: Map RawPackageName PackageValidationError -> Aff Unit
writePackageFailures =
  Json.writeJsonFile (rawPackageNameMapCodec jsonValidationErrorCodec) (Path.concat [ API.scratchDir, "package-failures.json" ])
    <<< map formatPackageValidationError

-- | Record all version failures to the 'version-failures.json' file.
writeVersionFailures :: Map RawPackageName (Map RawVersion VersionValidationError) -> Aff Unit
writeVersionFailures =
  Json.writeJsonFile (rawPackageNameMapCodec (rawVersionMapCodec jsonValidationErrorCodec)) (Path.concat [ API.scratchDir, "version-failures.json" ])
    <<< map (map formatVersionValidationError)

logImportStats :: LegacyRegistry -> ImportedIndex -> Aff Unit
logImportStats legacy = log <<< formatImportStats <<< calculateImportStats legacy

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
importLegacyRegistry :: ManifestIndex -> LegacyRegistry -> RegistryM ImportedIndex
importLegacyRegistry existingRegistry legacyRegistry = do
  legacyPackageSets <- Legacy.Manifest.fetchLegacyPackageSets
  manifests <- forWithIndex legacyRegistry \name address ->
    Except.runExceptT (buildLegacyPackageManifests existingRegistry legacyPackageSets name address)

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
        { owner, repo } <- hush $ GitHub.parseRepo address
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
  :: ManifestIndex
  -> LegacyPackageSetEntries
  -> RawPackageName
  -> GitHub.PackageURL
  -> ExceptT PackageValidationError RegistryM (Map RawVersion (Either VersionValidationError Manifest))
buildLegacyPackageManifests existingRegistry legacyPackageSets rawPackage rawUrl = do
  { cache } <- ask

  package <- validatePackage rawPackage rawUrl

  let
    location :: Location
    location = GitHub { owner: package.address.owner, repo: package.address.repo, subdir: Nothing }

    buildManifestForVersion :: GitHub.Tag -> ExceptT _ RegistryM Manifest
    buildManifestForVersion tag = do
      version <- Except.except $ validateVersion tag

      let
        buildManifest = do
          Except.except $ validateVersionDisabled package.name version
          let packageSetDeps = Map.lookup (RawVersion tag.name) =<< Map.lookup package.name legacyPackageSets
          let manifestError err = { error: InvalidManifest err, reason: "Legacy manifest could not be parsed." }
          Except.withExceptT manifestError do
            legacyManifest <- Legacy.Manifest.fetchLegacyManifest packageSetDeps package.address (RawVersion tag.name)
            pure $ Legacy.Manifest.toManifest package.name (LenientVersion.version version) location legacyManifest

      case ManifestIndex.lookup package.name (LenientVersion.version version) existingRegistry of
        Just manifest -> pure manifest
        _ -> do
          let key = "manifest__" <> PackageName.print package.name <> "--" <> tag.name
          let codec = CA.Common.either (Json.object "Error" { error: versionErrorCodec, reason: CA.string }) Manifest.codec
          liftEffect (Cache.readJsonEntry codec key cache) >>= case _ of
            Left _ -> ExceptT do
              log $ "CACHE MISS: Building manifest for " <> PackageName.print package.name <> "@" <> tag.name
              manifest <- Except.runExceptT buildManifest
              liftEffect $ Cache.writeJsonEntry codec key manifest cache
              pure manifest
            Right contents ->
              Except.except contents.value

  manifests <- lift $ for package.tags \tag -> do
    manifest <- Except.runExceptT $ buildManifestForVersion tag
    pure (Tuple (RawVersion tag.name) manifest)

  pure $ Map.fromFoldable manifests

type VersionValidationError = { error :: VersionError, reason :: String }

versionValidationErrorCodec :: JsonCodec VersionValidationError
versionValidationErrorCodec = Json.object "VersionValidationError"
  { error: versionErrorCodec
  , reason: CA.string
  }

-- | An error that affects a specific package version
data VersionError
  = InvalidTag GitHub.Tag
  | DisabledVersion
  | InvalidManifest LegacyManifestValidationError
  | UnregisteredDependencies (Array PackageName)

versionErrorCodec :: JsonCodec VersionError
versionErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { invalidTag: Right GitHub.tagCodec
  , disabledVersion: Left unit
  , invalidManifest: Right $ Json.object "LegacyManifestValidationError"
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

validateVersion :: GitHub.Tag -> Either VersionValidationError LenientVersion
validateVersion tag =
  LenientVersion.parse tag.name # lmap \parseError ->
    { error: InvalidTag tag
    , reason: parseError
    }

type PackageValidationError = { error :: PackageError, reason :: String }

-- | An error that affects an entire package
data PackageError
  = InvalidPackageName
  | InvalidPackageURL GitHub.PackageURL
  | PackageURLRedirects { registered :: GitHub.Address, received :: GitHub.Address }
  | CannotAccessRepo GitHub.Address
  | DisabledPackage

derive instance Eq PackageError

type PackageResult =
  { name :: PackageName
  , address :: GitHub.Address
  , tags :: Array GitHub.Tag
  }

validatePackage :: RawPackageName -> GitHub.PackageURL -> ExceptT PackageValidationError RegistryM PackageResult
validatePackage rawPackage rawUrl = do
  name <- Except.except $ validatePackageName rawPackage
  Except.except $ validatePackageDisabled name
  address <- Except.except $ validatePackageAddress rawUrl
  tags <- fetchPackageTags address
  -- We do not allow packages that redirect from their registered location elsewhere. The package
  -- transferrer will handle automatically transferring these packages.
  case Array.head tags of
    Nothing -> pure { name, address, tags }
    Just tag -> do
      tagAddress <- Except.except case tagUrlToRepoUrl tag.url of
        Nothing -> Left { error: InvalidPackageURL (GitHub.PackageURL tag.url), reason: "Failed to format redirected " <> tag.url <> " as a GitHub.Address." }
        Just formatted -> Right formatted
      Except.except $ validatePackageLocation { registered: address, received: tagAddress }
      pure { name, address, tags }

fetchPackageTags :: GitHub.Address -> ExceptT PackageValidationError RegistryM (Array GitHub.Tag)
fetchPackageTags address = do
  { octokit, cache } <- ask
  result <- Except.runExceptT $ Except.mapExceptT liftAff $ GitHub.listTags octokit cache address
  case result of
    Left err -> case err of
      GitHub.APIError apiError | apiError.statusCode >= 400 -> do
        let error = CannotAccessRepo address
        let reason = "GitHub API error with status code " <> show apiError.statusCode
        throwError { error, reason }
      _ ->
        liftEffect $ Exception.throw $ String.joinWith "\n"
          [ "Unexpected GitHub error with a status <= 400"
          , GitHub.printGitHubError err
          ]
    Right tags ->
      pure tags

validatePackageLocation :: { registered :: GitHub.Address, received :: GitHub.Address } -> Either PackageValidationError Unit
validatePackageLocation addresses = do
  let lower { owner, repo } = String.toLower owner <> "/" <> String.toLower repo
  if lower addresses.registered /= lower addresses.received then
    Left
      { error: PackageURLRedirects addresses
      , reason: "Registered address " <> show addresses.registered <> " redirects to another location " <> show addresses.received
      }
  else
    Right unit

validatePackageAddress :: GitHub.PackageURL -> Either PackageValidationError GitHub.Address
validatePackageAddress packageUrl =
  GitHub.parseRepo packageUrl # lmap \parserError ->
    { error: InvalidPackageURL packageUrl
    , reason: Parsing.parseErrorMessage parserError
    }

-- Example tag url:
-- https://api.github.com/repos/octocat/Hello-World/commits/c5b97d5ae6c19d5c5df71a34c7fbeeda2479ccbc
tagUrlToRepoUrl :: Http.URL -> Maybe GitHub.Address
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
jsonValidationErrorCodec = Json.object "JsonValidationError"
  { tag: CA.string
  , value: CA.Record.optional CA.string
  , reason: CA.string
  }

formatPackageValidationError :: PackageValidationError -> JsonValidationError
formatPackageValidationError { error, reason } = case error of
  InvalidPackageName ->
    { tag: "InvalidPackageName", value: Nothing, reason }
  InvalidPackageURL (GitHub.PackageURL url) ->
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

type LegacyRegistry = Map RawPackageName GitHub.PackageURL

-- | Read the legacy registry files stored in the root of the registry repo.
-- | Package names have their 'purescript-' prefix trimmed.
readLegacyRegistryFiles :: RegistryM LegacyRegistry
readLegacyRegistryFiles = do
  bowerPackages <- readLegacyRegistryFile BowerPackages
  registryPackages <- readLegacyRegistryFile NewPackages
  let allPackages = Map.union bowerPackages registryPackages
  let fixupNames = mapKeys (RawPackageName <<< stripPureScriptPrefix)
  pure $ fixupNames allPackages

readLegacyRegistryFile :: LegacyRegistryFile -> RegistryM (Map String GitHub.PackageURL)
readLegacyRegistryFile sourceFile = do
  { registry } <- ask
  let path = Path.concat [ registry, API.legacyRegistryFilePath sourceFile ]
  legacyPackages <- liftAff $ Json.readJsonFile API.legacyRegistryCodec path
  case legacyPackages of
    Left err -> do
      throwWithComment $ String.joinWith "\n"
        [ "Decoding registry file from " <> path <> "failed:"
        , err
        ]
    Right packages -> pure packages

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
