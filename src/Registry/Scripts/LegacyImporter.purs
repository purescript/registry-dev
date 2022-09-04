-- | This script attempts to import all package versions for packages listed in
-- | the legacy registry files (ie. bower-packages.json and new-packages.json).
-- |
-- | It can be run in different modes depending on whether you want to generate
-- | the registry from scratch, including uploading packages to the backend or
-- | you just want to iteratively pick up new releases.
module Registry.Scripts.LegacyImporter where

import Registry.Prelude

import Control.Alternative (guard)
import Control.Monad.Except as Except
import Control.Monad.Reader (ask, asks)
import Data.Array as Array
import Data.Compactable (separate)
import Data.Filterable (partition)
import Data.Foldable as Foldable
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map as Map
import Data.Ordering (invert)
import Data.Set as Set
import Data.String as String
import Dotenv as Dotenv
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.Path as Path
import Node.Process as Node.Process
import Parsing as Parsing
import Registry.API (Source(..))
import Registry.API as API
import Registry.Cache as Cache
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json ((.:))
import Registry.Json as Json
import Registry.Legacy.Manifest (LegacyManifestError(..), LegacyManifestValidationError)
import Registry.Legacy.Manifest as Legacy.Manifest
import Registry.Legacy.Manifest as LegacyManifest
import Registry.PackageGraph as Graph
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageUpload as Upload
import Registry.RegistryM (RegistryM, commitMetadataFile, readPackagesMetadata, runRegistryM, throwWithComment)
import Registry.Schema (BuildPlan(..), Location(..), Manifest(..), Operation(..))
import Registry.Version (Version)
import Registry.Version as Version

data ImportMode = GenerateRegistry | UpdateRegistry

derive instance Eq ImportMode

main :: Effect Unit
main = launchAff_ do
  log "Reading .env file..."
  _ <- Dotenv.loadFile

  FS.Extra.ensureDirectory API.scratchDir

  log "Parsing CLI args..."
  mode <- liftEffect do
    args <- Array.drop 2 <$> Node.Process.argv
    case Array.uncons args of
      Nothing -> Exception.throw "Expected 'generate' or 'update', but received no arguments."
      Just { head, tail: [] } -> case head of
        "generate" -> pure GenerateRegistry
        "update" -> pure UpdateRegistry
        other -> Exception.throw $ "Expected 'generate' or 'update' but received: " <> other
      Just _ -> Exception.throw $ String.joinWith "\n"
        [ "Expected 'generate' or 'update', but received multiple arguments:"
        , String.joinWith " " args
        ]

  octokit <- liftEffect do
    token <- do
      result <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
      maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) result
    GitHub.mkOctokit token

  cache <- Cache.useCache

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env = case mode of
      UpdateRegistry ->
        { comment: \comment -> log ("[COMMENT] " <> comment)
        , closeIssue: log "Running locally, not closing issue..."
        , commitMetadataFile: API.pacchettiBottiPushToRegistryMetadata
        , commitIndexFile: API.pacchettiBottiPushToRegistryIndex
        , commitPackageSetFile: \_ _ _ -> log "Not committing package set in legacy import." $> Right unit
        , uploadPackage: Upload.upload
        , deletePackage: Upload.delete
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
        , uploadPackage: Upload.upload
        , deletePackage: Upload.delete
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
      -- To ensure the metadata and registry index are always in sync, we remove
      -- any entries from the registry index that don't have accompanying metadata
      metadata <- liftEffect $ Ref.read metadataRef
      registry <- liftAff $ Index.readRegistryIndex registryIndexPath
      let hasMetadata package version = API.isPackageVersionInMetadata package version metadata
      let mismatched = mapWithIndex (Map.filterKeys <<< not <<< hasMetadata) registry
      liftAff $ do
        void $ forWithIndex mismatched \package versions ->
          forWithIndex versions \version _ ->
            Index.deleteManifest registryIndexPath package version
        Index.readRegistryIndex registryIndexPath

    log "Reading legacy registry..."
    legacyRegistry <- liftAff readLegacyRegistryFiles

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
          let metadata = { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
          liftAff $ Json.writeJsonFile (API.metadataFile registryPath package) metadata
          liftEffect $ Ref.modify_ (Map.insert package metadata) metadataRef
          commitMetadataFile package >>= case _ of
            Left err -> throwWithComment err
            Right _ -> pure unit
        Just _ -> pure unit

    log "Sorting packages for upload..."
    let indexPackages = Graph.topologicalSort importedIndex.registryIndex
    metadataMap <- readPackagesMetadata

    let
      isPublished { name, version } =
        API.isPackageVersionInMetadata name version metadataMap

      notPublished =
        indexPackages # Array.filter \(Manifest manifest) -> not (isPublished manifest)

      mkOperation manifest = Addition
        { newPackageLocation: manifest.location
        , packageName: manifest.name
        , newRef: Version.rawVersion manifest.version
        , buildPlan: BuildPlan
            { compiler: unsafeFromRight $ Version.parseVersion Version.Strict "0.15.4"
            , resolutions: Nothing
            }
        }

    case notPublished of
      [] -> log "No packages to publish."
      manifests -> do
        let printPackage (Manifest { name, version }) = PackageName.print name <> "@" <> Version.printVersion version
        log "\n----------"
        log "AVAILABLE TO PUBLISH"
        log "----------"
        log $ "  " <> String.joinWith "\n  " (map printPackage manifests)

        let
          source = case mode of
            UpdateRegistry -> API
            GenerateRegistry -> Importer

        void $ for notPublished \(Manifest manifest) -> do
          log "\n----------"
          log "UPLOADING"
          log $ PackageName.print manifest.name <> "@" <> Version.printVersion manifest.version
          log $ show manifest.location
          log "----------"
          API.runOperation source (mkOperation manifest)

    when (mode == GenerateRegistry) do
      log "Regenerating registry index..."
      void $ for indexPackages (liftAff <<< Index.insertManifest registryIndexPath)

    log "Done!"

-- | Record all package failures to the 'package-failures.json' file.
writePackageFailures :: Map RawPackageName PackageValidationError -> Aff Unit
writePackageFailures =
  Json.writeJsonFile (Path.concat [ API.scratchDir, "package-failures.json" ])
    <<< map formatPackageValidationError

-- | Record all version failures to the 'version-failures.json' file.
writeVersionFailures :: Map RawPackageName (Map RawVersion VersionValidationError) -> Aff Unit
writeVersionFailures =
  Json.writeJsonFile (Path.concat [ API.scratchDir, "version-failures.json" ])
    <<< map (map formatVersionValidationError)

logImportStats :: LegacyRegistry -> ImportedIndex -> Aff Unit
logImportStats legacy = log <<< formatImportStats <<< calculateImportStats legacy

type ImportedIndex =
  { failedPackages :: Map RawPackageName PackageValidationError
  , failedVersions :: Map RawPackageName (Map RawVersion VersionValidationError)
  , reservedPackages :: Map PackageName Location
  , registryIndex :: RegistryIndex
  }

-- | Construct a valid registry index containing manifests for all packages from
-- | the legacy registry files. This function also collects import errors for
-- | packages and package versions and reports packages that are present in the
-- | legacy registry but not in the resulting registry.
importLegacyRegistry :: RegistryIndex -> LegacyRegistry -> RegistryM ImportedIndex
importLegacyRegistry existingRegistry legacyRegistry = do
  legacyPackageSets <- LegacyManifest.fetchLegacyPackageSets
  manifests <- forWithIndex legacyRegistry \name address ->
    Except.runExceptT (buildLegacyPackageManifests existingRegistry legacyPackageSets name address)

  let
    separatedPackages = separate manifests
    separatedVersions =
      separatedPackages.right # flip foldlWithIndex { left: Map.empty, right: Map.empty } \key acc next -> do
        let { left, right } = separate next
        { left: if Map.isEmpty left then acc.left else Map.insert key left acc.left
        , right: if Map.isEmpty right then acc.right else Map.insert key right acc.right
        }

    toValues :: forall k v. Map k v -> Array v
    toValues = Array.fromFoldable <<< Map.values

    -- The registry index produced by fetching manifests for packages listed in
    -- the legacy registry files. This is not an acceptable index because it
    -- doesn't verify that all dependencies are contained in the registry.
    rawLegacyIndex :: Map PackageName (Map Version Manifest)
    rawLegacyIndex = do
      let
        validManifests =
          Array.concatMap toValues (toValues separatedVersions.right)
        foldFn m manifest@(Manifest { name, version }) =
          Map.insertWith Map.union name (Map.singleton version manifest) m
      Array.foldl foldFn Map.empty validManifests

    -- A 'checked' index is one where we have verified that all dependencies
    -- are self-contained within the registry.
    checkedIndex :: Graph.CheckResult
    checkedIndex = Graph.checkRegistryIndex rawLegacyIndex

    -- The list of all packages that were present in the legacy registry files,
    -- but which have no versions present in the fully-imported registry. These
    -- packages still need to have empty metadata files written for them.
    reservedPackages :: Map PackageName Location
    reservedPackages =
      Map.fromFoldable $ Array.mapMaybe reserved $ Map.toUnfoldable legacyRegistry
      where
      reserved (Tuple (RawPackageName name) address) = do
        packageName <- hush $ PackageName.parse name
        guard $ isNothing $ Map.lookup packageName checkedIndex.index
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
          let package = RawPackageName $ PackageName.print fail.package
          let version = RawVersion $ Version.rawVersion fail.version
          let error = { error: UnregisteredDependencies fail.dependencies, reason: "Contains dependencies that are not registered." }
          Map.insertWith Map.union package (Map.singleton version error) acc
        dependencyFailures =
          Array.foldl foldFn Map.empty checkedIndex.unsatisfied
      Map.unionWith Map.union separatedVersions.left dependencyFailures

  pure
    { failedPackages: packageFailures
    , failedVersions: versionFailures
    , reservedPackages: reservedPackages
    , registryIndex: checkedIndex.index
    }

-- | Attempt to build valid manifests for all releases associated with the given
-- | legacy package. This will result in a package error if versions could not
-- | be fetched in the first place. Otherwise, it will produce errors for all
-- | versions that don't produce valid manifests, and manifests for all that do.
buildLegacyPackageManifests
  :: RegistryIndex
  -> LegacyManifest.LegacyPackageSetEntries
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
            legacyManifest <- LegacyManifest.fetchLegacyManifest packageSetDeps package.address (RawVersion tag.name)
            pure $ LegacyManifest.toManifest package.name version location legacyManifest

      case Map.lookup package.name existingRegistry >>= Map.lookup version of
        Just manifest -> pure manifest
        _ -> do
          let key = "manifest__" <> show package.name <> "--" <> tag.name
          liftEffect (Cache.readJsonEntry key cache) >>= case _ of
            -- We can't just write the version directly, as we need to preserve the _raw_ version.
            -- So we update the manifest fields before reading/writing the cache.
            Left _ -> ExceptT do
              log $ "CACHE MISS: Building manifest for " <> show package.name <> "@" <> tag.name
              manifest <- Except.runExceptT buildManifest
              liftEffect $ Cache.writeJsonEntry key (map (_ { version = Version.rawVersion version } <<< un Manifest) manifest) cache
              pure manifest
            Right contents -> do
              fields <- Except.except contents.value
              pure $ Manifest $ fields { version = unsafeFromRight $ Version.parseVersion Version.Lenient fields.version }

  manifests <- lift $ for package.tags \tag -> do
    manifest <- Except.runExceptT $ buildManifestForVersion tag
    pure (Tuple (RawVersion tag.name) manifest)

  pure $ Map.fromFoldable manifests

type VersionValidationError = { error :: VersionError, reason :: String }

-- | An error that affects a specific package version
data VersionError
  = InvalidTag GitHub.Tag
  | DisabledVersion
  | InvalidManifest LegacyManifestValidationError
  | UnregisteredDependencies (Array PackageName)

instance RegistryJson VersionError where
  encode = case _ of
    InvalidTag tag -> Json.encode { tag: "InvalidTag", value: tag }
    DisabledVersion -> Json.encode { tag: "DisabledVersion" }
    InvalidManifest err -> Json.encode { tag: "InvalidManifest", value: err }
    UnregisteredDependencies names -> Json.encode { tag: "UnregisteredDependencies", value: names }
  decode = Json.decode >=> \obj -> (obj .: "tag") >>= case _ of
    "InvalidTag" -> map InvalidTag $ obj .: "value"
    "DisabledVersion" -> pure DisabledVersion
    "InvalidManifest" -> map InvalidManifest $ obj .: "value"
    "UnregisteredDependencies" -> map UnregisteredDependencies $ obj .: "value"
    tag -> Left $ "Unexpected tag: " <> tag

validateVersionDisabled :: PackageName -> Version -> Either VersionValidationError Unit
validateVersionDisabled package version =
  case Map.lookup (Tuple package version) disabledPackageVersions of
    Nothing -> pure unit
    Just reason -> Left { error: DisabledVersion, reason }
  where
  disabledPackageVersions :: Map (Tuple PackageName Version) String
  disabledPackageVersions = Map.fromFoldable
    [ Tuple (disabled "concur-core" "v0.3.9") noSrcDirectory
    , Tuple (disabled "concur-react" "v0.3.9") noSrcDirectory
    , Tuple (disabled "pux-devtool" "v5.0.0") noSrcDirectory
    , Tuple (disabled "endpoints-express" "v0.0.1") noSrcDirectory
    ]
    where
    noSrcDirectory = "Does not contain a 'src' directory."
    disabled name rawVersion =
      Tuple (unsafeFromRight $ PackageName.parse name) (unsafeFromRight $ Version.parseVersion Version.Lenient rawVersion)

validateVersion :: GitHub.Tag -> Either VersionValidationError Version
validateVersion tag =
  Version.parseVersion Version.Lenient tag.name # lmap \parserError ->
    { error: InvalidTag tag
    , reason: Parsing.parseErrorMessage parserError
    }

type PackageValidationError = { error :: PackageError, reason :: String }

-- | An error that affects an entire package
data PackageError
  = InvalidPackageName
  | InvalidPackageURL GitHub.PackageURL
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

validatePackageAddress :: GitHub.PackageURL -> Either PackageValidationError GitHub.Address
validatePackageAddress packageUrl =
  GitHub.parseRepo packageUrl # lmap \parserError ->
    { error: InvalidPackageURL packageUrl
    , reason: Parsing.parseErrorMessage parserError
    }

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
    , reason: Parsing.parseErrorMessage parserError
    }

type JsonValidationError =
  { tag :: String
  , value :: Maybe String
  , reason :: String
  }

formatPackageValidationError :: PackageValidationError -> JsonValidationError
formatPackageValidationError { error, reason } = case error of
  InvalidPackageName ->
    { tag: "InvalidPackageName", value: Nothing, reason }
  InvalidPackageURL (GitHub.PackageURL url) ->
    { tag: "InvalidPackageURL", value: Just url, reason }
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

-- | Read the legacy registry files stored in the root of the registry-dev.
-- | Package names have their 'purescript-' prefix trimmed.
readLegacyRegistryFiles :: Aff LegacyRegistry
readLegacyRegistryFiles = do
  bowerPackages <- readLegacyRegistryFile "bower-packages.json"
  registryPackages <- readLegacyRegistryFile "new-packages.json"
  let allPackages = Map.union bowerPackages registryPackages
  let fixupNames = mapKeys (RawPackageName <<< stripPureScriptPrefix)
  pure $ fixupNames allPackages

readLegacyRegistryFile :: String -> Aff (Map String GitHub.PackageURL)
readLegacyRegistryFile sourceFile = do
  legacyPackages <- Json.readJsonFile sourceFile
  case legacyPackages of
    Left err -> do
      throwError $ Exception.error $ String.joinWith "\n"
        [ "Decoding registry file from " <> sourceFile <> "failed:"
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
    registryIndex =
      mapKeys (RawPackageName <<< PackageName.print)
        $ map (mapKeys (RawVersion <<< Version.rawVersion)) imported.registryIndex

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
