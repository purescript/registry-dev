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
import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
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
import Node.Path as Path
import Node.Process as Node.Process
import Registry.API as API
import Registry.Cache (Cache)
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
import Registry.RegistryM (readPackagesMetadata, runRegistryM)
import Registry.Schema (BuildPlan(..), Location(..), Manifest(..), Operation(..))
import Registry.Version (Version)
import Registry.Version as Version
import Text.Parsing.StringParser as Parser

data ImportMode = GenerateRegistry | UpdateRegistry

derive instance Eq ImportMode

main :: Effect Unit
main = launchAff_ do
  log "Reading .env file..."
  _ <- Dotenv.loadFile

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
        , uploadPackage: Upload.upload
        , deletePackage: Upload.delete
        , packagesMetadata: metadataRef
        , cache
        , octokit
        , registry: Path.concat [ "..", "registry" ]
        , registryIndex: Path.concat [ "..", "registry-index" ]
        }
      GenerateRegistry ->
        API.mkLocalEnv octokit cache metadataRef

  runRegistryM env do
    API.fetchRegistry
    API.fetchRegistryIndex
    API.fillMetadataRef

    registryIndexPath <- asks _.registryIndex
    existingRegistry <- do
      -- To ensure the metadata and registry index are always in sync, we remove
      -- any entries from the registry index that don't have accompanying metadata
      metadata <- liftEffect $ Ref.read metadataRef
      registry <- liftAff $ Index.readRegistryIndex registryIndexPath
      let hasMetadata package version = API.isPackageVersionInMetadata package version metadata
      -- TODO: Delete missing metadata.
      pure $ mapWithIndex (Map.filterKeys <<< hasMetadata) registry

    log "Reading legacy registry..."
    legacyRegistry <- liftAff readLegacyRegistryFiles

    log "Importing legacy registry packages..."
    importedIndex <- importLegacyRegistry legacyRegistry (Just existingRegistry)

    liftAff do
      logImportStats legacyRegistry importedIndex

      log "Writing package and version failures..."
      writePackageFailures importedIndex.failedPackages
      writeVersionFailures importedIndex.failedVersions

      log "Writing metadata for legacy packages that can't be registered..."
      void $ forWithIndex importedIndex.reservedPackages \package location -> do
        let metadata = { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
        Json.writeJsonFile (API.metadataFile API.registryPath package) metadata
        liftEffect $ Ref.modify_ (Map.insert package metadata) metadataRef

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
        -- TODO: Technically, we could produce build plans for legacy packages
        -- by generating resolutions via Spago or Bower. We'd have to guess at
        -- the compiler.
        , buildPlan: BuildPlan
            { compiler: unsafeFromRight $ Version.parseVersion Version.Strict "0.15.0"
            , resolutions: Map.empty
            }
        }

    case mode of
      -- Simulate the API pipeline by importing package versions that have been
      -- released since the last time the registry was updated. This function will
      -- use PacchettiBotti to commit changes upstream to both the registry and
      -- the registry index.
      --
      -- TODO: If we are able to construct a build plan, then we could open an
      -- issue rather than exercise the API directly, which would give us a more
      -- real-world look at how the registry works.
      UpdateRegistry -> do
        log "Executing API for new package versions..."
        -- Note that we don't need to take any action after each upload, as the
        -- upload process in 'update' mode already commits and pushes changes.
        void $ for notPublished \(Manifest manifest) -> runRegistryM env do
          log $ "REGISTERING " <> show manifest.name <> "@" <> show manifest.version <> " at " <> show manifest.location
          API.runOperation octokit (mkOperation manifest)

      -- Generate manifest and metadata files for all packages in the legacy
      -- registry that don't already have them. To generate the full index from
      -- scratch, delete the contents of the registry metadata directory.
      --
      -- This branch uploads any packages that are not already in the registry and
      -- writes the resulting metadata and manifest files on disk. It does not
      -- not commit changes: you are expected to commit those changes yourself.
      GenerateRegistry -> do
        log "Executing local API for new package versions..."
        void $ for notPublished \(Manifest manifest) -> runRegistryM env do
          log "\n\n--------------------"
          log $ "UPLOADING PACKAGE: " <> show manifest.name <> "@" <> show manifest.version <> " at " <> show manifest.location
          log "--------------------"
          API.runOperation octokit (mkOperation manifest)
        log "Writing registry index to disk..."
        void $ for indexPackages \manifest ->
          Index.insertManifest API.registryIndexPath manifest

-- | Record all package failures to the 'package-failures.json' file.
writePackageFailures :: Map RawPackageName PackageValidationError -> Aff Unit
writePackageFailures = Json.writeJsonFile "package-failures.json" <<< map formatPackageValidationError

-- | Record all version failures to the 'version-failures.json' file.
writeVersionFailures :: Map RawPackageName (Map RawVersion VersionValidationError) -> Aff Unit
writeVersionFailures = Json.writeJsonFile "version-failures.json" <<< map (map formatVersionValidationError)

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
importLegacyRegistry :: LegacyRegistry -> RegistryIndex -> RegistryM ImportedIndex
importLegacyRegistry legacyRegistry existingRegistry = do
  manifests <- forWithIndex legacyRegistry (buildLegacyPackageManifests existingRegistry)

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
  -> RawPackageName
  -> GitHub.PackageURL
  -> RegistryM (Either PackageValidationError (Map RawVersion (Either VersionValidationError Manifest)))
buildLegacyPackageManifests existingRegistry rawPackage rawUrl = do
  { cache, octokit } <- ask
  liftAff $ Except.runExceptT do
    package <- do
      name <- Except.except $ validatePackageName rawPackage
      Except.except $ validatePackageDisabled name
      address <- Except.except $ validatePackageAddress rawUrl
      tags <- ExceptT $ fetchPackageTags octokit cache address
      pure { name, address, tags }

    let
      location = GitHub { owner: package.address.owner, repo: package.address.repo, subdir: Nothing }
      buildManifestForVersion tag = Except.runExceptT do
        version <- Except.except $ validateVersion tag

        let
          buildManifest = do
            Except.except $ validateVersionDisabled package.name version
            let fetchManifestError = { error: CannotAccessLegacyManifest, reason: _ }
            legacyManifest <- Except.withExceptT fetchManifestError $ ExceptT do
              LegacyManifest.fetchLegacyManifest octokit cache package.address (RawVersion tag.name)
            let parseManifestError err = { error: InvalidManifest err, reason: "Legacy manifest could not be parsed." }
            parsedManifest <- Except.withExceptT parseManifestError $ Except.except do
              LegacyManifest.parseLegacyManifest package.name location version legacyManifest
            pure parsedManifest

        -- If an existing registry was provided, then we can look up the existing
        -- manifest rather than produce a new one.
        case Map.lookup package.name registry >>= Map.lookup version of
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

    manifests <- liftAff $ for package.tags \tag -> do
      manifest <- buildManifestForVersion tag
      pure (Tuple (RawVersion tag.name) manifest)

    pure $ Map.fromFoldable manifests

type VersionValidationError = { error :: VersionError, reason :: String }

-- | An error that affects a specific package version
data VersionError
  = InvalidTag GitHub.Tag
  | DisabledVersion
  | CannotAccessLegacyManifest
  | InvalidManifest (NonEmptyArray LegacyManifestValidationError)
  | UnregisteredDependencies (Array PackageName)

instance RegistryJson VersionError where
  encode = case _ of
    InvalidTag tag -> Json.encode { tag: "InvalidTag", value: tag }
    DisabledVersion -> Json.encode { tag: "DisabledVersion" }
    CannotAccessLegacyManifest -> Json.encode { tag: "CannotAccessLegacyManifest" }
    InvalidManifest errs -> Json.encode { tag: "InvalidManifest", value: errs }
    UnregisteredDependencies names -> Json.encode { tag: "UnregisteredDependencies", value: names }
  decode = Json.decode >=> \obj -> (obj .: "tag") >>= case _ of
    "InvalidTag" -> map InvalidTag $ obj .: "value"
    "DisabledVersion" -> pure DisabledVersion
    "CannotAccessLegacyManifest" -> pure CannotAccessLegacyManifest
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
    , reason: Parser.printParserError parserError
    }

type PackageValidationError = { error :: PackageError, reason :: String }

-- | An error that affects an entire package
data PackageError
  = InvalidPackageName
  | InvalidPackageURL GitHub.PackageURL
  | CannotAccessRepo GitHub.Address
  | DisabledPackage

derive instance Eq PackageError

fetchPackageTags :: GitHub.Octokit -> Cache -> GitHub.Address -> Aff (Either PackageValidationError (Array GitHub.Tag))
fetchPackageTags octokit cache address = do
  result <- liftAff $ Except.runExceptT $ GitHub.listTags octokit cache address
  case result of
    Left err -> case err of
      GitHub.APIError apiError | apiError.statusCode >= 400 -> do
        let error = CannotAccessRepo address
        let reason = "GitHub API error with status code " <> show apiError.statusCode
        pure $ Left { error, reason }
      _ ->
        liftEffect $ Exception.throw $ String.joinWith "\n"
          [ "Unexpected GitHub error with a status <= 400"
          , GitHub.printGitHubError err
          ]
    Right tags ->
      pure $ Right tags

validatePackageAddress :: GitHub.PackageURL -> Either PackageValidationError GitHub.Address
validatePackageAddress packageUrl =
  GitHub.parseRepo packageUrl # lmap \parserError ->
    { error: InvalidPackageURL packageUrl
    , reason: Parser.printParserError parserError
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
    , reason: Parser.printParserError parserError
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
  CannotAccessLegacyManifest ->
    { tag: "CannotAccessLegacyManifest", value: Nothing, reason }
  InvalidManifest errs -> do
    let errorValue = String.joinWith ", " $ map (Legacy.Manifest.printLegacyManifestError <<< _.error) $ NonEmptyArray.toArray errs
    { tag: "InvalidManifest", value: Just errorValue, reason }
  UnregisteredDependencies names -> do
    let errorValue = String.joinWith ", " $ map PackageName.print names
    { tag: "UnregisteredDependencies", value: Just errorValue, reason }

type LegacyRegistry = Map RawPackageName GitHub.PackageURL

-- | Read the legacy registry files stored in the root of the registry-dev.
-- | Package names have their 'purescript-' prefix trimmed.
readLegacyRegistryFiles :: Aff (Map RawPackageName GitHub.PackageURL)
readLegacyRegistryFiles = do
  bowerPackages <- readLegacyRegistryFile "bower-packages.json"
  registryPackages <- readLegacyRegistryFile "new-packages.json"
  let allPackages = Map.union bowerPackages registryPackages
  let stripPrefixes = mapKeys (RawPackageName <<< stripPureScriptPrefix)
  pure $ stripPrefixes allPackages
  where
  readLegacyRegistryFile :: String -> Aff (Map String GitHub.PackageURL)
  readLegacyRegistryFile sourceFile = do
    let path = Path.concat [ "..", sourceFile ]
    legacyPackages <- Json.readJsonFile path
    case legacyPackages of
      Left err -> do
        throwError $ Exception.error $ String.joinWith "\n"
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
  [ "\n----------\n"
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
  , "\n----------\n"
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
        $ Array.concatMap toKey
        $ Array.fromFoldable
        $ List.concatMap Map.values
        $ Map.values imported.failedVersions
      where
      toKey = _.error >>> case _ of
        InvalidTag _ -> [ "Invalid Tag" ]
        DisabledVersion -> [ "Disabled Version" ]
        CannotAccessLegacyManifest -> [ "Cannot Access Legacy Manifest" ]
        InvalidManifest errs -> map (\err -> "Invalid Manifest (" <> innerKey err <> ")") (NonEmptyArray.toArray errs)
        UnregisteredDependencies _ -> [ "Unregistered Dependencies" ]

      innerKey = _.error >>> case _ of
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
