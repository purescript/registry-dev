module Registry.Scripts.LegacyImport where

import Registry.Prelude

import Control.Alternative (guard)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Data.Time.Duration (Hours(..))
import Dotenv as Dotenv
import Effect.Aff as Aff
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Registry.API as API
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json (printJson)
import Registry.Json as Json
import Registry.PackageGraph as Graph
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageUpload as Upload
import Registry.RegistryM (Env, readPackagesMetadata, runRegistryM, updatePackagesMetadata)
import Registry.Schema (BuildPlan(..), Location(..), Manifest(..), Metadata, Operation(..))
import Registry.Scripts.LegacyImport.Error (APIResource(..), ImportError(..), ManifestError(..), PackageFailures(..), RemoteResource(..), RequestError(..))
import Registry.Scripts.LegacyImport.Manifest as Manifest
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Scripts.LegacyImport.Stats as Stats
import Registry.Types (RawPackageName(..), RawVersion(..))
import Registry.Version (ParseMode(..), Version)
import Registry.Version as Version
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as StringParser

-- | This main loop uploads legacy packages to the new Registry
-- | In order to do this, we:
-- | - get an index of the legacy packages
-- | - create a graph (a tree really) where a package is a node and dependencies are edges
-- | - topologically sort this graph so that packages with no dependencies are at the root
-- | - go through this list: if the package is in the registry index then skip, otherwise upload
main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  API.checkIndexExists

  log "Starting import from legacy registries..."
  { registry, reservedNames } <- downloadLegacyRegistry

  let
    sortedPackages :: Array Manifest
    sortedPackages = Graph.topologicalSort registry

    -- These packages have no usable versions, but do have valid manifests, and
    -- so they make it to the processed packages but cannot be uploaded. They
    -- fail either because all versions have no src directory, or are too large,
    -- or it's a special package.
    disabledPackages :: Map PackageName Location
    disabledPackages = Map.fromFoldable
      -- [UNFIXABLE] This is a special package that should never be uploaded.
      [ mkDisabled "purescript" "purescript-metadata"
      -- [UNFIXABLE] These packages have no version with a src directory
      , mkDisabled "ethul" "purescript-bitstrings"
      , mkDisabled "paulyoung" "purescript-purveyor"
      , mkDisabled "paulyoung" "purescript-styled-components"
      , mkDisabled "paulyoung" "purescript-styled-system"
      ]
      where
      mkDisabled owner repo =
        Tuple (unsafeFromRight $ PackageName.parse $ stripPureScriptPrefix repo) (GitHub { owner, repo, subdir: Nothing })

    -- These package versions contain valid manifests, and other versions of the
    -- package are valid, but these are not. We manually exclude them from being
    -- uploaded.
    excludeVersion :: Manifest -> Maybe _
    excludeVersion (Manifest manifestFields) = case PackageName.print manifestFields.name, Version.printVersion manifestFields.version of
      -- [UNFIXABLE] These have no src directory.
      "concur-core", "0.3.9" -> Nothing
      "concur-react", "0.3.9" -> Nothing
      "pux-devtool", "5.0.0" -> Nothing
      _, _ -> Just manifestFields

    availablePackages = Array.mapMaybe excludeVersion sortedPackages

  log "Creating a Metadata Ref"
  packagesMetadataRef <- API.mkMetadataRef

  log "Starting upload..."
  runRegistryM (mkEnv packagesMetadataRef) do
    log "Adding metadata for reserved package names"
    forWithIndex_ (Map.union disabledPackages reservedNames) \package repo -> do
      let metadata = { location: repo, owners: Nothing, published: Map.empty, unpublished: Map.empty }
      liftAff $ Json.writeJsonFile (API.metadataFile package) metadata
      updatePackagesMetadata package metadata

    log "Filtering out packages we already uploaded"
    packagesMetadata <- readPackagesMetadata

    let
      disabled { name } = isJust $ Map.lookup name disabledPackages
      wasPackageUploaded { name, version } = API.isPackageVersionInMetadata name version packagesMetadata
      packagesToUpload = Array.filter (\package -> not (wasPackageUploaded package || disabled package)) availablePackages

    log "Uploading packages to the registry backend..."
    -- We need to use `for` here instead of `for_`, because the `Foldable` class
    -- isn't stack-safe.
    void $ for packagesToUpload \manifest -> do
      let
        addition = Addition
          { newPackageLocation: manifest.location
          , newRef: Version.rawVersion manifest.version
          -- The build plan check is not used for legacy packages, so we provide
          -- a dummy value.
          , buildPlan: BuildPlan
              { compiler: unsafeFromRight $ Version.parseVersion Version.Strict "0.15.0"
              , resolutions: Map.empty
              }
          , packageName: manifest.name
          }
      log "\n\n----------------------------------------------------------------------"
      log $ "UPLOADING PACKAGE: " <> show manifest.name <> "@" <> show manifest.version <> " to " <> show manifest.location
      log "----------------------------------------------------------------------"
      API.runOperation addition

    log "Writing the registry-index on disk..."
    -- While insertions are usually done as part of the API operation, we don't
    -- yet commit and push to the registry-index repository. For that reason, we
    -- write to disk here and can manually commit the result upstream if desired.
    let packagesToIndex = Array.filter (not disabled) availablePackages
    void $ for packagesToIndex \manifest ->
      liftAff $ Index.insertManifest API.indexDir $ Manifest manifest

  log "Done!"

mkEnv :: Ref (Map PackageName Metadata) -> Env
mkEnv packagesMetadata =
  { comment: \err -> error err
  , closeIssue: log "Skipping GitHub issue closing, we're running locally.."
  , commitToTrunk: \_ _ -> do
      log "Skipping committing to trunk.."
      pure (Right unit)
  , uploadPackage: Upload.upload
  , deletePackage: Upload.delete
  , packagesMetadata
  }

downloadLegacyRegistry :: Aff { registry :: RegistryIndex, reservedNames :: Map PackageName Location }
downloadLegacyRegistry = do
  octokit <- liftEffect GitHub.mkOctokit
  bowerPackages <- readRegistryFile "bower-packages.json"
  newPackages <- readRegistryFile "new-packages.json"

  let
    allPackages = Map.union bowerPackages newPackages
    initialPackages = { failures: PackageFailures Map.empty, packages: allPackages }

  log "Fetching package releases..."
  releaseIndex <- Process.forPackage initialPackages \name repoUrl -> do
    address <- case GitHub.parseRepo repoUrl of
      Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
      Right address -> pure address

    let
      repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]
      mkError = ResourceError <<< { resource: APIResource GitHubReleases, error: _ }

    releases <- Process.withCache Process.jsonSerializer repoCache (Just $ Hours 24.0) do
      log $ "Fetching releases for package " <> un RawPackageName name
      result <- lift $ try $ GitHub.getReleases octokit address
      case result of
        Left err -> logShow err *> throwError (mkError $ DecodeError $ Aff.message err)
        Right v -> pure v

    versions <- case NEA.fromArray releases of
      Nothing -> throwError $ mkError $ DecodeError "No releases returned from the GitHub API."
      Just arr -> pure $ Map.fromFoldable $ map (\tag -> Tuple (RawVersion tag.name) unit) arr

    pure $ Tuple { name, address } versions

  log "Parsing names and versions..."
  packageRegistry <- Process.forPackageVersionKeys releaseIndex \{ name, address } tag -> do
    packageName <- case PackageName.parse $ un RawPackageName name of
      Left err ->
        throwError $ MalformedPackageName $ StringParser.printParserError err
      Right pname ->
        pure pname

    packageVersion <- case Version.parseVersion Lenient $ un RawVersion tag of
      Left _ ->
        throwError $ ManifestImportError $ NEA.singleton $ BadVersion $ un RawVersion tag
      Right version ->
        pure version

    let
      outerKey = { name: packageName, original: name, address }
      innerKey = { version: packageVersion, original: tag }

    pure $ Tuple outerKey innerKey

  log "Converting to manifests..."
  let forPackageRegistry = Process.forPackageVersion packageRegistry
  manifestRegistry <- forPackageRegistry \{ name, original: originalName, address } tag _ -> do
    manifestFields <- Manifest.constructManifestFields originalName tag.original address

    let
      repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
      liftError = map (lmap ManifestImportError)

    Except.mapExceptT liftError $ Manifest.toManifest name repo tag.version manifestFields

  log "Writing exclusions file..."
  Json.writeJsonFile "./bower-exclusions.json" manifestRegistry.failures
  Stats.logStats $ Stats.errorStats manifestRegistry

  let
    registryIndex :: RegistryIndex
    registryIndex = do
      let
        packageManifests :: Array (Tuple PackageName (Map Version Manifest))
        packageManifests =
          map (lmap _.name)
            $ map (map (Map.fromFoldable <<< map (lmap _.version) <<< (Map.toUnfoldable :: _ -> Array _)))
            $ Map.toUnfoldable manifestRegistry.packages

      Map.fromFoldable packageManifests

  log "Constructing self-contained registry index..."
  let { index: checkedIndex, unsatisfied } = Graph.checkRegistryIndex registryIndex

  unless (Array.null unsatisfied) do
    log "Writing unsatisfied dependencies file..."
    Json.writeJsonFile "./unsatisfied-dependencies.json" unsatisfied
    log (show (Array.length unsatisfied) <> " manifest entries with unsatisfied dependencies")

  let
    reservedNames :: Map PackageName Location
    reservedNames =
      Map.fromFoldable
        $ Array.mapMaybe
            ( \(Tuple (RawPackageName name) address) -> do
                parsedName <- hush $ PackageName.parse name
                -- We don't need to preserve any packages that made it through
                -- processing into the resulting registry index.
                guard $ isNothing $ Map.lookup parsedName registryIndex
                { owner, repo } <- hush $ GitHub.parseRepo address
                pure $ Tuple parsedName (GitHub { owner, repo, subdir: Nothing })
            )
        $ Map.toUnfoldable allPackages

  log $ "Reserved names (" <> show (Map.size reservedNames) <> "):\n" <> (printJson $ Array.fromFoldable $ Map.keys reservedNames)
  pure { registry: checkedIndex, reservedNames }

-- Packages can be specified either in 'package-name' format or
-- in owner/package-name format. This function ensures we don't pick
-- up owner names as part of package names.
--
-- Example:
-- https://github.com/newlandsvalley/purescript-abc-parser/blob/1.1.2/bower.json
cleanPackageName :: RawPackageName -> ExceptT ImportError Aff RawPackageName
cleanPackageName (RawPackageName name) = do
  let
    split = String.split (String.Pattern "/") <<< coerce
    strip = coerce <<< stripPureScriptPrefix

  map strip $ case split name of
    [ packageName ] -> pure packageName
    [ _owner, repo ] -> pure repo
    _ -> throwError $ MalformedPackageName name

-- | Read the list of packages in a registry file
readRegistryFile :: FilePath -> Aff (Map RawPackageName GitHub.PackageURL)
readRegistryFile source = do
  registryFile <- Json.readJsonFile ("../" <> source)
  case registryFile of
    Left err -> do
      let decodeError = "Decoding " <> source <> "failed with error:\n\n" <> err
      throwError $ Aff.error decodeError
    Right packages -> do
      let toPackagesArray = Object.toArrayWithKey \k -> Tuple (RawPackageName $ stripPureScriptPrefix k)
      pure $ Map.fromFoldable $ toPackagesArray packages
