module Registry.Scripts.LegacyImport where

import Registry.Prelude

import Control.Alternative (guard)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception (throw)
import Foreign.GitHub (GitHubToken(..), Octokit)
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Node.Process as Node.Process
import Registry.API as API
import Registry.Cache (Cache)
import Registry.Cache as Cache
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
import Registry.Scripts.LegacyImport.Process (NameAddressOriginal, VersionOriginal)
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

  githubToken <- liftEffect do
    Node.Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  log "Creating an Octokit instance..."
  octokit <- liftEffect $ GitHub.mkOctokit githubToken

  log "Reading the cache..."
  cache <- Cache.useCache

  -- We rely on the existing registry index for two reasons. First, we write new
  -- entries to this location so they can be committed upsteram. Second, we use
  -- the registry index as a cache of manifests for packages that have already
  -- been uploaded. It is possible to do a dry-run without using the registry
  -- index as a cache by providing an empty map as `indexCache`. The files used
  -- to produce the manifests are still read from the GitHub cache.
  log "Fetching the registry index..."
  API.checkIndexExists
  indexCache <- Index.readRegistryIndex API.indexDir

  log "Starting import from legacy registries..."
  { registry, reservedNames } <- downloadLegacyRegistry octokit cache indexCache

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
    excludeVersion (Manifest manifestFields) = case PackageName.print manifestFields.name, Version.rawVersion manifestFields.version of
      -- [UNFIXABLE]
      -- These have no src directory.
      "concur-core", "v0.3.9" -> Nothing
      "concur-react", "v0.3.9" -> Nothing
      "pux-devtool", "v5.0.0" -> Nothing

      -- These have a malformed bower.json file
      "endpoints-express", "v0.0.1" -> Nothing

      _, _ -> Just manifestFields

    availablePackages = Array.mapMaybe excludeVersion sortedPackages

  log "Creating a Metadata Ref"
  packagesMetadataRef <- API.mkMetadataRef

  log "Starting upload..."
  runRegistryM (mkEnv githubToken cache packagesMetadataRef) do
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
      API.runOperation octokit addition

    log "Writing the registry-index on disk..."
    -- While insertions are usually done as part of the API operation, we don't
    -- yet commit and push to the registry-index repository. For that reason, we
    -- write to disk here and can manually commit the result upstream if desired.
    let packagesToIndex = Array.filter (not disabled) availablePackages
    void $ for packagesToIndex \manifest ->
      liftAff $ Index.insertManifest API.indexDir $ Manifest manifest

  log "Done!"

mkEnv :: GitHubToken -> Cache -> Ref (Map PackageName Metadata) -> Env
mkEnv githubToken cache packagesMetadata =
  { comment: \err -> error err
  , closeIssue: log "Skipping GitHub issue closing, we're running locally.."
  , commitToTrunk: \_ _ -> do
      log "Skipping committing to trunk.."
      pure (Right unit)
  , uploadPackage: Upload.upload
  , deletePackage: Upload.delete
  , packagesMetadata
  , cache
  , githubToken
  }

downloadLegacyRegistry :: Octokit -> Cache -> RegistryIndex -> Aff { registry :: RegistryIndex, reservedNames :: Map PackageName Location }
downloadLegacyRegistry octokit cache registryIndexCache = do
  bowerPackages <- readRegistryFile "bower-packages.json"
  newPackages <- readRegistryFile "new-packages.json"

  let
    allPackages = Map.union bowerPackages newPackages
    initialPackages = { failures: PackageFailures Map.empty, packages: allPackages }

  log "Checking rate limit status..."
  Except.runExceptT (GitHub.getRateLimit octokit) >>= case _ of
    Left err -> log $ GitHub.printGitHubError err
    Right rateLimit -> GitHub.printRateLimit rateLimit

  log "Fetching package releases..."
  releaseIndex <- Process.forPackage initialPackages \name repoUrl -> do
    address <- case GitHub.parseRepo repoUrl of
      Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
      Right address -> pure address

    let mkError = ResourceError <<< { resource: APIResource GitHubReleases, error: _ }

    releases <- do
      result <- liftAff $ Except.runExceptT $ GitHub.listTags octokit cache address
      case result of
        Left err -> case err of
          GitHub.APIError apiError
            | apiError.statusCode >= 400 -> throwError (mkError $ BadStatus apiError.statusCode)
            -- Success statuses should have returned a success result.
            | otherwise -> do
                log "WARNING:\nReceived GitHubError with a status code <= 400"
                log $ GitHub.printGitHubError err
                throwError (mkError BadRequest)
          GitHub.DecodeError str ->
            throwError (mkError $ DecodeError str)

        Right value -> pure value

    let versions = Map.fromFoldable $ map (\tag -> Tuple (RawVersion tag.name) unit) releases
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
      outerKey :: NameAddressOriginal
      outerKey = { name: packageName, original: name, address }

      innerKey :: VersionOriginal
      innerKey = { version: packageVersion, original: tag }

    pure $ Tuple outerKey innerKey

  log "Converting to manifests..."
  let forPackageRegistry = Process.forPackageVersion packageRegistry
  manifestRegistry <- forPackageRegistry \{ name, address } { version, original: originalVersion } _ -> do
    let
      nameVersion = PackageName.print name <> "--" <> Version.printVersion version
      key = "manifest__" <> nameVersion
    -- We attempt to pull the manifest from the registry index to avoid having
    -- to build a local cache ourselves.
    case Map.lookup name registryIndexCache >>= Map.lookup version of
      Just manifest -> pure manifest
      -- NOTE: We can't cache actual Version values as JSON because we will lose
      -- the raw version associated with them.
      Nothing -> liftEffect (Cache.readJsonEntry key cache) >>= case _ of
        Left _ -> Except.ExceptT do
          manifest <- Except.runExceptT do
            manifestFields <- Manifest.constructManifestFields octokit cache originalVersion address
            let repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
            let liftError = map (lmap ManifestImportError)
            Except.mapExceptT liftError $ Manifest.toManifest name repo version manifestFields
          let withRawVersion = map (un Manifest >>> (_ { version = originalVersion })) manifest
          liftEffect $ Cache.writeJsonEntry key withRawVersion cache
          pure manifest
        Right contents -> do
          fields <- Except.except contents.value
          let version' = unsafeFromRight $ Version.parseVersion Version.Lenient fields.version
          pure $ Manifest $ fields { version = version' }

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
