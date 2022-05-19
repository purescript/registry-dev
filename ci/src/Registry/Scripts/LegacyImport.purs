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
    extraReserved :: Map PackageName Location
    extraReserved = Map.fromFoldable
      [ Tuple (mkName "bitstrings") (GitHub { owner: "ethul", repo: "purescript-bitstrings", subdir: Nothing })
      , Tuple (mkName "metadata") (GitHub { owner: "purescript", repo: "purescript-metadata", subdir: Nothing })
      , Tuple (mkName "google-apps") (GitHub { owner: "BBVA", repo: "purescript-google-apps", subdir: Nothing })
      , Tuple (mkName "grain") (GitHub { owner: "purescript-grain", repo: "purescript-grain", subdir: Nothing })
      , Tuple (mkName "graphics-vis") (GitHub { owner: "paf31", repo: "purescript-graphics-vis", subdir: Nothing })
      , Tuple (mkName "graphqlclient") (GitHub { owner: "purescript-graphqlclient", repo: "purescript-graphqlclient", subdir: Nothing })
      , Tuple (mkName "halogen-day-picker") (GitHub { owner: "rnons", repo: "purescript-halogen-day-picker", subdir: Nothing })
      , Tuple (mkName "halogen-echarts") (GitHub { owner: "slamdata", repo: "purescript-halogen-echarts", subdir: Nothing })
      , Tuple (mkName "hyper") (GitHub { owner: "purescript-hyper", repo: "hyper", subdir: Nothing })
      , Tuple (mkName "ifrit") (GitHub { owner: "KtorZPersonal", repo: "purescript-ifrit", subdir: Nothing })
      , Tuple (mkName "impur") (GitHub { owner: "RuneBlaze", repo: "purescript-impur", subdir: Nothing })

      -- This one is missing because it depends on `grain`, and can be re-enabled when that is
      -- fixed and working.
      , Tuple (mkName "grain-portal") (GitHub { owner: "purescript-grain", repo: "purescript-grain-portal", subdir: Nothing })
      ]
      where
      mkName = unsafeFromRight <<< PackageName.parse

    -- These package versions contain valid manifests, and other versions of the
    -- package are valid, but these are not. We manually exclude them from being
    -- uploaded.
    excludeVersion :: Manifest -> Maybe _
    excludeVersion (Manifest manifest) = case PackageName.print manifest.name, Version.printVersion manifest.version of
      -- [UNFIXABLE] This is a special package that should never be uploaded.
      "metadata", _ -> Nothing
      -- [UNFIXABLE] These have no src directory. We can improve our process
      -- by checking the src directory as part of the legacy import process
      -- instead of failing in the API pipeline, or we can just leave this
      -- manual exclusion in place.
      "bitstrings", "0.0.0" -> Nothing
      "concur-core", "0.3.9" -> Nothing
      "concur-react", "0.3.9" -> Nothing

      -- These packages are over the size limit -- sometimes as much as 7mb
      -- over! We should fix these packages by introducing usage of the 'files'
      -- key, and applying it retroactively to legacy packages so that they
      -- only get commonly-recognized files and directories.
      --
      -- This should be OK: legacy packages could only have sources in src.
      --
      -- Once reintroduced we'll have to re-run the importer tool, as lots of
      -- packages will have different contents once they've been imported this
      -- way.

      "concur-core", "0.2.0" -> Nothing -- closure-compiler dir for all concur
      "concur-core", "0.3.0" -> Nothing
      "concur-core", "0.3.1" -> Nothing
      "concur-core", "0.3.2" -> Nothing
      "concur-core", "0.3.3" -> Nothing
      "concur-core", "0.3.4" -> Nothing
      "concur-core", "0.3.5" -> Nothing
      "concur-core", "0.3.6" -> Nothing
      "concur-core", "0.3.7" -> Nothing
      "concur-core", "0.3.8" -> Nothing
      "concur-react", "0.2.0" -> Nothing
      "concur-react", "0.3.0" -> Nothing
      "concur-react", "0.3.1" -> Nothing
      "concur-react", "0.3.2" -> Nothing
      "concur-react", "0.3.3" -> Nothing
      "concur-react", "0.3.4" -> Nothing
      "concur-react", "0.3.5" -> Nothing
      "concur-react", "0.3.6" -> Nothing
      "concur-react", "0.3.7" -> Nothing
      "concur-react", "0.3.8" -> Nothing
      "concur-react", "0.4.0" -> Nothing
      "concur-react", "0.4.1" -> Nothing
      "concur-react", "0.4.2" -> Nothing

      "flame", _ | manifest.version >= unsafeFromRight (Version.parseVersion Version.Lenient "1.0.0") -> Nothing -- 0xamples
      "freedom", _ | manifest.version > unsafeFromRight (Version.parseVersion Version.Lenient "0.6.2") -> Nothing -- 0xamples
      "google-apps", _ -> Nothing -- generator dir
      "grain", _ -> Nothing -- examples dir
      "grain-portal", _ -> Nothing -- this is excluded because it depends on grain, the package is OK
      "graphics-vis", _ -> Nothing -- images dir
      "graphql-client", _ | manifest.version > unsafeFromRight (Version.parseVersion Version.Lenient "0.2.0") -> Nothing -- gen-schema-bundle, codegen/schema, e2e, etc.
      "graphqlclient", _ -> Nothing -- generator, generator-test, examples, examples-test, bin, dist, ...
      "halogen-day-picker", _ -> Nothing -- examples, docs
      "halogen-echarts", _ -> Nothing -- example, especially example/dist/echarts-all.js
      "halogen-storybook", _ | manifest.version > unsafeFromRight (Version.parseVersion Version.Lenient "0.2.0") -> Nothing -- docs, examples, esp. an errant yarn.lock file
      "html", _ | manifest.version > unsafeFromRight (Version.parseVersion Version.Lenient "0.8.0") -> Nothing -- docs, examples, wrapper
      "hyper", _ -> Nothing -- docs, examples
      "ifrit", _ -> Nothing -- dist, examples
      "impur", _ -> Nothing

      _, _ -> Just manifest

    packages :: Array _
    packages = Array.mapMaybe excludeVersion sortedPackages

  log "Creating a Metadata Ref"
  packagesMetadataRef <- API.mkMetadataRef

  log "Starting upload..."
  runRegistryM (mkEnv packagesMetadataRef) do
    log "Adding metadata for reserved package names"
    forWithIndex_ (Map.union extraReserved reservedNames) \package repo -> do
      let metadata = { location: repo, owners: Nothing, published: Map.empty, unpublished: Map.empty }
      liftAff $ Json.writeJsonFile (API.metadataFile package) metadata
      updatePackagesMetadata package metadata

    log "Filtering out packages we already uploaded"
    packagesMetadata <- readPackagesMetadata

    let
      wasPackageUploaded { name, version } = API.isPackageVersionInMetadata name version packagesMetadata
      packagesToUpload = Array.filter (not wasPackageUploaded) packages

    for_ packagesToUpload \manifest -> do
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
    allPackages = Map.delete (RawPackageName "metadata") $ Map.union bowerPackages newPackages
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
        Left err -> do
          let message = Aff.message err
          log message
          if String.contains (String.Pattern "HttpError: Not Found") message then
            throwError $ mkError $ BadStatus 404
          else
            throwError $ mkError $ DecodeError message
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
