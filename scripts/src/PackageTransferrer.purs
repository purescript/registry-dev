module Registry.Scripts.PackageTransferrer where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk)
import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Codec as Codec
import Data.Map as Map
import Data.String as String
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Unsafe as Effect.Unsafe
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..), IssueNumber(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Registry.App.API (LegacyRegistryFile(..), Source(..))
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.LenientVersion as LenientVersion
import Registry.App.Monad (class MonadRegistry, GitHubEnv, LocalEnv, readPackagesMetadata)
import Registry.App.Monad as App
import Registry.Effect.Log (LogVerbosity(..))
import Registry.Effect.Log as Log
import Registry.Operation (AuthenticatedPackageOperation(..), PackageOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImporter as LegacyImporter

main :: Effect Unit
main = launchAff_ do
  _ <- API.loadEnv

  FS.Extra.ensureDirectory API.scratchDir

  octokit <- liftEffect do
    mbToken <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
    token <- maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) mbToken
    GitHub.mkOctokit token

  cache <- Cache.useCache API.cacheDir

  let
    env :: LocalEnv
    env =
      { closeIssue: Console.log "Running locally, not closing issue..."
      , commitMetadataFile: API.pacchettiBottiPushToRegistryMetadata
      , commitIndexFile: \_ _ -> unsafeCrashWith "Should not push to registry index in transfer."
      , commitPackageSetFile: \_ _ -> unsafeCrashWith "Should not modify package set in transfer."
      , uploadPackage: \_ -> unsafeCrashWith "Should not upload anything in transfer."
      , deletePackage: \_ -> unsafeCrashWith "Should not delete anything in transfer."
      , packagesMetadata: Effect.Unsafe.unsafePerformEffect (Ref.new Map.empty)
      , cache
      , octokit
      , issue: IssueNumber 0
      , username: "pacchettibotti"
      , registry: Path.concat [ API.scratchDir, "registry" ]
      , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
      , logfile: Path.concat [ API.scratchDir, "package-transferrer-logs.txt" ]
      , verbosity: Verbose
      }

  App.runLocalM env do
    API.fetchRegistry
    API.fillMetadataRef
    for_ [ BowerPackages, NewPackages ] processLegacyRegistry
    Log.info "Done!"

processLegacyRegistry :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => LegacyRegistryFile -> m Unit
processLegacyRegistry legacyFile = do
  Log.debug $ Array.fold [ "Reading legacy registry file (", API.legacyRegistryFilePath legacyFile, ")" ]
  packages <- LegacyImporter.readLegacyRegistryFile legacyFile
  Log.debug "Reading latest locations..."
  locations <- latestLocations packages
  let needsTransfer = Map.catMaybes locations
  case Map.size needsTransfer of
    0 -> Log.debug "No packages require transferring."
    n -> Log.debug $ Array.fold [ show n, " packages need transferring." ]
  _ <- transferAll packages needsTransfer
  Log.debug "Completed transfers!"

transferAll :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => Map String GitHub.PackageURL -> Map String PackageLocations -> m (Map String GitHub.PackageURL)
transferAll packages packageLocations = do
  packagesRef <- liftEffect (Ref.new packages)
  forWithIndex_ packageLocations \package locations -> do
    let newPackageLocation = locations.tagLocation
    transferPackage package newPackageLocation
    let url = locationToPackageUrl newPackageLocation
    liftEffect $ Ref.modify_ (Map.insert package url) packagesRef
  liftEffect $ Ref.read packagesRef

transferPackage :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => String -> Location -> m Unit
transferPackage rawPackageName newLocation = do
  name <- case PackageName.parse (stripPureScriptPrefix rawPackageName) of
    Left _ -> do
      Log.error $ "Unexpected package name parsing failure for " <> rawPackageName
      liftEffect (Process.exit 1)
    Right value -> pure value

  let
    payload = { name, newLocation }
    rawPayload = Argonaut.stringify $ Codec.encode Operation.transferCodec payload

  API.runOperation Importer $ Right $ Authenticated
    { email: Git.pacchettiBottiEmail
    , payload: Transfer payload
    , rawPayload
    , signature: [] -- The API will re-sign using @pacchettibotti credentials.
    }

type PackageLocations =
  { metadataLocation :: Location
  , tagLocation :: Location
  }

latestLocations :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => Map String GitHub.PackageURL -> m (Map String (Maybe PackageLocations))
latestLocations packages = forWithIndex packages \package location -> do
  let rawName = RawPackageName (stripPureScriptPrefix package)
  Except.runExceptT (LegacyImporter.validatePackage rawName location) >>= case _ of
    Left _ -> pure Nothing
    Right packageResult | Array.null packageResult.tags -> pure Nothing
    Right packageResult -> do
      packagesMetadata <- readPackagesMetadata
      case Map.lookup packageResult.name packagesMetadata of
        Nothing -> do Log.error ("No metadata exists for package " <> package) *> pure Nothing
        Just metadata -> do
          Except.runExceptT (latestPackageLocations packageResult metadata) >>= case _ of
            Left err -> Log.debug err *> pure Nothing
            Right locations
              | locationsMatch locations.metadataLocation locations.tagLocation -> pure Nothing
              | otherwise -> pure $ Just locations
  where
  -- The eq instance for locations has case sensitivity, but GitHub doesn't care.
  locationsMatch :: Location -> Location -> Boolean
  locationsMatch (GitHub location1) (GitHub location2) =
    (String.toLower location1.repo == String.toLower location2.repo)
      && (String.toLower location1.owner == String.toLower location2.owner)
  locationsMatch _ _ =
    unsafeCrashWith "Only GitHub locations can be considered in legacy registries."

latestPackageLocations :: forall m. MonadRegistry m => LegacyImporter.PackageResult -> Metadata -> ExceptT String m PackageLocations
latestPackageLocations package (Metadata { location, published }) = do
  let
    isMatchingTag :: Version -> GitHub.Tag -> Boolean
    isMatchingTag version tag = fromMaybe false do
      tagVersion <- hush $ LenientVersion.parse tag.name
      pure $ version == LenientVersion.version tagVersion

    matchMetadata :: Either String PackageLocations
    matchMetadata = do
      matchingTag <- do
        if Map.isEmpty published then do
          note "No repo tags exist" $ Array.head package.tags
        else do
          Tuple version _ <- note "No published versions" $ Array.last (Map.toUnfoldable published)
          note "No versions match repo tags" $ Array.find (isMatchingTag version) package.tags
      tagUrl <- note ("Could not parse tag url " <> matchingTag.url) $ LegacyImporter.tagUrlToRepoUrl matchingTag.url
      let tagLocation = GitHub { owner: tagUrl.owner, repo: tagUrl.repo, subdir: Nothing }
      pure { metadataLocation: location, tagLocation }

  case matchMetadata of
    Left err -> throwError $ Array.fold
      [ PackageName.print package.name
      , " failed to match locations: "
      , err
      ]
    Right locations ->
      pure locations

locationToPackageUrl :: Location -> GitHub.PackageURL
locationToPackageUrl = case _ of
  GitHub { owner, repo } ->
    GitHub.PackageURL $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
  Git _ ->
    unsafeCrashWith "Git urls cannot be registered."
