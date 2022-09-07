module Registry.Scripts.PackageTransferrer where

import Registry.Prelude

import Affjax as Http
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Dotenv as Dotenv
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.Path as Path
import Node.Process as Node.Process
import Registry.API as API
import Registry.Cache as Cache
import Registry.Constants as Constants
import Registry.Json as Json
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM, readPackagesMetadata, throwWithComment)
import Registry.RegistryM as RegistryM
import Registry.Schema (AuthenticatedData(..), AuthenticatedOperation(..), Location(..), Metadata, Operation(..))
import Registry.Scripts.LegacyImporter as LegacyImporter
import Registry.Version (Version)
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile

  FS.Extra.ensureDirectory API.scratchDir

  octokit <- liftEffect do
    mbToken <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
    token <- maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) mbToken
    GitHub.mkOctokit token

  cache <- Cache.useCache

  let
    env :: RegistryM.Env
    env =
      { comment: \comment -> log ("[COMMENT] " <> comment)
      , closeIssue: log "Running locally, not closing issue..."
      , commitMetadataFile: API.pacchettiBottiPushToRegistryMetadata
      , commitIndexFile: \_ _ -> unsafeCrashWith "Should not push to registry index in transfer."
      , commitPackageSetFile: \_ _ -> unsafeCrashWith "Should not modify package set in transfer."
      , uploadPackage: \_ -> unsafeCrashWith "Should not upload anything in transfer."
      , deletePackage: \_ -> unsafeCrashWith "Should not delete anything in transfer."
      , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
      , cache
      , octokit
      , username: "pacchettibotti"
      , registry: Path.concat [ API.scratchDir, "registry" ]
      , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
      }

  RegistryM.runRegistryM env do
    API.fetchRegistry
    API.fillMetadataRef
    processLegacyRegistry "bower-packages.json"
    processLegacyRegistry "new-packages.json"
    log "Done!"

processLegacyRegistry :: FilePath -> RegistryM Unit
processLegacyRegistry legacyFile = do
  log $ Array.fold [ "Reading legacy registry file (", legacyFile, ")" ]
  packages <- liftAff $ LegacyImporter.readLegacyRegistryFile legacyFile
  log "Reading latest locations..."
  locations <- latestLocations packages
  let needsTransfer = Map.catMaybes locations
  case Map.size needsTransfer of
    0 -> log "No packages require transferring."
    n -> log $ Array.fold [ show n, " packages need transferring." ]
  newPackages <- transferAll packages needsTransfer
  log "Writing legacy registry file..."
  liftAff (writeLegacyRegistryFile legacyFile newPackages)
  liftAff (commitLegacyRegistryFile legacyFile) >>= case _ of
    Left err -> throwWithComment err
    Right _ -> pure unit

transferAll :: Map String GitHub.PackageURL -> Map String PackageLocations -> RegistryM (Map String GitHub.PackageURL)
transferAll packages packageLocations = do
  packagesRef <- liftEffect (Ref.new packages)
  forWithIndex_ packageLocations \package locations -> do
    let newPackageLocation = locations.tagLocation
    transferPackage package newPackageLocation
    let url = locationToPackageUrl newPackageLocation
    liftEffect $ Ref.modify_ (Map.insert package url) packagesRef
  liftEffect $ Ref.read packagesRef

transferPackage :: String -> Location -> RegistryM Unit
transferPackage rawPackageName newPackageLocation = do
  packageName <- case PackageName.parse (stripPureScriptPrefix rawPackageName) of
    Left _ -> throwWithComment $ "Unexpected package name parsing failure for " <> rawPackageName
    Right value -> pure value

  let
    payload = Transfer { packageName, newPackageLocation }
    rawPayload = Json.stringifyJson payload

  API.runOperation $ Authenticated $ AuthenticatedData
    { email: Git.pacchettiBottiEmail
    , payload
    , rawPayload
    , signature: [] -- The API will re-sign using @pacchettibotti credentials.
    }

type PackageLocations =
  { metadataLocation :: Location
  , tagLocation :: Location
  }

latestLocations :: Map String GitHub.PackageURL -> RegistryM (Map String (Maybe PackageLocations))
latestLocations packages = forWithIndex packages \package location -> do
  let rawName = RawPackageName (stripPureScriptPrefix package)
  Except.runExceptT (LegacyImporter.validatePackage rawName location) >>= case _ of
    Left _ -> pure Nothing
    Right packageResult | Array.null packageResult.tags -> pure Nothing
    Right packageResult -> do
      packagesMetadata <- readPackagesMetadata
      case Map.lookup packageResult.name packagesMetadata of
        Nothing -> throwWithComment $ "No metadata exists for package " <> package
        Just metadata -> do
          Except.runExceptT (latestPackageLocations packageResult metadata) >>= case _ of
            Left err -> log err *> pure Nothing
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

latestPackageLocations :: LegacyImporter.PackageResult -> Metadata -> ExceptT String RegistryM PackageLocations
latestPackageLocations package { location, published } = do
  let
    isMatchingTag :: Version -> GitHub.Tag -> Boolean
    isMatchingTag version tag = fromMaybe false do
      tagVersion <- hush $ Version.parseVersion Version.Lenient tag.name
      pure $ version == tagVersion

    matchMetadata :: Either String PackageLocations
    matchMetadata = do
      matchingTag <- do
        if Map.isEmpty published then do
          note "No repo tags exist" $ Array.head package.tags
        else do
          Tuple version _ <- note "No published versions" $ Array.last (Map.toUnfoldable published)
          note "No versions match repo tags" $ Array.find (isMatchingTag version) package.tags
      tagUrl <- note ("Could not parse tag url " <> matchingTag.url) $ tagUrlToRepoUrl matchingTag.url
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

-- Example tag url:
-- https://api.github.com/repos/octocat/Hello-World/commits/c5b97d5ae6c19d5c5df71a34c7fbeeda2479ccbc
tagUrlToRepoUrl :: Http.URL -> Maybe GitHub.Address
tagUrlToRepoUrl url = do
  noPrefix <- String.stripPrefix (String.Pattern "https://api.github.com/repos/") url
  let getOwnerRepoArray = Array.take 2 <<< String.split (String.Pattern "/")
  case getOwnerRepoArray noPrefix of
    [ owner, repo ] -> Just { owner, repo: String.toLower repo }
    _ -> Nothing

locationToPackageUrl :: Location -> GitHub.PackageURL
locationToPackageUrl = case _ of
  GitHub { owner, repo } ->
    GitHub.PackageURL $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
  Git _ ->
    unsafeCrashWith "Git urls cannot be registered."

writeLegacyRegistryFile :: FilePath -> Map String GitHub.PackageURL -> Aff Unit
writeLegacyRegistryFile sourceFile packages = Json.writeJsonFile sourceFile packages

commitLegacyRegistryFile :: FilePath -> Aff (Either String Unit)
commitLegacyRegistryFile sourceFile = Except.runExceptT do
  Git.runGitSilent [ "diff", "--stat" ] Nothing >>= case _ of
    files | String.contains (String.Pattern sourceFile) files -> do
      GitHubToken token <- Git.configurePacchettiBotti Nothing
      Git.runGit_ [ "pull" ] Nothing
      Git.runGit_ [ "add", sourceFile ] Nothing
      log "Committing to registry..."
      let message = Array.fold [ "Sort ", sourceFile, " and transfer packages that have moved repositories." ]
      Git.runGit_ [ "commit", "-m", message ] Nothing
      let upstreamRepo = Constants.registryDevRepo.owner <> "/" <> Constants.registryDevRepo.repo
      let origin = "https://pacchettibotti:" <> token <> "@github.com" <> upstreamRepo <> ".git"
      void $ Git.runGitSilent [ "push", origin, "master" ] Nothing
    _ ->
      log "No changes to commit."
