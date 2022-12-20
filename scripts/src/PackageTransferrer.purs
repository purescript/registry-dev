module Registry.Scripts.PackageTransferrer where

import Registry.App.Prelude

import Data.Array as Array
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT, LogVerbosity(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify (Notify(..))
import Registry.App.Effect.Notify as Notify
import Registry.App.Effect.Registry (PullMode(..), REGISTRY, RegistryEnv, WriteStrategy(..))
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Types (RawPackageName(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (IssueNumber(..), Tag)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Operation (AuthenticatedPackageOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImporter as LegacyImporter
import Run (Run)
import Run as Run
import Run.Except as Run.Except
import Run.Reader as Run.Reader

main :: Effect Unit
main = launchAff_ do
  _ <- Env.loadEnvFile ".env"
  env <- liftEffect Env.readEnvVars

  FS.Extra.ensureDirectory scratchDir

  token <- case env.pacchettibottiToken of
    Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_TOKEN not defined in the environment."
    Just token -> pure token

  publicKey <- case env.pacchettibottiED25519Pub of
    Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_ED25519_PUB not defined in the environment."
    Just key -> pure key

  privateKey <- case env.pacchettibottiED25519 of
    Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_ED25519 not defined in the environment."
    Just key -> pure key

  octokit <- liftEffect $ Octokit.newOctokit token

  let
    registryEnv :: RegistryEnv
    registryEnv =
      { legacyPackageSets: Path.concat [ scratchDir, "package-sets" ]
      , registry: Path.concat [ scratchDir, "registry" ]
      , registryIndex: Path.concat [ scratchDir, "registry-index" ]
      , pullMode: Autostash
      -- Use Write if you do not want this to actually commit and push.
      , writeStrategy: Write -- CommitPush token
      , timer: unsafePerformEffect (Ref.new Nothing)
      }

  let cacheDir = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cacheDir
  cacheRef <- liftEffect $ Ref.new Map.empty

  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir

  now <- liftEffect nowUTC
  let logFile = "package-transferrer-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  transfer
    -- TODO: This environment is currently required for authenticated
    -- operations because we check whether the username is on the packaging
    -- team. However, that requirement should be dropped, and we can then
    -- remove this environment altogether.
    # Run.Reader.runReaderAt Env._githubEventEnv { issue: IssueNumber (-1), username: "" }
    # Run.Reader.runReaderAt Env._pacchettiBottiEnv { token, privateKey, publicKey }
    # Run.interpret (Run.on Registry._registry (Registry.handleRegistryGit registryEnv) Run.send)
    # Run.interpret (Run.on Storage._storage Storage.handleStorageReadOnly Run.send)
    # Run.interpret (Run.on GitHub._github (GitHub.handleGitHubOctokit octokit) Run.send)
    # Run.interpret (Run.on Cache._appCache (Cache.handleAppCacheFs cacheDir cacheRef) Run.send)
    # Run.interpret (Run.on Notify._notify (\(Notify msg next) -> Log.info msg *> pure next) Run.send)
    # Run.Except.catchAt Log._logExcept (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Run.interpret (Run.on Log._log (\log -> Log.handleLogTerminal Normal log *> Log.handleLogFs Verbose logPath log) Run.send)
    # Run.runBaseAff'

transfer :: forall r. Run (API.AuthenticatedEffects + r) Unit
transfer = do
  Log.info "Processing legacy registry..."
  { bower, new } <- Registry.readLegacyRegistry
  let packages = Map.union bower new
  Log.info "Reading latest locations for legacy registry packages..."
  locations <- latestLocations packages
  let needsTransfer = Map.catMaybes locations
  case Map.size needsTransfer of
    0 -> Log.info "No packages require transferring."
    n -> do
      Log.info $ Array.fold [ show n, " packages need transferring." ]
      _ <- transferAll packages needsTransfer
      Log.info "Completed transfers!"

transferAll :: forall r. Map String String -> Map String PackageLocations -> Run (API.AuthenticatedEffects + r) (Map String String)
transferAll packages packageLocations = do
  packagesRef <- liftEffect (Ref.new packages)
  forWithIndex_ packageLocations \package locations -> do
    let newPackageLocation = locations.tagLocation
    transferPackage package newPackageLocation
    let url = locationToPackageUrl newPackageLocation
    liftEffect $ Ref.modify_ (Map.insert package url) packagesRef
  liftEffect $ Ref.read packagesRef

transferPackage :: forall r. String -> Location -> Run (API.AuthenticatedEffects + r) Unit
transferPackage rawPackageName newLocation = do
  name <- case PackageName.parse (stripPureScriptPrefix rawPackageName) of
    Left _ -> Log.exit $ "Could not transfer " <> rawPackageName <> " because it is not a valid package name."
    Right value -> pure value

  let
    payload = { name, newLocation }
    rawPayload = stringifyJson Operation.transferCodec payload

  API.authenticated
    { email: Env.pacchettibottiEmail
    , payload: Transfer payload
    , rawPayload
    , signature: [] -- The API will re-sign using @pacchettibotti credentials.
    }

type PackageLocations =
  { metadataLocation :: Location
  , tagLocation :: Location
  }

latestLocations :: forall r. Map String String -> Run (REGISTRY + GITHUB + LOG + LOG_EXCEPT + r) (Map String (Maybe PackageLocations))
latestLocations packages = forWithIndex packages \package location -> do
  let rawName = RawPackageName (stripPureScriptPrefix package)
  Run.Except.runExceptAt LegacyImporter._exceptPackage (LegacyImporter.validatePackage rawName location) >>= case _ of
    Left _ -> pure Nothing
    Right packageResult | Array.null packageResult.tags -> pure Nothing
    Right packageResult -> do
      Registry.readMetadata packageResult.name >>= case _ of
        Nothing -> do
          Log.error $ "No metadata exists for package " <> package
          Log.exit $ "Cannot verify location of " <> PackageName.print packageResult.name <> " because it has no metadata."
        Just metadata -> case latestPackageLocations packageResult metadata of
          Left error -> do
            Log.warn $ "Could not verify location of " <> PackageName.print packageResult.name <> ": " <> error
            pure Nothing
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

latestPackageLocations :: LegacyImporter.PackageResult -> Metadata -> Either String PackageLocations
latestPackageLocations package (Metadata { location, published }) = do
  let
    isMatchingTag :: Version -> Tag -> Boolean
    isMatchingTag version tag = fromMaybe false do
      tagVersion <- hush $ LenientVersion.parse tag.name
      pure $ version == LenientVersion.version tagVersion

  matchingTag <- do
    if Map.isEmpty published then do
      note "No repo tags exist" $ Array.head package.tags
    else do
      Tuple version _ <- note "No published versions" $ Array.last (Map.toUnfoldable published)
      note "No versions match repo tags" $ Array.find (isMatchingTag version) package.tags
  tagUrl <- note ("Could not parse tag url " <> matchingTag.url) $ LegacyImporter.tagUrlToRepoUrl matchingTag.url
  let tagLocation = GitHub { owner: tagUrl.owner, repo: tagUrl.repo, subdir: Nothing }
  pure { metadataLocation: location, tagLocation }

locationToPackageUrl :: Location -> String
locationToPackageUrl = case _ of
  GitHub { owner, repo } ->
    Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
  Git _ ->
    unsafeCrashWith "Git urls cannot be registered."
