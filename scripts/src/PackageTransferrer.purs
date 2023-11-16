module Registry.Scripts.PackageTransferrer where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Effect.Ref as Ref
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Auth as Auth
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Types (RawPackageName(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (Tag)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Location as Location
import Registry.Operation (AuthenticatedPackageOperation(..))
import Registry.Operation as Operation
import Registry.Operation.Validation as Operation.Validation
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImporter as LegacyImporter
import Run (Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Run.Except as Run.Except

main :: Effect Unit
main = launchAff_ do

  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  resourceEnv <- Env.lookupResourceEnv

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  -- GitHub
  octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl

  -- Registry
  debouncer <- Registry.newDebouncer
  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { write: Registry.CommitAs (Git.pacchettibottiCommitter token)
      , pull: Git.ForceClean
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "package-transferrer-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  transfer
    # Registry.interpret (Registry.handle registryEnv)
    # Storage.interpret (Storage.handleReadOnly cache)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Except.catch (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Comment.interpret Comment.handleLog
    # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
    # Env.runPacchettiBottiEnv { privateKey, publicKey }
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'

transfer :: forall r. Run (API.AuthenticatedEffects + r) Unit
transfer = do
  Log.info "Processing legacy registry..."
  allMetadata <- Registry.readAllMetadata
  { bower, new } <- Registry.readLegacyRegistry
  let packages = Map.union bower new
  Log.info "Reading latest locations for legacy registry packages..."
  locations <- latestLocations allMetadata packages
  let needsTransfer = Map.catMaybes locations
  case Map.size needsTransfer of
    0 -> Log.info "No packages require transferring."
    n -> do
      Log.info $ Array.fold [ show n, " packages need transferring: ", printJson (CA.Common.strMap packageLocationsCodec) needsTransfer ]
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
    Left _ -> Except.throw $ "Could not transfer " <> rawPackageName <> " because it is not a valid package name."
    Right value -> pure value

  let
    payload = { name, newLocation }
    rawPayload = stringifyJson Operation.transferCodec payload

  { privateKey } <- Env.askPacchettiBotti

  signature <- case Auth.signPayload { privateKey, rawPayload } of
    Left _ -> Except.throw "Error signing transfer."
    Right signature -> pure signature

  API.authenticated
    { payload: Transfer payload
    , rawPayload
    , signature
    }

type PackageLocations =
  { registeredLocation :: Location
  , tagLocation :: Location
  }

packageLocationsCodec :: JsonCodec PackageLocations
packageLocationsCodec = CA.Record.object "PackageLocations"
  { registeredLocation: Location.codec
  , tagLocation: Location.codec
  }

latestLocations :: forall r. Map PackageName Metadata -> Map String String -> Run (REGISTRY + GITHUB + LOG + EXCEPT String + r) (Map String (Maybe PackageLocations))
latestLocations allMetadata packages = forWithIndex packages \package location -> do
  let rawName = RawPackageName (stripPureScriptPrefix package)
  Run.Except.runExceptAt LegacyImporter._exceptPackage (LegacyImporter.validatePackage rawName location) >>= case _ of
    Left { error: LegacyImporter.PackageURLRedirects { received, registered } } -> do
      let newLocation = GitHub { owner: received.owner, repo: received.repo, subdir: Nothing }
      Log.info $ "Package " <> package <> " has moved to " <> locationToPackageUrl newLocation
      if Operation.Validation.locationIsUnique newLocation allMetadata then do
        Log.info "New location is unique; package will be transferred."
        pure $ Just
          { registeredLocation: GitHub { owner: registered.owner, repo: registered.repo, subdir: Nothing }
          , tagLocation: newLocation
          }
      else do
        Log.info "Package will not be transferred! New location is already in use."
        pure Nothing
    Left _ -> pure Nothing
    Right packageResult | Array.null packageResult.tags -> pure Nothing
    Right packageResult -> do
      Registry.readMetadata packageResult.name >>= case _ of
        Nothing -> do
          Log.error $ "Cannot verify location of " <> PackageName.print packageResult.name <> " because it has no metadata."
          pure Nothing
        Just metadata -> case latestPackageLocations packageResult metadata of
          Left error -> do
            Log.warn $ "Could not verify location of " <> PackageName.print packageResult.name <> ": " <> error
            pure Nothing
          Right locations
            | locationsMatch locations.registeredLocation locations.tagLocation -> pure Nothing
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
  pure { registeredLocation: location, tagLocation }

locationToPackageUrl :: Location -> String
locationToPackageUrl = case _ of
  GitHub { owner, repo } ->
    Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
  Git _ ->
    unsafeCrashWith "Git urls cannot be registered."
