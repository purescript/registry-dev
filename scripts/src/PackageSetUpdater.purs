-- | This script plans a compile-verified package set upgrade and submits it
-- | to the registry API as one exact, atomic package set update.
-- |
-- | Run via Nix:
-- |   nix run .#package-set-updater -- --dry-run   # Log what would be submitted
-- |   nix run .#package-set-updater -- --submit    # Actually submit to the API
-- |
-- | Required environment variables:
-- |   REGISTRY_API_URL - Registry API URL (default: https://registry.purescript.org)
module Registry.Scripts.PackageSetUpdater where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Node.Path as Path
import Node.Process as Process
import Registry.API.V1 as V1
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (REGISTRY_READ)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.PackageSetPlanner as Planner
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata (Metadata(..))
import Registry.Operation (PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.PackageSet (PackageSet(..))
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

data Mode = DryRun | Submit

derive instance Eq Mode

parser :: ArgParser Mode
parser = Arg.choose "command"
  [ Arg.flag [ "dry-run" ]
      "Log what would be submitted without actually calling the API."
      $> DryRun
  , Arg.flag [ "submit" ]
      "Submit package set update jobs to the registry API."
      $> Submit
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "Plan a compile-verified package set upgrade and submit it to the registry API."
  mode <- case Arg.parseArgs "package-set-updater" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right command -> pure command

  Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv

  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  registryCacheRef <- Cache.newCacheRef

  debouncer <- Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { jobId: Nothing
      , pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  runPackageSetUpdater mode resourceEnv.registryApiUrl
    # PackageSets.interpret (PackageSets.handle { workdir: scratchDir })
    # Except.runExcept
    # Registry.interpretRead (Registry.handleRead registryEnv)
    # Storage.interpret (Storage.handleReadOnly cache)
    # Env.runResourceEnv resourceEnv
    # Log.interpret (Log.handleTerminal Normal)
    # Run.runBaseAff'
    >>= case _ of
      Left err -> do
        Console.error $ "Error: " <> err
        liftEffect $ Process.exit' 1
      Right _ -> pure unit

type PackageSetUpdaterEffects = (REGISTRY_READ + PACKAGE_SETS + STORAGE + RESOURCE_ENV + LOG + EXCEPT String + AFF + EFFECT + ())

runPackageSetUpdater :: Mode -> URL -> Run PackageSetUpdaterEffects Unit
runPackageSetUpdater mode registryApiUrl = do
  Log.info "Package Set Updater: planning package set upgrades..."

  Registry.readLatestPackageSet >>= case _ of
    Nothing -> Log.warn "No package set found, skipping package set updates."
    Just packageSet -> do
      now <- nowUTC
      metadata <- Registry.readAllMetadata
      manifests <- Registry.readAllManifests
      let additions = recentAdditionSeeds now (Hours 24.0) packageSet metadata
      let candidates = plannerCandidates packageSet metadata manifests additions
      if Map.isEmpty candidates then
        Log.info "No package set candidates have usable manifests; nothing to do."
      else do
        Log.info $ "Compile-planning upgrades for " <> show (Map.size candidates) <> " candidate packages..."
        plan <- Planner.planUpgrades Planner.probeAtomic packageSet manifests (Map.keys additions) candidates
        when plan.truncated do
          Log.warn "The planner exhausted its probe budget. The verified payload below still compiled exactly, but it may not include every possible upgrade."
        if Map.isEmpty plan.verified then
          Log.info "No upgrades could be compile-verified; nothing to submit."
        else do
          let
            operation = PackageSetUpdate
              { compiler: Nothing
              , packages: map Just plan.verified
              }
            formatted = Map.toUnfoldable plan.verified <#> \(Tuple name version) ->
              "  - " <> formatPackageVersion name version
          case mode of
            DryRun ->
              Log.info $ "[DRY RUN] Would submit verified package set updates:\n" <> String.joinWith "\n" formatted
            Submit -> do
              let
                request :: Operation.PackageSetUpdateRequest
                request =
                  { payload: operation
                  , rawPayload: JSON.print $ CJ.encode Operation.packageSetOperationCodec operation
                  , signature: Nothing
                  }
              Log.info $ "Submitting verified package set updates:\n" <> String.joinWith "\n" formatted
              result <- Run.liftAff $ submitPackageSetJob (registryApiUrl <> "/v1/package-sets") request
              case result of
                Left err -> Except.throw $ "Failed to submit package set job: " <> err
                Right { jobId } -> do
                  Log.info $ "Submitted package set job " <> unwrap jobId <> "; waiting for publication..."
                  Run.liftAff (pollPackageSetJob registryApiUrl jobId) >>= case _ of
                    Left err -> Except.throw err
                    Right _ -> Log.notice $ "Package set job " <> unwrap jobId <> " completed successfully."

-- | Packages that are not in the package set but were uploaded within the
-- | window, at their latest published version. These seed addition
-- | candidates; packages already in the set are always candidates for every
-- | pending newer version, no matter when it was published.
recentAdditionSeeds :: DateTime -> Hours -> PackageSet -> Map PackageName Metadata -> Map PackageName Version
recentAdditionSeeds now limit (PackageSet packageSet) = Map.mapMaybeWithKey \name (Metadata metadata) -> do
  guard $ not $ Map.member name packageSet.packages
  let recent = Array.filter (\(Tuple _ { publishedTime }) -> between (Hours 0.0) limit (DateTime.diff now publishedTime)) (Map.toUnfoldable metadata.published)
  Array.last $ Array.sort $ map fst recent

-- | Assemble planner candidates: existing packages contribute every newer
-- | published release, and both existing upgrades and addition seeds are
-- | expanded with any dependency names absent from the current package set so
-- | the planner can consider a self-contained plan. Recency gates which
-- | additions are independently proposed (the seeds), not which dependencies
-- | may satisfy a candidate: a dependency required by an eligible candidate is
-- | pulled in no matter when it was published. The planner includes such
-- | support packages only at their latest version, and only in payloads whose
-- | roots actually require them. Versions without an indexed manifest are
-- | excluded because the planner cannot reason about their dependencies.
-- | Declared ranges and compiler publication metadata are deliberately not
-- | gates: the compiler decides what is compatible.
plannerCandidates :: PackageSet -> Map PackageName Metadata -> ManifestIndex -> Map PackageName Version -> Planner.Candidates
plannerCandidates packageSet@(PackageSet set) metadata manifests additionSeeds =
  Map.union existingCandidates (close seeds)
  where
  hasManifest name version = isJust $ ManifestIndex.lookup name version manifests

  existingCandidates = Planner.existingPackageCandidates packageSet metadata # Map.mapMaybeWithKey \name versions ->
    NonEmptySet.fromSet $ Set.filter (hasManifest name) $ NonEmptySet.toSet versions

  existingDependencies = Array.concatMap dependenciesOf (Map.toUnfoldable existingCandidates)

  seeds = additionSeeds # Map.mapMaybeWithKey \name version -> do
    guard $ not (Map.member name set.packages) && hasManifest name version
    pure $ NonEmptySet.singleton version

  close selected = do
    let dependencies = existingDependencies <> Array.concatMap dependenciesOf (Map.toUnfoldable selected)
    let missing = Array.filter (\name -> not (Map.member name set.packages) && not (Map.member name selected)) dependencies
    let next = Foldable.foldl addLatestVersion selected missing
    if next == selected then selected else close next

  dependenciesOf (Tuple name versions) = Array.fromFoldable versions # Array.concatMap \selected ->
    case ManifestIndex.lookup name selected manifests of
      Nothing -> []
      Just (Manifest manifest) -> Array.fromFoldable $ Map.keys manifest.dependencies

  addLatestVersion selected name = fromMaybe selected do
    Metadata packageMetadata <- Map.lookup name metadata
    latest <- Set.findMax $ Set.filter (hasManifest name) $ Map.keys packageMetadata.published
    pure $ Map.insert name (NonEmptySet.singleton latest) selected

-- | Submit a package set job to the registry API
submitPackageSetJob :: String -> Operation.PackageSetUpdateRequest -> Aff (Either String V1.JobCreatedResponse)
submitPackageSetJob url request = do
  let body = JSON.print $ CJ.encode Operation.packageSetUpdateRequestCodec request
  result <- Aff.attempt $ Fetch.fetch url
    { method: POST
    , headers: { "Content-Type": "application/json" }
    , body
    }
  case result of
    Left err -> pure $ Left $ "Network error: " <> Aff.message err
    Right response -> do
      responseBody <- response.text
      if response.status >= 200 && response.status < 300 then
        case JSON.parse responseBody >>= \json -> lmap CJ.DecodeError.print (CJ.decode V1.jobCreatedResponseCodec json) of
          Left err -> pure $ Left $ "Failed to parse response: " <> err
          Right r -> pure $ Right r
      else
        pure $ Left $ "HTTP " <> show response.status <> ": " <> responseBody

-- | Wait for the server-side exact recompile and publication to finish. The
-- | script must not report success merely because a durable job was queued,
-- | and the server allows jobs up to 90 minutes, so we poll a little longer
-- | than that.
pollPackageSetJob :: String -> V1.JobId -> Aff (Either String Unit)
pollPackageSetJob registryApiUrl jobId = go 1 0
  where
  maxAttempts = 1200
  maxConsecutiveErrors = 5
  interval = Aff.Milliseconds 5_000.0
  url = registryApiUrl <> "/v1/jobs/" <> unwrap jobId <> "?level=NOTICE"

  go attempt consecutiveErrors
    | attempt > maxAttempts =
        pure $ Left $ "Package set job " <> unwrap jobId <> " did not finish within 100 minutes."
    | otherwise = do
        result <- fetchPackageSetJob url
        case result of
          Left error
            | consecutiveErrors + 1 >= maxConsecutiveErrors ->
                pure $ Left $ "Could not poll package set job " <> unwrap jobId <> " after " <> show maxConsecutiveErrors <> " consecutive errors: " <> error
            | otherwise -> do
                Aff.delay interval
                go (attempt + 1) (consecutiveErrors + 1)
          Right job -> do
            let info = V1.jobInfo job
            case info.finishedAt of
              Nothing -> do
                Aff.delay interval
                go (attempt + 1) 0
              Just _ | info.success ->
                pure $ Right unit
              Just _ -> do
                let logs = Foldable.foldMap (\line -> "\n" <> line.message) info.logs
                pure $ Left $ "Package set job " <> unwrap jobId <> " failed:" <> logs

fetchPackageSetJob :: String -> Aff (Either String V1.Job)
fetchPackageSetJob url = do
  result <- Aff.attempt $ Fetch.fetch url { method: GET }
  case result of
    Left err -> pure $ Left $ "Network error: " <> Aff.message err
    Right response -> do
      responseBody <- response.text
      if response.status >= 200 && response.status < 300 then
        case JSON.parse responseBody >>= \json -> lmap CJ.DecodeError.print (CJ.decode V1.jobCodec json) of
          Left err -> pure $ Left $ "Failed to parse package set job: " <> err
          Right job -> pure $ Right job
      else
        pure $ Left $ "HTTP " <> show response.status <> ": " <> responseBody
