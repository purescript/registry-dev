-- | This script checks for packages recently uploaded to the registry and
-- | submits package set update jobs to add them to the package set.
-- |
-- | Run via Nix:
-- |   nix run .#package-set-updater -- --dry-run   # Log what would be submitted
-- |   nix run .#package-set-updater -- --submit    # Actually submit to the API
-- |
-- | Required environment variables:
-- |   GITHUB_TOKEN - GitHub API token
-- |   REGISTRY_API_URL - Registry API URL (default: https://registry.purescript.org)
module Registry.Scripts.PackageSetUpdater where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.DateTime as DateTime
import Data.Map as Map
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
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.Foreign.Octokit as Octokit
import Registry.Operation (PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet(..))
import Registry.Version as Version
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

  let description = "Check for recent uploads and submit package set update jobs to the registry API."
  mode <- case Arg.parseArgs "package-set-updater" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right command -> pure command

  Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv
  token <- Env.lookupRequired Env.githubToken

  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef
  let cache = Path.concat [ scratchDir, ".cache" ]

  octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl
  debouncer <- Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  runPackageSetUpdater mode resourceEnv.registryApiUrl
    # Except.runExcept
    # Registry.interpret (Registry.handle registryEnv)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Log.interpret (Log.handleTerminal Normal)
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'
    >>= case _ of
      Left err -> do
        Console.error $ "Error: " <> err
        liftEffect $ Process.exit' 1
      Right _ -> pure unit

type PackageSetUpdaterEffects = (REGISTRY + GITHUB + LOG + RESOURCE_ENV + EXCEPT String + AFF + EFFECT + ())

runPackageSetUpdater :: Mode -> URL -> Run PackageSetUpdaterEffects Unit
runPackageSetUpdater mode registryApiUrl = do
  Log.info "Package Set Updater: checking for recent uploads..."

  -- Get the current package set
  latestPackageSet <- Registry.readLatestPackageSet >>= case _ of
    Nothing -> do
      Log.warn "No package set found, skipping package set updates"
      pure Nothing
    Just set -> pure (Just set)

  for_ latestPackageSet \packageSet -> do
    let currentPackages = (un PackageSet packageSet).packages

    -- Find packages uploaded in the last 24 hours
    recentUploads <- findRecentUploads (Hours 24.0)
    let
      -- Filter out packages already in the set at the same or newer version
      newOrUpdated = recentUploads # Map.filterWithKey \name version ->
        case Map.lookup name currentPackages of
          Nothing -> true -- new package
          Just currentVersion -> version > currentVersion -- upgrade

    if Map.isEmpty newOrUpdated then
      Log.info "No new packages for package set update."
    else do
      Log.info $ "Found " <> show (Map.size newOrUpdated) <> " candidates to validate"

      -- Pre-validate candidates to filter out packages with missing dependencies
      manifestIndex <- Registry.readAllManifests
      let candidates = PackageSets.validatePackageSetCandidates manifestIndex packageSet (map Just newOrUpdated)

      unless (Map.isEmpty candidates.rejected) do
        Log.info $ "Some packages are not eligible for the package set:\n" <> PackageSets.printRejections candidates.rejected

      -- Only include accepted packages (filter out removals, keep only updates)
      let accepted = Map.catMaybes candidates.accepted

      if Map.isEmpty accepted then
        Log.info "No packages passed validation for package set update."
      else do
        Log.info $ "Validated " <> show (Map.size accepted) <> " packages for package set update"

        -- Create a package set update payload
        let
          payload :: Operation.PackageSetUpdateData
          payload =
            { compiler: Nothing -- Use current compiler
            , packages: map Just accepted -- Just version = add/update
            }

        case mode of
          DryRun -> do
            Log.info $ "[DRY RUN] Would submit package set update with packages:"
            for_ (Map.toUnfoldable accepted :: Array _) \(Tuple name version) ->
              Log.info $ "  - " <> PackageName.print name <> "@" <> Version.print version

          Submit -> do
            let
              rawPayload = JSON.print $ CJ.encode Operation.packageSetUpdateCodec payload
              request :: Operation.PackageSetUpdateRequest
              request =
                { payload: PackageSetUpdate payload
                , rawPayload
                , signature: Nothing
                }

            Log.info $ "Submitting package set update..."
            result <- Run.liftAff $ submitPackageSetJob (registryApiUrl <> "/v1/package-sets") request
            case result of
              Left err -> do
                Log.error $ "Failed to submit package set job: " <> err
              Right { jobId } -> do
                Log.info $ "Submitted package set job " <> unwrap jobId

-- | Find the latest version of each package uploaded within the time limit
findRecentUploads :: Hours -> Run PackageSetUpdaterEffects (Map PackageName Version)
findRecentUploads limit = do
  allMetadata <- Registry.readAllMetadata
  now <- nowUTC

  let
    getLatestRecentVersion :: Metadata -> Maybe Version
    getLatestRecentVersion (Metadata metadata) = do
      let
        recentVersions = Array.catMaybes $ flip map (Map.toUnfoldable metadata.published)
          \(Tuple version { publishedTime }) ->
            if (DateTime.diff now publishedTime) <= limit then Just version else Nothing
      Array.last $ Array.sort recentVersions

  pure $ Map.fromFoldable $ Array.catMaybes $ flip map (Map.toUnfoldable allMetadata) \(Tuple name metadata) ->
    map (Tuple name) $ getLatestRecentVersion metadata

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
