module Registry.App.Server.Scheduler
  ( runScheduler
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Registry.App.Auth as Auth
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry as Registry
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Server.Env (ServerEffects, ServerEnv, runEffects)
import Registry.Foreign.Octokit as Octokit
import Registry.Location (Location(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet(..))
import Run (Run)

-- | The scheduler loop runs immediately, then every 24 hours.
-- | It checks for work that needs to be enqueued (transfers, package set
-- | updates, legacy imports) and creates the appropriate jobs.
runScheduler :: ServerEnv -> Aff (Either Aff.Error Unit)
runScheduler env = runEffects env do
  Log.info "Starting Scheduler"
  loop
  where
  sleepTime = Milliseconds (1000.0 * 60.0 * 60.0 * 12.0)

  loop = do
    -- Run all scheduling checks
    scheduleTransfers
    schedulePackageSetUpdates
    scheduleLegacyImports
    Log.info "Scheduler cycle complete, sleeping for 24 hours..."
    -- Sleep for a while, then run again
    liftAff $ Aff.delay sleepTime
    loop

-- | Check for packages that have moved and enqueue transfer jobs.
scheduleTransfers :: Run ServerEffects Unit
scheduleTransfers = do
  Log.info "Scheduler: checking for package transfers..."
  allMetadata <- Registry.readAllMetadata

  -- Check each package for location changes
  transfersNeeded <- Array.catMaybes <$> for (Map.toUnfoldable allMetadata) \(Tuple name (Metadata metadata)) ->
    case metadata.location of
      Git _ -> pure Nothing -- Skip non-GitHub packages
      GitHub registered -> do
        -- Fetch tags to see if repo has moved
        GitHub.listTags { owner: registered.owner, repo: registered.repo } >>= case _ of
          Left _ -> pure Nothing -- Can't fetch tags, skip
          Right tags | Array.null tags -> pure Nothing -- No tags, skip
          Right tags -> case Array.head tags of
            Nothing -> pure Nothing
            Just tag ->
              -- Parse the tag URL to get actual current location
              case tagUrlToRepoUrl tag.url of
                Nothing -> pure Nothing
                Just actual
                  | locationsMatch registered actual -> pure Nothing -- No change
                  | otherwise -> pure $ Just { name, newLocation: GitHub { owner: actual.owner, repo: actual.repo, subdir: registered.subdir } }

  case Array.length transfersNeeded of
    0 -> Log.info "No packages require transferring."
    n -> do
      Log.info $ show n <> " packages need transferring"
      for_ transfersNeeded \{ name, newLocation } ->
        enqueueTransferJob name newLocation

-- | Parse GitHub API tag URL to extract owner/repo
-- | Example: https://api.github.com/repos/octocat/Hello-World/commits/abc123
tagUrlToRepoUrl :: String -> Maybe { owner :: String, repo :: String }
tagUrlToRepoUrl url = do
  noPrefix <- String.stripPrefix (String.Pattern "https://api.github.com/repos/") url
  case Array.take 2 $ String.split (String.Pattern "/") noPrefix of
    [ owner, repo ] -> Just { owner, repo: String.toLower repo }
    _ -> Nothing

-- | Case-insensitive comparison of GitHub locations
locationsMatch :: forall r. { owner :: String, repo :: String | r } -> { owner :: String, repo :: String } -> Boolean
locationsMatch loc1 loc2 =
  String.toLower loc1.owner == String.toLower loc2.owner
    && String.toLower loc1.repo
    == String.toLower loc2.repo

enqueueTransferJob :: PackageName -> Location -> Run ServerEffects Unit
enqueueTransferJob name newLocation = do
  -- Check if transfer job already exists
  existingJob <- Db.selectTransferJob name
  case existingJob of
    Just _ -> Log.debug $ "Transfer job already exists for " <> PackageName.print name
    Nothing -> do
      let payload = { name, newLocation }
      let rawPayload = stringifyJson Operation.transferCodec payload
      { privateKey } <- Env.askPacchettiBotti
      case Auth.signPayload { privateKey, rawPayload } of
        Left _ -> Log.error $ "Failed to sign transfer for " <> PackageName.print name
        Right signature -> do
          jobId <- Db.insertTransferJob { payload, rawPayload, signature }
          Log.info $ "Enqueued transfer job " <> unwrap jobId <> " for " <> PackageName.print name

-- | Check for recent uploads and enqueue package set update job.
schedulePackageSetUpdates :: Run ServerEffects Unit
schedulePackageSetUpdates = do
  Log.info "Scheduler: checking for package set updates..."

  -- Get the current package set
  latestPackageSet <- Registry.readLatestPackageSet >>= case _ of
    Nothing -> do
      Log.warn "No package set found, skipping package set updates"
      pure Nothing
    Just set -> pure (Just set)

  for_ latestPackageSet \packageSet -> do
    let currentPackages = (un PackageSet packageSet).packages

    -- Find packages uploaded in the last 24 hours that aren't already in the set
    recentUploads <- findRecentUploads (Hours 24.0)
    let
      -- Filter out packages already in the set at the same or newer version
      newOrUpdated = recentUploads # Map.filterWithKey \name version ->
        case Map.lookup name currentPackages of
          -- new package goes in
          Nothing -> true
          -- as do existing packages with a newer version
          Just currentVersion -> version > currentVersion

    if Map.isEmpty newOrUpdated then
      Log.info "No new packages for package set update."
    else do
      Log.info $ "Found " <> show (Map.size newOrUpdated) <> " candidates to validate"

      -- Pre-validate candidates to filter out packages with missing dependencies
      manifestIndex <- Registry.readAllManifests
      let candidates = PackageSets.validatePackageSetCandidates manifestIndex packageSet (map Just newOrUpdated)

      unless (Map.isEmpty candidates.rejected) do
        Log.info $ "Some packages are not eligible for the package set:\n" <> PackageSets.printRejections candidates.rejected

      -- Only enqueue accepted packages (filter out removals, keep only updates)
      let accepted = Map.catMaybes candidates.accepted

      if Map.isEmpty accepted then
        Log.info "No packages passed validation for package set update."
      else do
        Log.info $ "Validated " <> show (Map.size accepted) <> " packages for package set update"

        -- Create a package set update payload with only validated packages
        let
          payload = Operation.PackageSetUpdate
            { compiler: Nothing -- Use current compiler
            , packages: map Just accepted -- Just version = add/update
            }
          rawPayload = stringifyJson Operation.packageSetOperationCodec payload

        -- Check if a similar job already exists
        existingJob <- Db.selectPackageSetJobByPayload payload
        case existingJob of
          Just _ -> Log.debug "Package set job with same payload already exists"
          Nothing -> do
            -- No signature needed for package additions (only for compiler upgrades)
            jobId <- Db.insertPackageSetJob { payload, rawPayload, signature: Nothing }
            Log.info $ "Enqueued package set job " <> unwrap jobId

-- | Find the latest version of each package uploaded within the time limit
findRecentUploads :: Hours -> Run ServerEffects (Map PackageName Version)
findRecentUploads limit = do
  allMetadata <- Registry.readAllMetadata
  now <- nowUTC

  let
    getLatestRecentVersion :: Metadata -> Maybe Version
    getLatestRecentVersion (Metadata metadata) =
      let
        recentVersions = Array.catMaybes $ flip map (Map.toUnfoldable metadata.published) \(Tuple version { publishedTime }) ->
          let
            diff = DateTime.diff now publishedTime
          in
            if diff <= limit then Just version else Nothing
      in
        Array.last $ Array.sort recentVersions

  pure $ Map.fromFoldable $ Array.catMaybes $ flip map (Map.toUnfoldable allMetadata) \(Tuple name metadata) ->
    map (Tuple name) $ getLatestRecentVersion metadata

-- | Check for new tags on existing packages and enqueue publish jobs for
-- | versions not yet published. This allows the registry to automatically
-- | import new versions of packages that only have legacy manifests.
scheduleLegacyImports :: Run ServerEffects Unit
scheduleLegacyImports = do
  Log.info "Scheduler: checking for new package versions..."

  allMetadata <- Registry.readAllMetadata
  let packages = Map.toUnfoldable allMetadata :: Array (Tuple PackageName Metadata)

  for_ packages \(Tuple name (Metadata metadata)) -> do
    case metadata.location of
      Git _ -> pure unit -- Skip non-GitHub packages for now
      GitHub { owner, repo } -> do
        GitHub.listTags { owner, repo } >>= case _ of
          Left err -> do
            Log.debug $ "Failed to fetch tags for " <> PackageName.print name <> ": " <> Octokit.printGitHubError err
          Right tags -> do
            let
              -- Combine published and unpublished versions into a set
              publishedVersions = Set.fromFoldable
                $ Map.keys metadata.published
                <> Map.keys metadata.unpublished

              -- Parse tags as versions and filter out already published ones
              newVersions = Array.catMaybes $ tags <#> \tag ->
                case LenientVersion.parse tag.name of
                  Left _ -> Nothing -- Not a valid version tag
                  Right result ->
                    let
                      version = LenientVersion.version result
                    in
                      if Set.member version publishedVersions then Nothing -- Already published
                      else Just { version, ref: tag.name }

            for_ newVersions \{ version, ref } ->
              enqueuePublishJob name (Metadata metadata) version ref

    -- Delay between packages to spread GitHub API load
    liftAff $ Aff.delay (Milliseconds 500.0)

-- | Enqueue a publish job for a new package version discovered by the scheduler.
-- | Uses the lowest compiler from the previous published version for compatibility,
-- | falling back to the latest compiler if no previous version exists.
enqueuePublishJob :: PackageName -> Metadata -> Version -> String -> Run ServerEffects Unit
enqueuePublishJob name (Metadata metadata) version ref = do
  -- Check if a publish job already exists for this package version
  existingJob <- Db.selectPublishJob name version
  case existingJob of
    Just _ -> Log.debug $ "Publish job already exists for " <> formatPackageVersion name version
    Nothing -> do
      -- Use the lowest compiler from previous version for compatibility,
      -- falling back to latest if no previous version exists
      compiler <- case Map.findMax metadata.published of
        Just { value: publishedInfo } ->
          pure $ NonEmptyArray.head publishedInfo.compilers
        Nothing -> NonEmptyArray.last <$> PursVersions.pursVersions
      let
        payload =
          { name
          , location: Just metadata.location
          , ref
          , version
          , compiler
          , resolutions: Nothing
          }
      jobId <- Db.insertPublishJob { payload }
      Log.info $ "Enqueued legacy publish job " <> unwrap jobId <> " for " <> formatPackageVersion name version
