-- | This script checks for new package versions by fetching GitHub tags for all
-- | packages in the registry. When a new version is discovered (a tag that hasn't
-- | been published or unpublished), it submits a publish job to the registry API.
-- |
-- | Run via Nix:
-- |   nix run .#daily-importer -- --dry-run     # Log what would be submitted
-- |   nix run .#daily-importer -- --submit      # Actually submit to the API
-- |
-- | Required environment variables:
-- |   GITHUB_TOKEN - GitHub API token for fetching tags
-- |   REGISTRY_API_URL - Registry API URL (default: https://registry.purescript.org)
module Registry.Scripts.DailyImporter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON as CJ
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Node.Path as Path
import Node.Process as Process
import Registry.API.V1 as V1
import Registry.App.CLI.Git as Git
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.Foreign.Octokit as Octokit
import Registry.Location (Location(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Range as Range
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
      "Submit publish jobs to the registry API."
      $> Submit
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "Check for new package versions and submit publish jobs to the registry API."
  mode <- case Arg.parseArgs "daily-importer" description parser args of
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

  runDailyImport mode resourceEnv.registryApiUrl
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

type DailyImportEffects = (REGISTRY + GITHUB + LOG + RESOURCE_ENV + EXCEPT String + AFF + EFFECT + ())

runDailyImport :: Mode -> URL -> Run DailyImportEffects Unit
runDailyImport mode registryApiUrl = do
  Log.info "Daily Importer: checking for new package versions..."

  allMetadata <- Registry.readAllMetadata
  let packages = Map.toUnfoldable allMetadata :: Array (Tuple PackageName Metadata)

  Log.info $ "Checking " <> show (Array.length packages) <> " packages for new versions..."

  submitted <- for packages \(Tuple name (Metadata metadata)) -> do
    case metadata.location of
      Git _ -> pure 0 -- Skip non-GitHub packages for now
      GitHub { owner, repo } -> do
        GitHub.listTags { owner, repo } >>= case _ of
          Left err -> do
            Log.debug $ "Failed to fetch tags for " <> PackageName.print name <> ": " <> Octokit.printGitHubError err
            pure 0
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
                      if Set.member version publishedVersions then Nothing
                      else Just { version, ref: tag.name }

            -- Submit publish jobs for new versions
            count <- for newVersions \{ version, ref } -> do
              submitPublishJob mode registryApiUrl allMetadata name (Metadata metadata) version ref

            pure $ Array.length $ Array.filter identity count

  let totalSubmitted = Array.foldl (+) 0 submitted
  Log.info $ "Daily Importer complete. Submitted " <> show totalSubmitted <> " publish jobs."

-- | Submit a publish job for a new package version.
-- | Attempts to find a compatible compiler by looking at the previous version's
-- | dependencies. Falls back to the lowest compiler from the previous version if
-- | no dependencies exist, or to the latest compiler if no previous version exists.
submitPublishJob
  :: Mode
  -> URL
  -> Map PackageName Metadata
  -> PackageName
  -> Metadata
  -> Version
  -> String
  -> Run DailyImportEffects Boolean
submitPublishJob mode registryApiUrl allMetadata name (Metadata metadata) version ref = do
  let formatted = formatPackageVersion name version

  -- Determine the appropriate compiler version
  compiler <- case Map.findMax metadata.published of
    Just { key: prevVersion, value: publishedInfo } -> do
      -- Look up the manifest for the previous version to get its dependencies
      maybeManifest <- Registry.readManifest name prevVersion
      case maybeManifest of
        Just (Manifest manifest) | not (Map.isEmpty manifest.dependencies) -> do
          -- Use previous version's dependencies to find compatible compilers
          let
            depVersions :: Map PackageName Version
            depVersions = Map.mapMaybeWithKey
              ( \depName range ->
                  case Map.lookup depName allMetadata of
                    Just (Metadata depMeta) ->
                      Array.last $ Array.filter (Range.includes range) $ Array.sort $ Array.fromFoldable $ Map.keys depMeta.published
                    Nothing -> Nothing
              )
              manifest.dependencies

          case compatibleCompilers allMetadata depVersions of
            Just compilerSet -> pure $ NonEmptySet.min compilerSet
            -- No intersection found, fall back to lowest compiler from previous version
            Nothing -> pure $ NonEmptyArray.head publishedInfo.compilers
        -- No manifest or no dependencies, fall back to lowest compiler from previous version
        _ -> pure $ NonEmptyArray.head publishedInfo.compilers
    Nothing ->
      NonEmptyArray.last <$> PursVersions.pursVersions

  let
    payload :: Operation.PublishData
    payload =
      { name
      , version
      , location: Nothing -- Use current metadata location at publish time
      , ref
      , compiler
      , resolutions: Nothing
      }

  case mode of
    DryRun -> do
      Log.info $ "[DRY RUN] Would submit publish job for " <> formatted <> " with compiler " <> Version.print compiler
      pure true

    Submit -> do
      Log.info $ "Submitting publish job for " <> formatted <> " with compiler " <> Version.print compiler
      result <- Run.liftAff $ submitJob (registryApiUrl <> "/v1/publish") payload
      case result of
        Left err -> do
          Log.error $ "Failed to submit publish job for " <> formatted <> ": " <> err
          pure false
        Right { jobId } -> do
          Log.info $ "Submitted publish job " <> unwrap jobId <> " for " <> formatted
          pure true

-- | Submit a job to the registry API
submitJob :: String -> Operation.PublishData -> Aff (Either String V1.JobCreatedResponse)
submitJob url payload = do
  let body = JSON.print $ CJ.encode Operation.publishCodec payload
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

-- | Given a set of package versions, determine the set of compilers that can be
-- | used for all packages by intersecting their supported compiler ranges.
compatibleCompilers :: Map PackageName Metadata -> Map PackageName Version -> Maybe (NonEmptySet Version)
compatibleCompilers allMetadata resolutions = do
  let
    associated :: Array { compilers :: NonEmptyArray Version }
    associated = Map.toUnfoldableUnordered resolutions # Array.mapMaybe \(Tuple depName depVersion) -> do
      Metadata depMeta <- Map.lookup depName allMetadata
      published <- Map.lookup depVersion depMeta.published
      Just { compilers: published.compilers }

  case Array.uncons associated of
    Nothing -> Nothing
    Just { head, tail: [] } -> Just $ NonEmptySet.fromFoldable1 head.compilers
    Just { head, tail } -> do
      let foldFn prev = Set.intersection prev <<< Set.fromFoldable <<< _.compilers
      NonEmptySet.fromFoldable $ Array.foldl foldFn (Set.fromFoldable head.compilers) tail
