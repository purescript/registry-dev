-- | This script checks for packages that have moved to a new GitHub location
-- | and submits transfer jobs to update their registered location.
-- |
-- | Run via Nix:
-- |   nix run .#package-transferrer -- --dry-run   # Log what would be submitted
-- |   nix run .#package-transferrer -- --submit    # Actually submit to the API
-- |
-- | Required environment variables:
-- |   GITHUB_TOKEN - GitHub API token for fetching tags
-- |   PACCHETTIBOTTI_ED25519 - Private key for signing (only for --submit)
-- |   REGISTRY_API_URL - Registry API URL (default: https://registry.purescript.org)
module Registry.Scripts.PackageTransferrer where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Node.Path as Path
import Node.Process as Process
import Registry.API.V1 as V1
import Registry.App.Auth as Auth
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.Foreign.Octokit as Octokit
import Registry.Location (Location(..))
import Registry.Operation (AuthenticatedPackageOperation(..))
import Registry.Operation as Operation
import Registry.Operation.Validation as Operation.Validation
import Registry.PackageName as PackageName
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
      "Submit transfer jobs to the registry API."
      $> Submit
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "Check for moved packages and submit transfer jobs to the registry API."
  mode <- case Arg.parseArgs "package-transferrer" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right command -> pure command

  Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv
  token <- Env.lookupRequired Env.githubToken

  -- Only require pacchettibotti keys in submit mode
  maybePrivateKey <- case mode of
    DryRun -> pure Nothing
    Submit -> Just <$> Env.lookupRequired Env.pacchettibottiED25519

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

  runPackageTransferrer mode maybePrivateKey resourceEnv.registryApiUrl
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

type PackageTransferrerEffects = (REGISTRY + GITHUB + LOG + RESOURCE_ENV + EXCEPT String + AFF + EFFECT + ())

runPackageTransferrer :: Mode -> Maybe String -> URL -> Run PackageTransferrerEffects Unit
runPackageTransferrer mode maybePrivateKey registryApiUrl = do
  Log.info "Package Transferrer: checking for package transfers..."
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
                  | otherwise -> pure $ Just
                      { name
                      , newLocation: GitHub { owner: actual.owner, repo: actual.repo, subdir: registered.subdir }
                      }

  uniqueTransfers <- Array.catMaybes <$> for transfersNeeded \transfer@{ name, newLocation } ->
    if Operation.Validation.locationIsUnique newLocation allMetadata then
      pure $ Just transfer
    else do
      Log.warn $ Array.fold
        [ "Skipping transfer for "
        , PackageName.print name
        , " because location "
        , formatLocation newLocation
        , " is already registered."
        ]
      pure Nothing

  case Array.length uniqueTransfers of
    0 -> Log.info "No packages require transferring."
    n -> do
      Log.info $ show n <> " packages need transferring"
      for_ uniqueTransfers \{ name, newLocation } ->
        submitTransferJob mode maybePrivateKey registryApiUrl name newLocation

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

-- | Submit a transfer job for a package that has moved
submitTransferJob
  :: Mode
  -> Maybe String
  -> URL
  -> PackageName
  -> Location
  -> Run PackageTransferrerEffects Unit
submitTransferJob mode maybePrivateKey registryApiUrl name newLocation = do
  let
    formatted = PackageName.print name
    locationStr = formatLocation newLocation

  case mode of
    DryRun -> do
      Log.info $ "[DRY RUN] Would submit transfer job for " <> formatted <> " to " <> locationStr

    Submit -> do
      privateKey <- case maybePrivateKey of
        Nothing -> Except.throw "PACCHETTIBOTTI_ED25519 required for --submit mode"
        Just pk -> pure pk

      let
        payload :: Operation.TransferData
        payload = { name, newLocation }

        rawPayload = JSON.print $ CJ.encode Operation.transferCodec payload

      -- Sign the payload with pacchettibotti keys
      signature <- case Auth.signPayload { privateKey, rawPayload } of
        Left err -> Except.throw $ "Error signing transfer for " <> formatted <> ": " <> err
        Right sig -> pure sig

      let
        authenticatedData :: Operation.AuthenticatedData
        authenticatedData =
          { payload: Transfer payload
          , rawPayload
          , signature
          }

      Log.info $ "Submitting transfer job for " <> formatted
      result <- Run.liftAff $ submitJob (registryApiUrl <> "/v1/transfer") authenticatedData
      case result of
        Left err ->
          Log.error $ "Failed to submit transfer job for " <> formatted <> ": " <> err
        Right { jobId } ->
          Log.info $ "Submitted transfer job " <> unwrap jobId <> " for " <> formatted

formatLocation :: Location -> String
formatLocation = case _ of
  GitHub { owner, repo } -> owner <> "/" <> repo
  Git { url } -> url

-- | Submit a transfer job to the registry API
submitJob :: String -> Operation.AuthenticatedData -> Aff (Either String V1.JobCreatedResponse)
submitJob url authData = do
  let body = JSON.print $ CJ.encode Operation.authenticatedCodec authData
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
