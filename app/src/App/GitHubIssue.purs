-- | A thin client that proxies GitHub issue operations to the registry API server.
-- |
-- | When a GitHub issue is created or commented on in the purescript/registry repo,
-- | this module:
-- | 1. Parses the issue body to determine the operation type
-- | 2. Re-signs authenticated operations with pacchettibotti keys if submitted by a trustee
-- | 3. POSTs the operation to the registry API server
-- | 4. Polls for job completion, posting logs as GitHub comments
-- | 5. Closes the issue on success
module Registry.App.GitHubIssue where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import JSON.Object as CJ.Object
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.API.V1 as V1
import Registry.App.API as API
import Registry.App.Auth as Auth
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV, RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Constants as Constants
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (GitHubToken, IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

main :: Effect Unit
main = launchAff_ $ do
  initializeGitHub >>= case _ of
    Nothing -> pure unit
    Just env -> do
      result <- runGitHubIssue env
      case result of
        Left err -> do
          -- Post error as comment and exit with failure
          void $ Octokit.request env.octokit $ Octokit.createCommentRequest
            { address: Constants.registry
            , issue: env.issue
            , body: "❌ " <> err
            }
          liftEffect $ Process.exit' 1
        Right _ ->
          -- Issue closing is handled inside runGitHubIssue
          pure unit

runGitHubIssue :: GitHubEventEnv -> Aff (Either String Boolean)
runGitHubIssue env = do
  let cache = Path.concat [ scratchDir, ".cache" ]
  githubCacheRef <- Cache.newCacheRef

  let
    run :: forall a. Run (GITHUB + RESOURCE_ENV + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + LOG + EXCEPT String + AFF + EFFECT + ()) a -> Aff (Either String a)
    run action = action
      # GitHub.interpret (GitHub.handle { octokit: env.octokit, cache, ref: githubCacheRef })
      # Except.runExcept
      # Env.runResourceEnv env.resourceEnv
      # Env.runGitHubEventEnv { username: env.username, issue: env.issue }
      # Env.runPacchettiBottiEnv { publicKey: env.publicKey, privateKey: env.privateKey }
      # Log.interpret (Log.handleTerminal env.logVerbosity)
      # Run.runBaseAff'

  run do
    -- Determine endpoint and prepare the JSON payload
    { endpoint, jsonBody } <- case env.operation of
      Left (PackageSetUpdate payload) -> pure
        { endpoint: "/v1/package-sets"
        , jsonBody: JSON.print $ CJ.encode Operation.packageSetUpdateCodec payload
        }

      Right (Publish payload) -> pure
        { endpoint: "/v1/publish"
        , jsonBody: JSON.print $ CJ.encode Operation.publishCodec payload
        }

      Right (Authenticated auth) -> do
        -- Re-sign with pacchettibotti if submitter is a trustee
        signed <- signPacchettiBottiIfTrustee auth
        let
          endpoint = case signed.payload of
            Unpublish _ -> "/v1/unpublish"
            Transfer _ -> "/v1/transfer"
        pure { endpoint, jsonBody: JSON.print $ CJ.encode Operation.authenticatedCodec signed }

    -- Submit to the registry API
    let registryApiUrl = env.resourceEnv.registryApiUrl
    Log.debug $ "Submitting to " <> registryApiUrl <> endpoint
    submitResult <- Run.liftAff $ submitJob (registryApiUrl <> endpoint) jsonBody
    case submitResult of
      Left err -> Except.throw $ "Failed to submit job: " <> err
      Right { jobId } -> do
        let jobIdStr = unwrap jobId
        Log.debug $ "Job created: " <> jobIdStr

        -- Post initial comment with job ID
        Run.liftAff $ void $ Octokit.request env.octokit $ Octokit.createCommentRequest
          { address: Constants.registry
          , issue: env.issue
          , body: "Job started: `" <> jobIdStr <> "`\nLogs: " <> registryApiUrl <> "/v1/jobs/" <> jobIdStr
          }

        -- Poll for completion, posting logs as comments
        pollAndReport env.octokit env.issue env.pollConfig registryApiUrl jobId

-- | Submit a job to the registry API
submitJob :: String -> String -> Aff (Either String V1.JobCreatedResponse)
submitJob url body = do
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

-- | Poll a job until it completes, posting logs as GitHub comments.
-- | Returns true if the job succeeded, false otherwise.
pollAndReport
  :: forall r
   . Octokit
  -> IssueNumber
  -> PollConfig
  -> URL
  -> V1.JobId
  -> Run (LOG + EXCEPT String + AFF + r) Boolean
pollAndReport octokit issue pollConfig registryApiUrl jobId = go Nothing 0 0
  where
  maxConsecutiveErrors :: Int
  maxConsecutiveErrors = 5

  go :: Maybe DateTime -> Int -> Int -> Run (LOG + EXCEPT String + AFF + r) Boolean
  go lastTimestamp attempt consecutiveErrors
    | attempt >= pollConfig.maxAttempts = do
        Run.liftAff $ void $ Octokit.request octokit $ Octokit.createCommentRequest
          { address: Constants.registry
          , issue
          , body: "⏱️ Job timed out"
          }
        pure false
    | consecutiveErrors >= maxConsecutiveErrors = do
        Run.liftAff $ void $ Octokit.request octokit $ Octokit.createCommentRequest
          { address: Constants.registry
          , issue
          , body: "❌ Failed to poll job status after " <> show maxConsecutiveErrors <> " consecutive errors"
          }
        pure false
    | otherwise = do
        Run.liftAff $ Aff.delay pollConfig.interval
        result <- Run.liftAff $ fetchJob registryApiUrl jobId lastTimestamp
        case result of
          Left err -> do
            Log.error $ "Error polling job: " <> err
            go lastTimestamp (attempt + 1) (consecutiveErrors + 1)
          Right job -> do
            let info = V1.jobInfo job

            -- Post any new logs (filtered to Info level and above, and after lastTimestamp)
            let
              newLogs = Array.filter isNewLog info.logs
              isNewLog l = l.level >= V1.Info && case lastTimestamp of
                Nothing -> true
                Just ts -> l.timestamp > ts
            unless (Array.null newLogs) do
              let
                formatLog l = "[" <> V1.printLogLevel l.level <> "] " <> l.message
                logText = String.joinWith "\n" $ map formatLog newLogs
              Run.liftAff $ void $ Octokit.request octokit $ Octokit.createCommentRequest
                { address: Constants.registry
                , issue
                , body: "```\n" <> logText <> "\n```"
                }

            -- Check if job is done
            case info.finishedAt of
              Just _ -> do
                let statusMsg = if info.success then "✅ Job completed successfully" else "❌ Job failed"
                Run.liftAff $ void $ Octokit.request octokit $ Octokit.createCommentRequest
                  { address: Constants.registry
                  , issue
                  , body: statusMsg
                  }
                -- Close the issue on success, leave open on failure
                when info.success do
                  Run.liftAff $ void $ Octokit.request octokit $ Octokit.closeIssueRequest
                    { address: Constants.registry
                    , issue
                    }
                pure info.success
              Nothing -> do
                -- Continue polling with updated timestamp, reset consecutive errors on success
                let newTimestamp = Array.last newLogs <#> _.timestamp
                go (newTimestamp <|> lastTimestamp) (attempt + 1) 0

-- | Fetch job status from the API
fetchJob :: String -> V1.JobId -> Maybe DateTime -> Aff (Either String V1.Job)
fetchJob registryApiUrl (V1.JobId jobId) since = do
  let
    baseUrl = registryApiUrl <> "/v1/jobs/" <> jobId
    url = case since of
      Nothing -> baseUrl <> "?level=INFO"
      Just ts -> baseUrl <> "?level=INFO&since=" <> DateTime.format Internal.Format.iso8601DateTime ts
  result <- Aff.attempt $ Fetch.fetch url { method: GET }
  case result of
    Left err -> pure $ Left $ "Network error: " <> Aff.message err
    Right response -> do
      responseBody <- response.text
      if response.status == 200 then
        case JSON.parse responseBody >>= \json -> lmap CJ.DecodeError.print (CJ.decode V1.jobCodec json) of
          Left err -> pure $ Left $ "Failed to parse job: " <> err
          Right job -> pure $ Right job
      else
        pure $ Left $ "HTTP " <> show response.status <> ": " <> responseBody

-- | Configuration for polling job status
type PollConfig =
  { maxAttempts :: Int
  , interval :: Aff.Milliseconds
  }

-- | Default poll config: 30 minutes at 5 second intervals
defaultPollConfig :: PollConfig
defaultPollConfig =
  { maxAttempts: 360
  , interval: Aff.Milliseconds 5000.0
  }

type GitHubEventEnv =
  { octokit :: Octokit
  , token :: GitHubToken
  , issue :: IssueNumber
  , username :: String
  , operation :: Either PackageSetOperation PackageOperation
  , publicKey :: String
  , privateKey :: String
  , resourceEnv :: Env.ResourceEnv
  , pollConfig :: PollConfig
  , logVerbosity :: LogVerbosity
  }

initializeGitHub :: Aff (Maybe GitHubEventEnv)
initializeGitHub = do
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  resourceEnv <- Env.lookupResourceEnv
  eventPath <- Env.lookupRequired Env.githubEventPath

  octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl

  readOperation eventPath >>= case _ of
    NotJson ->
      pure Nothing

    MalformedJson issue err -> do
      let
        comment = String.joinWith "\n"
          [ "The JSON input for this package update is malformed:"
          , "```"
          , err
          , "```"
          , "You can try again by commenting on this issue with a corrected payload."
          ]

      Octokit.request octokit (Octokit.createCommentRequest { address: Constants.registry, issue, body: comment }) >>= case _ of
        Left githubError -> Aff.throwError $ Aff.error $ Octokit.printGitHubError githubError
        Right _ -> pure Nothing

    DecodedOperation issue username operation -> do
      pure $ Just
        { octokit
        , token
        , issue
        , username
        , operation
        , publicKey
        , privateKey
        , resourceEnv
        , pollConfig: defaultPollConfig
        , logVerbosity: Verbose
        }

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber String (Either PackageSetOperation PackageOperation)

derive instance Eq OperationDecoding

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.Aff.readTextFile UTF8 eventPath

  IssueEvent { issueNumber, body, username } <- case JSON.parse fileContents >>= decodeIssueEvent of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  let
    decodeOperation :: JSON -> Either CJ.DecodeError (Either PackageSetOperation PackageOperation)
    decodeOperation json = do
      object <- CJ.decode CJ.jobject json
      let keys = CJ.Object.keys object
      let hasKeys = all (flip Array.elem keys)
      if hasKeys [ "packages" ] then
        map (Left <<< PackageSetUpdate) (CJ.decode Operation.packageSetUpdateCodec json)
      else if hasKeys [ "name", "ref", "compiler" ] then
        map (Right <<< Publish) (CJ.decode Operation.publishCodec json)
      else if hasKeys [ "payload", "signature" ] then
        map (Right <<< Authenticated) (CJ.decode Operation.authenticatedCodec json)
      else
        Left $ CJ.DecodeError.basic "Operation: Expected a valid registry operation, but provided object did not match any operation decoder."

  case JSON.parse (JsonRepair.tryRepair (firstObject body)) of
    Left err -> do
      Console.log "Not JSON."
      Console.logShow { err, body }
      pure NotJson
    Right json -> case decodeOperation json of
      Left jsonError -> do
        let printedError = CJ.DecodeError.print jsonError
        Console.log $ "Malformed JSON:\n" <> printedError
        Console.log $ "Received body:\n" <> body
        pure $ MalformedJson issueNumber printedError
      Right operation ->
        pure $ DecodedOperation issueNumber username operation

-- | Users may submit issues with contents wrapped in code fences, perhaps with
-- | a language specifier, trailing lines, and other issues. This rudimentary
-- | cleanup pass retrieves all contents within an opening { and closing }
-- | delimiter.
firstObject :: String -> String
firstObject input = fromMaybe input do
  before <- String.indexOf (String.Pattern "{") input
  let start = String.drop before input
  after <- String.lastIndexOf (String.Pattern "}") start
  pure (String.take (after + 1) start)

-- | An event triggered by a GitHub workflow, specifically via an issue commentAdd a comment on  line L244Add diff commentMarkdown input:  edit mode selected.WritePreviewHeadingBoldItalicQuoteCodeLinkUnordered listNumbered listTask listMentionReferenceSaved repliesAdd FilesPaste, drop, or click to add filesCancelCommentStart a reviewReturn to code
-- | or issue creation.
-- | https://docs.github.com/developers/webhooks-and-events/webhooks/webhook-events-and-payloads#issue_comment
newtype IssueEvent = IssueEvent
  { issueNumber :: IssueNumber
  , body :: String
  , username :: String
  }

derive instance Newtype IssueEvent _

decodeIssueEvent :: JSON -> Either String IssueEvent
decodeIssueEvent json = lmap CJ.DecodeError.print do
  object <- CJ.decode CJ.jobject json
  username <- Octokit.atKey "login" CJ.string =<< Octokit.atKey "sender" CJ.jobject object

  issueObject <- Octokit.atKey "issue" CJ.jobject object
  issueNumber <- Octokit.atKey "number" CJ.int issueObject

  -- We accept issue creation and issue comment events, but both contain an
  -- 'issue' field. However, only comments contain a 'comment' field. For that
  -- reason we first try to parse the comment and fall back to the issue if
  -- that fails.
  body <- Octokit.atKey "body" CJ.string =<< Octokit.atKey "comment" CJ.jobject object <|> pure issueObject
  pure $ IssueEvent { body, username, issueNumber: IssueNumber issueNumber }

-- | Re-sign a payload as pacchettibotti if the authenticated operation was
-- | submitted by a registry trustee.
--
-- @pacchettibotti is considered an 'owner' of all packages for authenticated
-- operations. Registry trustees can ask pacchettibotti to perform an action on
-- behalf of a package by submitting a payload with an empty signature. If the
-- payload was submitted by a trustee (ie. a member of the packaging team) then
-- pacchettibotti will re-sign it and add itself as an owner before continuing
-- with the authenticated operation.
signPacchettiBottiIfTrustee
  :: forall r
   . AuthenticatedData
  -> Run (GITHUB + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + LOG + EXCEPT String + r) AuthenticatedData
signPacchettiBottiIfTrustee auth = do
  GitHub.listTeamMembers API.packagingTeam >>= case _ of
    Left githubError -> do
      Log.warn $ Array.fold
        [ "Unable to fetch members of packaging team, not verifying whether requestor is a member of @purescript/packaging: "
        , Octokit.printGitHubError githubError
        ]
      pure auth
    Right members -> do
      { username } <- Env.askGitHubEvent
      if Array.elem username members then do
        Log.info "Authenticated payload submitted by a registry trustee, re-signing with pacchettibotti keys."
        { privateKey } <- Env.askPacchettiBotti
        signature <- case Auth.signPayload { privateKey, rawPayload: auth.rawPayload } of
          Left _ -> Except.throw "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature
        pure $ auth { signature = signature }
      else do
        Log.info "Authenticated payload not submitted by a registry trustee, continuing with original signature."
        pure auth
