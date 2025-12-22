-- | End-to-end tests for the GitHubIssue workflow.
-- | These tests exercise the full flow: parsing a GitHub event, submitting to
-- | the registry API, polling for completion, and posting comments.
module Test.E2E.GitHubIssue (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.String as String
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import JSON as JSON
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.GitHubIssue as GitHubIssue
import Registry.Foreign.Tmp as Tmp
import Registry.Operation (AuthenticatedData)
import Registry.Operation as Operation
import Registry.Test.E2E.Client as Client
import Registry.Test.E2E.Fixtures as Fixtures
import Registry.Test.E2E.WireMock (WireMockRequest)
import Registry.Test.E2E.WireMock as WireMock
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "GitHubIssue end-to-end" do
    Spec.before clearWireMockJournal do

      Spec.it "handles a publish via GitHub issue, posts comments, and closes issue on success" \_ -> do
        result <- runWorkflowWithEvent $ mkGitHubPublishEvent Fixtures.effectPublishData

        assertJobSucceeded result
        assertHasComment jobStartedText result
        assertHasComment jobCompletedText result
        assertIssueClosed result

      Spec.it "posts failure comment and leaves issue open when job fails" \_ -> do
        result <- runWorkflowWithEvent $ mkGitHubPublishEvent Fixtures.failingPublishData

        assertJobFailed result
        assertHasComment jobStartedText result
        assertHasComment jobFailedText result
        assertNoComment jobCompletedText result
        assertIssueOpen result

      Spec.it "re-signs authenticated operation for trustee (job fails due to unpublish time limit)" \_ -> do
        result <- runWorkflowWithEvent $ mkGitHubAuthenticatedEvent Fixtures.trusteeAuthenticatedData

        assertHasComment jobStartedText result
        assertTeamsApiCalled result

  where
  clearWireMockJournal :: Aff Unit
  clearWireMockJournal = do
    wmConfig <- liftEffect WireMock.configFromEnv
    WireMock.clearRequestsOrFail wmConfig

testIssueNumber :: Int
testIssueNumber = 101

-- | Username configured as a packaging team member in test WireMock fixtures.
-- | See nix/test/config.nix for the GitHub Teams API stub.
packagingTeamUsername :: String
packagingTeamUsername = "packaging-team-user"

jobStartedText :: String
jobStartedText = "Job started"

jobCompletedText :: String
jobCompletedText = "Job completed successfully"

jobFailedText :: String
jobFailedText = "Job failed"

packagingTeamMembersPath :: String
packagingTeamMembersPath = "/orgs/purescript/teams/packaging/members"

testPollConfig :: GitHubIssue.PollConfig
testPollConfig =
  { maxAttempts: 60
  , interval: Milliseconds 500.0
  }

githubEventCodec :: CJ.Codec { sender :: { login :: String }, issue :: { number :: Int, body :: String } }
githubEventCodec = CJ.named "GitHubEvent" $ CJ.Record.object
  { sender: CJ.Record.object { login: CJ.string }
  , issue: CJ.Record.object { number: CJ.int, body: CJ.string }
  }

mkGitHubPublishEvent :: Operation.PublishData -> String
mkGitHubPublishEvent publishData =
  let
    publishJson = JSON.print $ CJ.encode Operation.publishCodec publishData
    body = "```json\n" <> publishJson <> "\n```"
    event = { sender: { login: packagingTeamUsername }, issue: { number: testIssueNumber, body } }
  in
    JSON.print $ CJ.encode githubEventCodec event

mkGitHubAuthenticatedEvent :: AuthenticatedData -> String
mkGitHubAuthenticatedEvent authData =
  let
    authJson = JSON.print $ CJ.encode Operation.authenticatedCodec authData
    body = "```json\n" <> authJson <> "\n```"
    event = { sender: { login: packagingTeamUsername }, issue: { number: testIssueNumber, body } }
  in
    JSON.print $ CJ.encode githubEventCodec event

issuePath :: Int -> String
issuePath n = "/issues/" <> show n

issueCommentsPath :: Int -> String
issueCommentsPath n = issuePath n <> "/comments"

commentRequests :: Array WireMockRequest -> Array WireMockRequest
commentRequests =
  WireMock.filterByMethod "POST"
    >>> WireMock.filterByUrlContaining (issueCommentsPath testIssueNumber)

closeRequests :: Array WireMockRequest -> Array WireMockRequest
closeRequests =
  WireMock.filterByMethod "PATCH"
    >>> WireMock.filterByUrlContaining (issuePath testIssueNumber)

teamsRequests :: Array WireMockRequest -> Array WireMockRequest
teamsRequests =
  WireMock.filterByMethod "GET"
    >>> WireMock.filterByUrlContaining packagingTeamMembersPath

bodyContains :: String -> WireMockRequest -> Boolean
bodyContains text r = fromMaybe false (String.contains (String.Pattern text) <$> r.body)

hasComment :: String -> Array WireMockRequest -> Boolean
hasComment text = Array.any (bodyContains text)

-- | Result of running the GitHubIssue workflow.
type RunResult =
  { success :: Boolean
  , requests :: Array WireMockRequest
  }

-- | Run the GitHub issue workflow with a given event JSON.
-- | Handles server check, temp file creation, env setup, and request capture.
runWorkflowWithEvent :: String -> Aff RunResult
runWorkflowWithEvent eventJson = do
  -- Verify server is reachable
  config <- liftEffect Client.configFromEnv
  statusResult <- Client.getStatus config
  case statusResult of
    Left err -> Aff.throwError $ Aff.error $ "Server not reachable: " <> Client.printClientError err
    Right _ -> pure unit

  -- Write event to temp file
  tmpDir <- Tmp.mkTmpDir
  let eventPath = Path.concat [ tmpDir, "github-event.json" ]
  FS.Aff.writeTextFile UTF8 eventPath eventJson
  liftEffect $ Process.setEnv "GITHUB_EVENT_PATH" eventPath

  -- Initialize and run workflow
  envResult <- GitHubIssue.initializeGitHub
  case envResult of
    Nothing ->
      Aff.throwError $ Aff.error "initializeGitHub returned Nothing"
    Just env -> do
      let testEnv = env { pollConfig = testPollConfig, logVerbosity = Quiet }
      result <- GitHubIssue.runGitHubIssue testEnv

      -- Capture WireMock requests
      wmConfig <- liftEffect WireMock.configFromEnv
      requests <- WireMock.getRequestsOrFail wmConfig

      case result of
        Left err ->
          WireMock.failWithRequests ("runGitHubIssue failed: " <> err) requests
        Right success ->
          pure { success, requests }

assertJobSucceeded :: RunResult -> Aff Unit
assertJobSucceeded { success, requests } =
  unless success do
    WireMock.failWithRequests "Job did not succeed" requests

assertJobFailed :: RunResult -> Aff Unit
assertJobFailed { success, requests } =
  when success do
    WireMock.failWithRequests "Expected job to fail but it succeeded" requests

assertHasComment :: String -> RunResult -> Aff Unit
assertHasComment text { requests } =
  unless (hasComment text (commentRequests requests)) do
    WireMock.failWithRequests ("Expected '" <> text <> "' comment but not found") requests

assertNoComment :: String -> RunResult -> Aff Unit
assertNoComment text { requests } =
  when (hasComment text (commentRequests requests)) do
    WireMock.failWithRequests ("Did not expect '" <> text <> "' comment") requests

assertIssueClosed :: RunResult -> Aff Unit
assertIssueClosed { requests } =
  when (Array.null (closeRequests requests)) do
    WireMock.failWithRequests "Expected issue to be closed, but no close request was made" requests

assertIssueOpen :: RunResult -> Aff Unit
assertIssueOpen { requests } =
  unless (Array.null (closeRequests requests)) do
    WireMock.failWithRequests "Expected issue to remain open, but a close request was made" requests

assertTeamsApiCalled :: RunResult -> Aff Unit
assertTeamsApiCalled { requests } =
  when (Array.null (teamsRequests requests)) do
    WireMock.failWithRequests "Expected GitHub Teams API to be called, but no such request was seen" requests
