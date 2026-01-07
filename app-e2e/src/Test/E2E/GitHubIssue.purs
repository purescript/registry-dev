-- | End-to-end tests for the GitHubIssue workflow.
-- | Tests the full flow: parsing GitHub event → submitting to registry API →
-- | polling for completion → posting comments.
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
import Registry.Operation as Operation
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "GitHubIssue end-to-end" do
    -- Clear both WireMock journal and filesystem cache before each test
    -- to ensure fresh API calls are captured for verification
    Spec.before_ (Env.clearGithubRequests *> Env.resetGitHubRequestCache) do

      Spec.it "handles publish via GitHub issue, posts comments, and closes issue on success" do
        requests <- runWorkflow $ mkPublishEvent Fixtures.effectPublishData
        assertComment "Job started" requests
        assertComment "Job completed successfully" requests
        assertClosed requests

      Spec.it "posts failure comment and leaves issue open when job fails" do
        requests <- runWorkflow $ mkAuthenticatedEvent "random-user" Fixtures.failingTransferData
        assertComment "Job started" requests
        assertComment "Job failed" requests
        assertNoComment "Job completed successfully" requests
        assertOpen requests

      Spec.it "calls Teams API to verify trustee membership for authenticated operation" do
        requests <- runWorkflow $ mkAuthenticatedEvent packagingTeamUser Fixtures.trusteeAuthenticatedData
        assertComment "Job started" requests
        assertTeamsApiCalled requests

      Spec.it "posts error comment when issue body contains invalid JSON" do
        requests <- runWorkflow Fixtures.invalidJsonIssueEvent
        assertComment "malformed" requests
        assertOpen requests

-- Constants
testIssueNumber :: Int
testIssueNumber = 101

packagingTeamUser :: String
packagingTeamUser = "packaging-team-user"

-- Event builders
githubEventCodec :: CJ.Codec { sender :: { login :: String }, issue :: { number :: Int, body :: String } }
githubEventCodec = CJ.named "GitHubEvent" $ CJ.Record.object
  { sender: CJ.Record.object { login: CJ.string }
  , issue: CJ.Record.object { number: CJ.int, body: CJ.string }
  }

mkPublishEvent :: Operation.PublishData -> String
mkPublishEvent publishData =
  let
    body = "```json\n" <> JSON.print (CJ.encode Operation.publishCodec publishData) <> "\n```"
  in
    JSON.print $ CJ.encode githubEventCodec
      { sender: { login: packagingTeamUser }, issue: { number: testIssueNumber, body } }

mkAuthenticatedEvent :: String -> Operation.AuthenticatedData -> String
mkAuthenticatedEvent username authData =
  let
    body = "```json\n" <> JSON.print (CJ.encode Operation.authenticatedCodec authData) <> "\n```"
  in
    JSON.print $ CJ.encode githubEventCodec
      { sender: { login: username }, issue: { number: testIssueNumber, body } }

-- Workflow runner
runWorkflow :: String -> Aff (Array WireMock.WireMockRequest)
runWorkflow eventJson = do
  -- Verify server and write event file
  config <- Env.getConfig
  Client.getStatus config >>= case _ of
    Left err -> Aff.throwError $ Aff.error $ "Server not reachable: " <> Client.printClientError err
    Right _ -> pure unit

  tmpDir <- Tmp.mkTmpDir
  let eventPath = Path.concat [ tmpDir, "github-event.json" ]
  FS.Aff.writeTextFile UTF8 eventPath eventJson
  liftEffect $ Process.setEnv "GITHUB_EVENT_PATH" eventPath

  -- GitHubIssue uses a relative "scratch" path, so we need to cd to STATE_DIR
  -- just like the server does. This ensures cache/logs go to the right place.
  stateDir <- Env.getStateDir
  originalCwd <- liftEffect Process.cwd
  liftEffect $ Process.chdir stateDir

  -- Run workflow (may return Nothing for parse errors, which is fine)
  envResult <- GitHubIssue.initializeGitHub
  for_ envResult \env -> do
    let testEnv = env { pollConfig = { maxAttempts: 60, interval: Milliseconds 500.0 }, logVerbosity = Quiet }
    void $ GitHubIssue.runGitHubIssue testEnv

  -- Restore original working directory
  liftEffect $ Process.chdir originalCwd

  -- Capture and return requests
  wmConfig <- liftEffect WireMock.configFromEnv
  WireMock.getRequestsOrFail wmConfig

-- Assertions (all operate on captured requests)
assertComment :: String -> Array WireMock.WireMockRequest -> Aff Unit
assertComment text requests = do
  let
    comments = requests # Array.filter \r ->
      r.method == "POST" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber <> "/comments") r.url
  unless (Array.any (bodyContains text) comments) do
    WireMock.failWithRequests ("Expected '" <> text <> "' comment but not found") requests

assertNoComment :: String -> Array WireMock.WireMockRequest -> Aff Unit
assertNoComment text requests = do
  let
    comments = requests # Array.filter \r ->
      r.method == "POST" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber <> "/comments") r.url
  when (Array.any (bodyContains text) comments) do
    WireMock.failWithRequests ("Did not expect '" <> text <> "' comment") requests

assertClosed :: Array WireMock.WireMockRequest -> Aff Unit
assertClosed requests = do
  let
    closes = requests # Array.filter \r ->
      r.method == "PATCH" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber) r.url
  when (Array.null closes) do
    WireMock.failWithRequests "Expected issue to be closed" requests

assertOpen :: Array WireMock.WireMockRequest -> Aff Unit
assertOpen requests = do
  let
    closes = requests # Array.filter \r ->
      r.method == "PATCH" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber) r.url
  unless (Array.null closes) do
    WireMock.failWithRequests "Expected issue to remain open" requests

assertTeamsApiCalled :: Array WireMock.WireMockRequest -> Aff Unit
assertTeamsApiCalled requests = do
  let
    teams = requests # Array.filter \r ->
      r.method == "GET" && String.contains (String.Pattern "/orgs/purescript/teams/packaging/members") r.url
  when (Array.null teams) do
    WireMock.failWithRequests "Expected Teams API to be called" requests

bodyContains :: String -> WireMock.WireMockRequest -> Boolean
bodyContains text r = fromMaybe false (String.contains (String.Pattern text) <$> r.body)
