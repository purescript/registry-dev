-- | End-to-end tests for the GitHubIssue workflow.
-- | Tests the full flow: parsing GitHub event → submitting to registry API →
-- | polling for completion → posting comments.
module Test.E2E.GitHubIssue (spec) where

import Registry.App.Prelude

import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.String as String
import Effect.Aff (Milliseconds(..))
import JSON as JSON
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.GitHubIssue as GitHubIssue
import Registry.Foreign.Tmp as Tmp
import Registry.Operation as Operation
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2E, E2ESpec)
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "GitHubIssue end-to-end" do
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
runWorkflow :: String -> E2E (Array WireMock.WireMockRequest)
runWorkflow eventJson = do
  { stateDir } <- ask

  Client.getStatus

  tmpDir <- liftAff Tmp.mkTmpDir
  let eventPath = Path.concat [ tmpDir, "github-event.json" ]
  liftAff $ FS.Aff.writeTextFile UTF8 eventPath eventJson
  liftEffect $ Process.setEnv "GITHUB_EVENT_PATH" eventPath

  originalCwd <- liftEffect Process.cwd
  liftEffect $ Process.chdir stateDir

  envResult <- liftAff GitHubIssue.initializeGitHub
  for_ envResult \env -> do
    let testEnv = env { pollConfig = { maxAttempts: 60, interval: Milliseconds 500.0 }, logVerbosity = Quiet }
    liftAff $ void $ GitHubIssue.runGitHubIssue testEnv

  liftEffect $ Process.chdir originalCwd

  WireMock.getGithubRequests

-- Assertions (all operate on captured requests)
assertComment :: String -> Array WireMock.WireMockRequest -> E2E Unit
assertComment text requests = do
  let
    comments = requests # Array.filter \r ->
      r.method == "POST" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber <> "/comments") r.url
  unless (Array.any (bodyContains text) comments) do
    WireMock.failWithRequests ("Expected '" <> text <> "' comment but not found") requests

assertNoComment :: String -> Array WireMock.WireMockRequest -> E2E Unit
assertNoComment text requests = do
  let
    comments = requests # Array.filter \r ->
      r.method == "POST" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber <> "/comments") r.url
  when (Array.any (bodyContains text) comments) do
    WireMock.failWithRequests ("Did not expect '" <> text <> "' comment") requests

assertClosed :: Array WireMock.WireMockRequest -> E2E Unit
assertClosed requests = do
  let
    closes = requests # Array.filter \r ->
      r.method == "PATCH" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber) r.url
  when (Array.null closes) do
    WireMock.failWithRequests "Expected issue to be closed" requests

assertOpen :: Array WireMock.WireMockRequest -> E2E Unit
assertOpen requests = do
  let
    closes = requests # Array.filter \r ->
      r.method == "PATCH" && String.contains (String.Pattern $ "/issues/" <> show testIssueNumber) r.url
  unless (Array.null closes) do
    WireMock.failWithRequests "Expected issue to remain open" requests

assertTeamsApiCalled :: Array WireMock.WireMockRequest -> E2E Unit
assertTeamsApiCalled requests = do
  let
    teams = requests # Array.filter \r ->
      r.method == "GET" && String.contains (String.Pattern "/orgs/purescript/teams/packaging/members") r.url
  when (Array.null teams) do
    WireMock.failWithRequests "Expected Teams API to be called" requests

bodyContains :: String -> WireMock.WireMockRequest -> Boolean
bodyContains text r = fromMaybe false (String.contains (String.Pattern text) <$> r.body)
