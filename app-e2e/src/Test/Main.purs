module Test.E2E.Main (main) where

import Registry.App.Prelude

import Data.Time.Duration (Milliseconds(..))
import Test.E2E.Endpoint.Jobs as Jobs
import Test.E2E.Endpoint.Publish as Publish
import Test.E2E.Endpoint.Transfer as Transfer
import Test.E2E.Endpoint.Unpublish as Unpublish
import Test.E2E.GitHubIssue as GitHubIssue
import Test.E2E.Support.Env (assertReposClean, mkTestEnv, resetTestState, runE2E)
import Test.E2E.Workflow as Workflow
import Test.Spec (hoistSpec)
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Cfg

main :: Effect Unit
main = do
  env <- mkTestEnv
  runSpecAndExitProcess' config [ consoleReporter ] $ hoistE2E env do
    Spec.before_ resetTestState $ Spec.after_ assertReposClean $ Spec.describe "E2E Tests" do
      Spec.describe "Endpoints" do
        Spec.describe "Publish" Publish.spec
        Spec.describe "Jobs" Jobs.spec
        Spec.describe "Unpublish" Unpublish.spec
        Spec.describe "Transfer" Transfer.spec

      Spec.describe "Workflows" do
        Spec.describe "GitHubIssue" GitHubIssue.spec
        Spec.describe "Multi-operation" Workflow.spec
  where
  hoistE2E env = hoistSpec identity (\_ m -> runE2E env m)
  config =
    { defaultConfig: Cfg.defaultConfig { timeout = Just $ Milliseconds 60_000.0 }
    , parseCLIOptions: false
    }
