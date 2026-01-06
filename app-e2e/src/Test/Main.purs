module Test.E2E.Main (main) where

import Registry.App.Prelude

import Data.Time.Duration (Milliseconds(..))
import Test.E2E.Endpoint.Jobs as Jobs
import Test.E2E.Endpoint.Publish as Publish
import Test.E2E.Endpoint.Transfer as Transfer
import Test.E2E.Endpoint.Unpublish as Unpublish
import Test.E2E.GitHubIssue as GitHubIssue
import Test.E2E.Workflow as Workflow
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Cfg

main :: Effect Unit
main = runSpecAndExitProcess' config [ consoleReporter ] do
  Spec.describe "E2E Tests" do
    Spec.describe "Endpoints" do
      Spec.describe "Jobs" Jobs.spec
      Spec.describe "Publish" Publish.spec
      Spec.describe "Unpublish" Unpublish.spec
      Spec.describe "Transfer" Transfer.spec

    Spec.describe "Workflows" do
      Spec.describe "GitHubIssue" GitHubIssue.spec
      Spec.describe "Multi-operation" Workflow.spec
  where
  config =
    { defaultConfig: Cfg.defaultConfig { timeout = Just $ Milliseconds 60_000.0 }
    , parseCLIOptions: false
    }
