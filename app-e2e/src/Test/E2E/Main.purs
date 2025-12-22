module Test.E2E.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Test.E2E.GitHubIssue as Test.E2E.GitHubIssue
import Test.E2E.Publish as Test.E2E.Publish
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Cfg

main :: Effect Unit
main = runSpecAndExitProcess' config [ consoleReporter ] do
  Spec.describe "E2E Tests" do
    Spec.describe "Publish" Test.E2E.Publish.spec
    Spec.describe "GitHubIssue" Test.E2E.GitHubIssue.spec
  where
  config =
    { defaultConfig: Cfg.defaultConfig { timeout = Just $ Milliseconds 120_000.0 }
    , parseCLIOptions: false
    }
