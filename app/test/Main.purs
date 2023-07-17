module Test.Registry.Main (main) where

import Registry.App.Prelude

import Data.Time.Duration (Milliseconds(..))
import Test.Registry.App.API as Test.API
import Test.Registry.App.Auth as Test.Auth
import Test.Registry.App.CLI.Licensee as Test.CLI.Licensee
import Test.Registry.App.CLI.Purs as Test.CLI.Purs
import Test.Registry.App.CLI.Tar as Test.CLI.Tar
import Test.Registry.App.Effect.PackageSets as Test.Effect.PackageSets
import Test.Registry.App.GitHubIssue as Test.GitHubIssue
import Test.Registry.App.Legacy.LenientRange as Test.Legacy.LenientRange
import Test.Registry.App.Legacy.LenientVersion as Test.Legacy.LenientVersion
import Test.Registry.App.Legacy.Manifest as Test.Legacy.Manifest
import Test.Registry.App.Legacy.PackageSet as Test.Legacy.PackageSet
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [ consoleReporter ] do
  Spec.describe "Registry.App.CLI" do
    Spec.describe "Licensee" Test.CLI.Licensee.spec
    Spec.describe "Tar" Test.CLI.Tar.spec
    Spec.describe "Purs" Test.CLI.Purs.spec

  Spec.describe "Registry.App.Effect" do
    Test.Effect.PackageSets.spec

  Spec.describe "Registry.App.Legacy" do
    Spec.describe "Lenient Version" Test.Legacy.LenientVersion.spec
    Spec.describe "Lenient Range" Test.Legacy.LenientRange.spec
    Spec.describe "Legacy Manifest" Test.Legacy.Manifest.spec
    Spec.describe "Legacy Package Set" Test.Legacy.PackageSet.spec

  Spec.describe "Registry.App.Auth" do
    Test.Auth.spec

  Spec.describe "Registry.App.API" do
    Test.API.spec

  Spec.describe "Registry.App.GitHubIssue" do
    Test.GitHubIssue.spec
