module Test.Registry where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Registry.Internal as Test.Internal
import Test.Registry.Manifest as Test.Manifest
import Test.Registry.Metadata as Test.Metadata
import Test.Registry.Operation as Test.Operation
import Test.Registry.PackageName as Test.PackageName
import Test.Registry.PackageSet as Test.PackageSet
import Test.Registry.Range as Test.Range
import Test.Registry.Sha256 as Test.Sha256
import Test.Registry.Version as Test.Version
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

main :: Effect Unit
main = Aff.launchAff_ $ Spec.Runner.runSpec [ Spec.Reporter.consoleReporter ] do
  Spec.describe "Internal" Test.Internal.spec

  Spec.describe "Data Types" do
    Spec.describe "Sha256" Test.Sha256.spec
    Spec.describe "PackageName" Test.PackageName.spec
    Spec.describe "Version" Test.Version.spec
    Spec.describe "Range" Test.Range.spec
    Spec.describe "Manifest" Test.Manifest.spec
    Spec.describe "Metadata" Test.Metadata.spec
    Spec.describe "Package Set" Test.PackageSet.spec
    Spec.describe "Operation" Test.Operation.spec
