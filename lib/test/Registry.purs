module Test.Registry where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Registry.PackageName as Test.PackageName
import Test.Registry.Range as Test.Range
import Test.Registry.Sha256 as Test.Sha256
import Test.Registry.Version as Test.Version
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

main :: Effect Unit
main = Aff.launchAff_ $ Spec.Runner.runSpec [ Spec.Reporter.consoleReporter ] do
  Spec.describe "Sha256" Test.Sha256.spec
  Spec.describe "PackageName" Test.PackageName.spec
  Spec.describe "Version" Test.Version.spec
  Spec.describe "Range" Test.Range.spec