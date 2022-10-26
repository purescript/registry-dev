module Test.Registry where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Registry.SRIHash as Test.SRIHash
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

main :: Effect Unit
main = Aff.launchAff_ $ Spec.Runner.runSpec [ Spec.Reporter.consoleReporter ] do
  Spec.describe "SRIHash" Test.SRIHash.spec
