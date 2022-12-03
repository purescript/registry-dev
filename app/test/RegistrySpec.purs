module Test.RegistrySpec where

import Registry.App.Prelude

import Test.Spec as Spec
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner as Runner
import Test.TestM (TestM)
import Test.TestM as TestM

type RegistrySpec a = Spec.SpecT TestM Unit Identity a

toSpec :: RegistrySpec Unit -> Spec.Spec Unit
toSpec = Spec.hoistSpec identity (\_ -> TestM.runTestM TestM.defaultTestEnv)

runRegistrySpec :: RegistrySpec Unit -> Aff Unit
runRegistrySpec = Runner.runSpec [ Reporter.consoleReporter ] <<< toSpec
