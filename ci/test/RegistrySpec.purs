module Test.RegistrySpec where

import Registry.Prelude

import Data.Map as Map
import Data.Newtype (unwrap)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Registry.RegistryM (Env, RegistryM)
import Registry.RegistryM as RegistryM
import Test.Spec as Spec
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner as Runner

newtype RegistrySpec = RegistrySpec (Spec.SpecT RegistryM Unit Identity Unit)

derive instance Newtype RegistrySpec _

defaultTestEnv :: Env
defaultTestEnv =
  { closeIssue: mempty
  , comment: mempty
  , commitToTrunk: \_ _ -> pure (Right unit)
  , deletePackage: mempty
  , uploadPackage: mempty
  , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
  }

toSpec :: RegistrySpec -> Spec.Spec Unit
toSpec = Spec.hoistSpec identity (\_ -> RegistryM.runRegistryM defaultTestEnv) <<< unwrap

runRegistrySpec :: RegistrySpec -> Aff Unit
runRegistrySpec = Runner.runSpec [ Reporter.consoleReporter ] <<< toSpec
