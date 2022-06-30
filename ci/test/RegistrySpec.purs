module Test.RegistrySpec where

import Registry.Prelude

import Data.Map as Map
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..))
import Registry.RegistryM (Env, RegistryM)
import Registry.RegistryM as RegistryM
import Test.Spec as Spec
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner as Runner

type RegistrySpec a = Spec.SpecT RegistryM Unit Identity a

defaultTestEnv :: Env
defaultTestEnv =
  { closeIssue: mempty
  , comment: mempty
  , commitToTrunk: \_ _ -> pure (Right unit)
  , deletePackage: mempty
  , uploadPackage: mempty
  , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
  , githubCache: unsafePerformEffect (Ref.new Map.empty)
  , githubToken: GitHubToken ""
  }

toSpec :: RegistrySpec Unit -> Spec.Spec Unit
toSpec = Spec.hoistSpec identity (\_ -> RegistryM.runRegistryM defaultTestEnv)

runRegistrySpec :: RegistrySpec Unit -> Aff Unit
runRegistrySpec = Runner.runSpec [ Reporter.consoleReporter ] <<< toSpec
