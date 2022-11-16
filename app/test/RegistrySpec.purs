module Test.RegistrySpec where

import Registry.App.Prelude

import Data.Map as Map
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as Octokit
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
  , commitMetadataFile: \_ _ -> pure (Right unit)
  , commitIndexFile: \_ _ -> pure (Right unit)
  , commitPackageSetFile: \_ _ _ -> pure (Right unit)
  , deletePackage: mempty
  , uploadPackage: mempty
  , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
  , octokit: unsafePerformEffect (Octokit.mkOctokit (GitHubToken ""))
  , cache:
      { read: \_ -> pure (Left "")
      , write: mempty
      , remove: mempty
      }
  , username: mempty
  , registry: mempty
  , registryIndex: mempty
  }

toSpec :: RegistrySpec Unit -> Spec.Spec Unit
toSpec = Spec.hoistSpec identity (\_ -> RegistryM.runRegistryM defaultTestEnv)

runRegistrySpec :: RegistrySpec Unit -> Aff Unit
runRegistrySpec = Runner.runSpec [ Reporter.consoleReporter ] <<< toSpec
