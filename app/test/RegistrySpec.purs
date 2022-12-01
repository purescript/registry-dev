module Test.RegistrySpec where

import Registry.App.Prelude

import Data.Map as Map
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as Octokit
import Registry.App.RegistryM (Env, RegistryEffects, RegistryM)
import Registry.App.RegistryM as RegistryM
import Registry.Effect.Log as Log
import Run (Run)
import Run as Run
import Test.Spec as Spec
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner as Runner

type RegistrySpec a = Spec.SpecT RegistryM Unit Identity a

defaultTestEnv :: Env
defaultTestEnv =
  { closeIssue: mempty
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

defaultTestEffectHandler :: Run RegistryEffects ~> Aff
defaultTestEffectHandler =
  Log.runLogExcept
    >>> Run.interpret (Run.on Log._log Log.handleLogConsole Run.send)
    >>> Run.runBaseAff'

toSpec :: RegistrySpec Unit -> Spec.Spec Unit
toSpec = Spec.hoistSpec identity (\_ -> RegistryM.runRegistryM defaultTestEnv defaultTestEffectHandler)

runRegistrySpec :: RegistrySpec Unit -> Aff Unit
runRegistrySpec = Runner.runSpec [ Reporter.consoleReporter ] <<< toSpec
