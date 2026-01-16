module Test.Registry.App.Effect.Registry (spec) where

import Registry.App.Prelude

import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Ref as Ref
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry as Registry
import Registry.Test.Assert as Assert
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = Spec.it "Releases repo lock on exception" do
  locks <- Registry.newRepoLocks
  repoLock <- Registry.getOrCreateLock locks Registry.RegistryRepo

  result <- Aff.attempt $ runBase do
    Registry.withRepoLock Registry.API locks Registry.RegistryRepo do
      Log.info "Acquiring lock"
      Except.throw "boom"

  case result of
    Left _ -> do
      owner <- liftEffect $ Ref.read repoLock.owner
      available <- AVar.tryRead repoLock.lock
      Assert.shouldEqual Nothing owner
      Assert.shouldEqual (Just unit) available
    Right _ ->
      Assert.fail "Expected lock action to throw"
  where
  runBase :: forall a. Run (LOG + EXCEPT String + AFF + EFFECT + ()) a -> Aff a
  runBase =
    Log.interpret (\(Log.Log _ _ next) -> pure next)
      >>> Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
      >>> Run.runBaseAff'
