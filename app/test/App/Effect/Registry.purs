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
spec = do
  Spec.it "Releases repo lock on exception" do
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

  Spec.it "Tracks lock owner while held" do
    locks <- Registry.newRepoLocks
    repoLock <- Registry.getOrCreateLock locks Registry.RegistryRepo
    entered <- AVar.empty
    release <- AVar.empty

    fiber <- Aff.forkAff $ runBase do
      Registry.withRepoLock Registry.Scheduler locks Registry.RegistryRepo do
        Run.liftAff $ AVar.put unit entered
        Run.liftAff $ AVar.take release

    _ <- AVar.take entered
    ownerWhile <- liftEffect $ Ref.read repoLock.owner
    Assert.shouldEqual (Just Registry.Scheduler) ownerWhile

    _ <- AVar.put unit release
    _ <- Aff.joinFiber fiber

    ownerAfter <- liftEffect $ Ref.read repoLock.owner
    Assert.shouldEqual Nothing ownerAfter

  Spec.it "Serializes work for same repo" do
    locks <- Registry.newRepoLocks
    entered <- AVar.empty
    release <- AVar.empty
    secondEntered <- liftEffect $ Ref.new false

    let
      action = Registry.withRepoLock Registry.API locks Registry.RegistryRepo
      buildWorker = runBase <<< action

      worker1 = buildWorker do
        Run.liftAff $ AVar.put unit entered
        Run.liftAff $ AVar.take release

      worker2 = buildWorker do
        Run.liftEffect $ Ref.write true secondEntered

    fiber1 <- Aff.forkAff worker1
    _ <- AVar.take entered
    fiber2 <- Aff.forkAff worker2

    _ <- Aff.delay (Aff.Milliseconds 50.0)
    enteredBefore <- liftEffect $ Ref.read secondEntered
    Assert.shouldEqual false enteredBefore

    _ <- AVar.put unit release
    _ <- Aff.joinFiber fiber1
    _ <- Aff.joinFiber fiber2

    enteredAfter <- liftEffect $ Ref.read secondEntered
    Assert.shouldEqual true enteredAfter

  Spec.it "Allows concurrent work for different repos" do
    locks <- Registry.newRepoLocks
    enteredA <- AVar.empty
    enteredB <- AVar.empty
    releaseA <- AVar.empty
    releaseB <- AVar.empty
    doneA <- liftEffect $ Ref.new false
    doneB <- liftEffect $ Ref.new false

    let
      action repo entered release flag = runBase do
        Registry.withRepoLock Registry.API locks repo do
          Run.liftAff $ AVar.put unit entered
          Run.liftAff $ AVar.take release
          Run.liftEffect $ Ref.write true flag

      workerA = action Registry.RegistryRepo enteredA releaseA doneA
      workerB = action Registry.ManifestIndexRepo enteredB releaseB doneB

    _ <- Aff.forkAff workerA
    _ <- AVar.take enteredA

    _ <- Aff.forkAff workerB
    _ <- Aff.delay (Aff.Milliseconds 50.0)
    enteredBStatus <- AVar.tryRead enteredB
    Assert.shouldEqual (Just unit) enteredBStatus

    _ <- AVar.put unit releaseA
    _ <- AVar.put unit releaseB

    gotA <- liftEffect $ Ref.read doneA
    gotB <- liftEffect $ Ref.read doneB
    Assert.shouldEqual true gotA
    Assert.shouldEqual true gotB

  Spec.it "Releases lock after timeout" do
    locks <- Registry.newRepoLocks
    repoLock <- Registry.getOrCreateLock locks Registry.RegistryRepo

    result <- Aff.attempt $ runBase do
      Registry.withRepoLockTimeout (Aff.Milliseconds 10.0) Registry.API locks Registry.RegistryRepo do
        Run.liftAff $ Aff.delay (Aff.Milliseconds 50.0)

    case result of
      Left _ -> do
        owner <- liftEffect $ Ref.read repoLock.owner
        available <- AVar.tryRead repoLock.lock
        Assert.shouldEqual Nothing owner
        Assert.shouldEqual (Just unit) available

        followUp <- Aff.attempt $ runBase do
          Registry.withRepoLock Registry.API locks Registry.RegistryRepo do
            pure unit
        Assert.shouldEqual true (isRight followUp)
      Right _ ->
        Assert.fail "Expected timeout"
  where
  runBase :: forall a. Run (LOG + EXCEPT String + AFF + EFFECT + ()) a -> Aff a
  runBase =
    Log.interpret (\(Log.Log _ _ next) -> pure next)
      >>> Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
      >>> Run.runBaseAff'
