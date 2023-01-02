module Registry.Test.Assert.Run where

import Registry.App.Prelude

import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Effect.Aff as Aff
import Registry.App.Effect.Log (LOG, LOG_EXCEPT, Log(..))
import Registry.App.Effect.Log as Log
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Run.Except
import Test.Utils as Utils

runTest :: forall a. Run (EXCEPT String + LOG + LOG_EXCEPT + AFF + EFFECT ()) a -> Aff a
runTest =
  runLog
    >>> runLogExcept
    >>> runBaseTest

runLog :: forall r a. Run (LOG + r) a -> Run r a
runLog = Run.interpret (Run.on Log._log (\(Log _ _ next) -> pure next) Run.send)

runLogExcept :: forall r a. Run (LOG_EXCEPT + AFF + r) a -> Run (AFF + r) a
runLogExcept comp = do
  let print = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces
  Run.Except.catchAt Log._logExcept (Run.liftAff <<< Aff.throwError <<< Aff.error <<< print) comp

runBaseTest :: forall a. Run (EXCEPT String + AFF + EFFECT ()) a -> Aff a
runBaseTest test =
  test
    # Run.Except.catch (Run.liftAff <<< Aff.throwError <<< Aff.error)
    # Run.runBaseAff'

shouldContain :: forall f a r. Eq a => Foldable f => f a -> a -> Run (EXCEPT String + r) Unit
shouldContain container elem =
  when (elem `Foldable.notElem` container) do
    Run.Except.throw (Utils.unsafeStringify elem <> "\n\nshould be a member of\n\n" <> Utils.unsafeStringify container)

shouldNotContain :: forall f a r. Eq a => Foldable f => f a -> a -> Run (EXCEPT String + r) Unit
shouldNotContain container elem =
  unless (elem `Foldable.notElem` container) do
    Run.Except.throw (Utils.unsafeStringify elem <> "\n\nis, but should not be, a member of\n\n" <> Utils.unsafeStringify container)
