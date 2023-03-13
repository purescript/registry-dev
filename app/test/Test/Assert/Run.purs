module Registry.Test.Assert.Run where

import Registry.App.Prelude

import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Effect.Exception as Exception
import Registry.App.Effect.Log (LOG, Log(..))
import Registry.App.Effect.Log as Log
import Registry.Test.Utils as Utils
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

runTest :: forall a. Run (LOG + EXCEPT String + AFF + EFFECT ()) a -> Aff a
runTest =
  Log.interpret (\(Log _ _ next) -> pure next)
    >>> Except.catch (Exception.throw >>> Run.liftEffect)
    >>> Run.runBaseAff'

shouldContain :: forall f a r. Eq a => Foldable f => f a -> a -> Run (EXCEPT String + r) Unit
shouldContain container elem =
  when (elem `Foldable.notElem` container) do
    Except.throw (Utils.unsafeStringify elem <> "\n\nshould be a member of\n\n" <> Utils.unsafeStringify container)

shouldNotContain :: forall f a r. Eq a => Foldable f => f a -> a -> Run (EXCEPT String + r) Unit
shouldNotContain container elem =
  unless (elem `Foldable.notElem` container) do
    Except.throw (Utils.unsafeStringify elem <> "\n\nis, but should not be, a member of\n\n" <> Utils.unsafeStringify container)
