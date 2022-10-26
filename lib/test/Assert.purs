module Test.Assert where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Effect.Exception (Error)
import Test.Spec.Assertions (AnyShow(..))
import Test.Spec.Assertions as Assertions
import Test.Utils as Utils

fail :: forall m. MonadThrow Error m => String -> m Unit
fail = Assertions.fail

shouldEqual :: forall m a. MonadThrow Error m => Eq a => a -> a -> m Unit
shouldEqual a b = Assertions.shouldEqual (AnyShow a) (AnyShow b)

shouldEqualRight :: forall m e a. MonadThrow Error m => Eq a => a -> Either e a -> m Unit
shouldEqualRight a = case _ of
  Left e -> fail ("Expected Right, but received Left with value: " <> Utils.unsafeStringify e)
  Right b -> shouldEqual a b

shouldContain :: forall m f a. MonadThrow Error m => Eq a => Foldable f => f a -> a -> m Unit
shouldContain container elem =
  when (elem `Foldable.notElem` container) do
    fail (Utils.unsafeStringify elem <> "\n\nshould be a member of\n\n" <> Utils.unsafeStringify container)

shouldNotContain :: forall m f a. MonadThrow Error m => Eq a => Foldable f => f a -> a -> m Unit
shouldNotContain container elem =
  when (elem `Foldable.elem` container) do
    fail (Utils.unsafeStringify elem <> "\n\nshould not be a member of\n\n" <> Utils.unsafeStringify container)

shouldSatisfy :: forall m a. MonadThrow Error m => a -> (a -> Boolean) -> m Unit
shouldSatisfy a predicate =
  unless (predicate a) do
    fail (Utils.unsafeStringify a <> " doesn't satisfy predicate.")

shouldNotSatisfy :: forall m a. MonadThrow Error m => a -> (a -> Boolean) -> m Unit
shouldNotSatisfy a predicate =
  when (predicate a) do
    fail (Utils.unsafeStringify a <> " satisfies predicate, but should not.")
