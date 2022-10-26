module Test.Assert where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core as Argonaut
import Data.Either (Either(..))
import Effect.Exception (Error)
import Test.Spec.Assertions (AnyShow(..))
import Test.Spec.Assertions as Assertions
import Unsafe.Coerce (unsafeCoerce)

shouldEqual :: forall m a. MonadThrow Error m => Eq a => a -> a -> m Unit
shouldEqual a b = Assertions.shouldEqual (AnyShow a) (AnyShow b)

shouldEqualRight :: forall m e a. MonadThrow Error m => Eq a => a -> Either e a -> m Unit
shouldEqualRight a = case _ of
  Left e -> Assertions.fail (Argonaut.stringify (unsafeCoerce e :: Argonaut.Json))
  Right b -> Assertions.shouldEqual (AnyShow a) (AnyShow b)
