module Test.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec.Assertions (AnyShow(..))
import Test.Spec.Assertions as Assertions

shouldEqual :: forall m a. MonadThrow Error m => Eq a => a -> a -> m Unit
shouldEqual a b = Assertions.shouldEqual (AnyShow a) (AnyShow b)
