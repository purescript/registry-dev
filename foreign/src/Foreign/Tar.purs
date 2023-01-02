module Registry.Foreign.Tar
  ( getToplevelDir
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import getToplevelDirImpl :: Fn1 String (Effect (Array String))

getToplevelDir :: forall m. MonadEffect m => String -> m (Maybe String)
getToplevelDir filename = do
  paths <- liftEffect $ runFn1 getToplevelDirImpl filename
  pure $ Array.head paths
