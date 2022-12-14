module Registry.Foreign.Tar
  ( getToplevelDir
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Effect (Effect)

foreign import getToplevelDirImpl :: Fn1 String (Effect (Array String))

getToplevelDir :: String -> Effect (Maybe String)
getToplevelDir filename = do
  paths <- runFn1 getToplevelDirImpl filename
  pure $ Array.head paths
