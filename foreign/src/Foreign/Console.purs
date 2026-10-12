module Registry.Foreign.Console (warnError) where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)

foreign import warnError :: Error -> Effect Unit
