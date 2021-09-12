module Foreign.Node.FS
  ( mkdirSync
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import mkdirSyncImpl :: EffectFn1 String Unit

mkdirSync :: String -> Effect Unit
mkdirSync = runEffectFn1 mkdirSyncImpl
