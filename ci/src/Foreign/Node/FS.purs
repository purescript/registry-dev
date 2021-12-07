module Foreign.Node.FS
  ( mkdirSync
  , rmdirSync
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import mkdirSyncImpl :: EffectFn1 String Unit

mkdirSync :: String -> Effect Unit
mkdirSync = runEffectFn1 mkdirSyncImpl

foreign import rmdirSyncImpl :: EffectFn1 String Unit

rmdirSync :: String -> Effect Unit
rmdirSync = runEffectFn1 mkdirSyncImpl
