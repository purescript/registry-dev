-- Adapted from:
-- https://github.com/oreshinya/purescript-crypto/blob/v4.0.0/src/Node/Crypto/Hash.purs
module Foreign.Node.Crypto
  ( Hash
  , createHash
  , updateHash
  , digestHash
  ) where

import Effect (Effect)
import Node.Buffer (Buffer)

foreign import data Hash :: Type

foreign import createHash :: String -> Effect Hash

foreign import updateHash :: Buffer -> Hash -> Effect Hash

foreign import digestHash :: Hash -> Effect Buffer
