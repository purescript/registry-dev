-- | The Signature type used for SSH-signed payloads.
-- |
-- | This type is separated from Registry.SSH so that modules needing only
-- | the Signature newtype (such as Registry.Operation) do not transitively
-- | depend on the ssh2 FFI, which is Node.js-only. Browser consumers such
-- | as the dashboard depend on this module, so it must stay free of
-- | Node.js-only FFI.
module Registry.SSH.Signature
  ( Signature(..)
  ) where

import Prelude

import Data.Newtype (class Newtype)

-- | A hex-encoded SSH signature
newtype Signature = Signature String

derive instance Newtype Signature _
derive newtype instance Eq Signature
