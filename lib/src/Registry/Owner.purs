-- | Implementation of the `Owner` data type from the registry spec. An Owner
-- | identifies who can take authenticated operations for a given package.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#owner
-- |
-- | There is also a Dhall spec for this data type:
-- | https://github.com/purescript/registry-dev/blob/master/specs/v1/Owner.dhall
module Registry.Owner
  ( Owner(..)
  , codec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor

-- | A public key which can be used to authenticate package operations.
newtype Owner = Owner
  { email :: String
  , keytype :: String
  , public :: String
  }

derive instance Newtype Owner _
derive newtype instance Eq Owner

-- | A codec for encoding and decoding an `Owner` as JSON. Represented as a JSON
-- | object.
codec :: JsonCodec Owner
codec = Profunctor.wrapIso Owner $ CA.Record.object "Owner"
  { email: CA.string
  , keytype: CA.string
  , public: CA.string
  }
