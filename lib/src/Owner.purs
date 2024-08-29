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

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor

-- | A public key which can be used to authenticate package operations.
newtype Owner = Owner
  { id :: Maybe String
  , keytype :: String
  , public :: String
  }

derive instance Newtype Owner _

-- | Owners are equal if their keytype and public key are equal, regardless of
-- | the id field, which is just an arbitrary string.
instance Eq Owner where
  eq (Owner o1) (Owner o2) = o1.keytype == o2.keytype && o1.public == o2.public

-- | A codec for encoding and decoding an `Owner` as JSON. Represented as a JSON
-- | object.
codec :: CJ.Codec Owner
codec = Profunctor.wrapIso Owner $ CJ.named "Owner" $ CJ.Record.object
  { id: CJ.Record.optional CJ.string
  , keytype: CJ.string
  , public: CJ.string
  }
