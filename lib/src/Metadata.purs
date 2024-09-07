-- | Implementation of the `Metadata` data type as described in the registry
-- | spec. The metadata for a particular package records all versions that have
-- | been published or unpublished as well as any package owners and the last-
-- | used location of the source code.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#metadata
-- |
-- | There is also a Dhall spec for this data type:
-- | https://github.com/purescript/registry-dev/blob/master/specs/v1/Metadata.dhall
-- |
-- | Package metadata can be found at in the registry repository:
-- | https://github.com/purescript/registry/blob/main/metadata
module Registry.Metadata
  ( Metadata(..)
  , PublishedMetadata
  , UnpublishedMetadata
  , codec
  , publishedMetadataCodec
  , unpublishedMetadataCodec
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Codec.JSON.Strict as CJS
import Data.DateTime (DateTime)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec
import Registry.Location (Location)
import Registry.Location as Location
import Registry.Owner (Owner)
import Registry.Owner as Owner
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256
import Registry.Version (Version)

-- | A record of all published and unpublished versions of a package, along with
-- | the last-used location and any owners (public keys) authorized to take
-- | authenticated package operations on behalf of the package.
newtype Metadata = Metadata
  { location :: Location
  , owners :: Maybe (NonEmptyArray Owner)
  , published :: Map Version PublishedMetadata
  , unpublished :: Map Version UnpublishedMetadata
  }

derive instance Newtype Metadata _
derive instance Eq Metadata

-- | A codec for encoding and decoding a `Metadata` value as JSON. Represented
-- | as a JSON object. Keys are explicitly ordered.
codec :: CJ.Codec Metadata
codec = Profunctor.wrapIso Metadata $ CJ.named "Metadata" $ CJS.objectStrict
  $ CJS.recordProp @"location" Location.codec
  $ CJS.recordPropOptional @"owners" (CJ.Common.nonEmptyArray Owner.codec)
  $ CJS.recordProp @"published" (Internal.Codec.versionMap publishedMetadataCodec)
  $ CJS.recordProp @"unpublished" (Internal.Codec.versionMap unpublishedMetadataCodec)
  $ CJS.record

-- | Metadata about a published package version.
-- |
-- | NOTE: The `ref` field is UNSPECIFIED and WILL BE REMOVED in the future. Do
-- | not rely on its presence!
type PublishedMetadata =
  { bytes :: Number
  , hash :: Sha256
  , publishedTime :: DateTime
  , ref :: String
  }

publishedMetadataCodec :: CJ.Codec PublishedMetadata
publishedMetadataCodec = CJ.named "PublishedMetadata" $ CJ.Record.object
  { bytes: CJ.number
  , hash: Sha256.codec
  , publishedTime: Internal.Codec.iso8601DateTime
  , ref: CJ.string
  }

-- | Metadata about an unpublished package version.
type UnpublishedMetadata =
  { publishedTime :: DateTime
  , reason :: String
  , unpublishedTime :: DateTime
  }

unpublishedMetadataCodec :: CJ.Codec UnpublishedMetadata
unpublishedMetadataCodec = CJ.named "UnpublishedMetadata" $ CJ.Record.object
  { publishedTime: Internal.Codec.iso8601DateTime
  , reason: Internal.Codec.limitedString 300
  , unpublishedTime: Internal.Codec.iso8601DateTime
  }
