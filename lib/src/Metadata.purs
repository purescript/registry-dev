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

import Control.Alt ((<|>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.Either (Either(..))
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
import Registry.Version as Version
import Type.Proxy (Proxy(..))

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
codec :: JsonCodec Metadata
codec = Profunctor.wrapIso Metadata $ CA.object "Metadata"
  $ CA.recordProp (Proxy :: _ "location") Location.codec
  $ CA.recordPropOptional (Proxy :: _ "owners") (CA.Common.nonEmptyArray Owner.codec)
  $ CA.recordProp (Proxy :: _ "published") (Internal.Codec.versionMap publishedMetadataCodec)
  $ CA.recordProp (Proxy :: _ "unpublished") (Internal.Codec.versionMap unpublishedMetadataCodec)
  $ CA.record

-- | Metadata about a published package version.
-- |
-- | NOTE: The `ref` field is UNSPECIFIED and WILL BE REMOVED in the future. Do
-- | not rely on its presence!
type PublishedMetadata =
  { bytes :: Number
  , compilers :: Either Version (NonEmptyArray Version)
  , hash :: Sha256
  , publishedTime :: DateTime

  -- UNSPECIFIED: Will be removed in the future.
  , ref :: String
  }

publishedMetadataCodec :: JsonCodec PublishedMetadata
publishedMetadataCodec = CA.Record.object "PublishedMetadata"
  { bytes: CA.number
  , compilers: compilersCodec
  , hash: Sha256.codec
  , publishedTime: Internal.Codec.iso8601DateTime
  , ref: CA.string
  }
  where
  compilersCodec :: JsonCodec (Either Version (NonEmptyArray Version))
  compilersCodec = CA.codec' decode encode
    where
    decode json =
      map Left (CA.decode Version.codec json)
        <|> map Right (CA.decode (CA.Common.nonEmptyArray Version.codec) json)

    encode = case _ of
      Left version -> CA.encode Version.codec version
      Right versions -> CA.encode (CA.Common.nonEmptyArray Version.codec) versions

-- | Metadata about an unpublished package version.
type UnpublishedMetadata =
  { publishedTime :: DateTime
  , reason :: String
  , unpublishedTime :: DateTime
  }

unpublishedMetadataCodec :: JsonCodec UnpublishedMetadata
unpublishedMetadataCodec = CA.Record.object "UnpublishedMetadata"
  { publishedTime: Internal.Codec.iso8601DateTime
  , reason: Internal.Codec.limitedString 300
  , unpublishedTime: Internal.Codec.iso8601DateTime
  }
