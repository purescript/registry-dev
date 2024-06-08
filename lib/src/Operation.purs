-- | Data types representing operations that can be taken using the Registry API
-- | as described in the registry spec.
-- |
-- | We distinguish between operations that affect a single package (a 'package
-- | operation'), which are executed by sending a POST request to the registry
-- | HTTP API:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#package-operations
-- |
-- | and operations that affect the package sets (a 'package set operation'),
-- | which are executed using GitHub Issues:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#package-set-operations
-- |
-- | This module implements the operations, the validation that ensures operations
-- | are well-formed, and JSON codecs package managers can use to construct the
-- | requests necessary to send to the Registry API or publish in a GitHub issue.
module Registry.Operation
  ( AuthenticatedPackageOperation(..)
  , AuthenticatedData
  , PackageOperation(..)
  , PackageSetOperation(..)
  , PackageSetUpdateData
  , PublishData
  , TransferData
  , UnpublishData
  , authenticatedCodec
  , packageSetUpdateCodec
  , publishCodec
  , transferCodec
  , unpublishCodec
  ) where

import Prelude

import Codec.JSON.DecodeError as JSON.DecodeError
import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Map (Map)
import Data.Maybe (Maybe)
import JSON as JSON
import Registry.Internal.Codec as Internal.Codec
import Registry.Location (Location)
import Registry.Location as Location
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.SSH (Signature(..))
import Registry.Version (Version)
import Registry.Version as Version

-- | An operation supported by the registry HTTP API for package operations.
data PackageOperation
  = Publish PublishData
  | Authenticated AuthenticatedData

derive instance Eq PackageOperation

-- | An operation supported by the registry HTTP API for package operations and
-- | which must be authenticated.
data AuthenticatedPackageOperation
  = Unpublish UnpublishData
  | Transfer TransferData

derive instance Eq AuthenticatedPackageOperation

-- | Publish a package version to the registry.
-- |
-- | For full details, see the registry spec:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#51-publish-a-package
type PublishData =
  { name :: PackageName
  , location :: Maybe Location
  , ref :: String
  , compiler :: Version
  , resolutions :: Maybe (Map PackageName Version)
  }

-- | A codec for encoding and decoding a `Publish` operation as JSON.
publishCodec :: CJ.Codec PublishData
publishCodec = CJ.named "Publish" $ CJ.Record.object
  { name: PackageName.codec
  , location: CJ.Record.optional Location.codec
  , ref: CJ.string
  , compiler: Version.codec
  , resolutions: CJ.Record.optional (Internal.Codec.packageMap Version.codec)
  }

-- | Authenticate a package operation to send to the registry.
-- |
-- | For full details, see the registry spec:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#52-authentication
type AuthenticatedData =
  { payload :: AuthenticatedPackageOperation
  , rawPayload :: String
  , signature :: Signature
  }

-- | A codec for encoding and decoding authenticated operations as JSON.
authenticatedCodec :: CJ.Codec AuthenticatedData
authenticatedCodec = topLevelCodec
  where
  -- We first parse the payload as a simple string to use in verification so as
  -- to preserve any quirks of formatting that could change the hash of its
  -- contents. However, we also need to decode the operation itself, and so we
  -- parse that in a second pass over the input.
  topLevelCodec :: CJ.Codec AuthenticatedData
  topLevelCodec = CJ.named "Authenticated" $ Codec.codec' decode encode
    where
    decode json = do
      rep <- Codec.decode repCodec json
      payloadJson <- except $ lmap JSON.DecodeError.basic $ JSON.parse rep.payload
      operation <- Codec.decode payloadCodec payloadJson
      pure { payload: operation, rawPayload: rep.payload, signature: Signature rep.signature }

    encode { rawPayload, signature: Signature signature } =
      CJ.encode repCodec { payload: rawPayload, signature }

  repCodec :: CJ.Codec { payload :: String, signature :: String }
  repCodec = CJ.named "AuthenticatedData" $ CJ.Record.object
    { payload: CJ.string
    , signature: CJ.string
    }

  -- The only acceptable payloads for an authenticated operation are the
  -- `AuthenticatedPackageOperation`s.
  payloadCodec :: CJ.Codec AuthenticatedPackageOperation
  payloadCodec = CJ.named "AuthenticatedPackageOperation" $ Codec.codec' decode encode
    where
    decode json =
      map Unpublish (Codec.decode unpublishCodec json)
        <|> map Transfer (Codec.decode transferCodec json)

    encode = case _ of
      Unpublish unpublish -> CJ.encode unpublishCodec unpublish
      Transfer transfer -> CJ.encode transferCodec transfer

-- | Unpublish a package version from the registry. This operation must be
-- | authenticated and not all package versions can be unpublished.
-- |
-- | For full details, see the registry spec:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#53-unpublish-a-package-authenticated
type UnpublishData =
  { name :: PackageName
  , version :: Version
  , reason :: String
  }

-- | A codec for encoding and decoding an `Unpublish` operation as JSON.
unpublishCodec :: CJ.Codec UnpublishData
unpublishCodec = CJ.named "Unpublish" $ CJ.Record.object
  { name: PackageName.codec
  , version: Version.codec
  , reason: Internal.Codec.limitedString 300
  }

-- | Transfer a package from one location to another. This operation must be
-- | authenticated.
-- |
-- | For full details, see the registry spec:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#54-transfer-a-package-authenticated
type TransferData =
  { name :: PackageName
  , newLocation :: Location
  }

-- | A codec for encoding and decoding a `Transfer` operation as JSON.
transferCodec :: CJ.Codec TransferData
transferCodec = CJ.named "Transfer" $ CJ.Record.object
  { name: PackageName.codec
  , newLocation: Location.codec
  }

-- | An operation that affects the package sets.
data PackageSetOperation = PackageSetUpdate PackageSetUpdateData

derive instance Eq PackageSetOperation

-- | Submit a batch update to the most recent package set.
-- |
-- | For full details, see the registry spec:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#55-update-the-package-set
type PackageSetUpdateData =
  { compiler :: Maybe Version
  , packages :: Map PackageName (Maybe Version)
  }

-- | A codec for encoding and decoding a `PackageSetUpdate` operation as JSON.
packageSetUpdateCodec :: CJ.Codec PackageSetUpdateData
packageSetUpdateCodec = CJ.named "PackageSetUpdate" $ CJ.Record.object
  { compiler: CJ.Record.optional Version.codec
  -- We encode and decode `Nothing` values as `null` when working with versions,
  -- as the absence of the key altogether means not to update it, while the
  -- presence of `null` means to remove the package. For that reason we use the
  -- `Compat` version of the `maybe` codec.
  , packages: Internal.Codec.packageMap (CJ.Common.nullable Version.codec)
  }
