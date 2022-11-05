module Registry.Operation where

import Registry.Prelude

import Registry.Json ((.:))
import Registry.Json as Json
import Registry.Location (Location)
import Registry.PackageName (PackageName)
import Registry.Version (Version)

data Operation
  = Publish PublishData
  | PackageSetUpdate PackageSetUpdateData
  | Authenticated AuthenticatedData

derive instance Eq Operation

instance RegistryJson Operation where
  encode = case _ of
    Publish fields -> Json.encode fields
    PackageSetUpdate fields -> Json.encode fields
    Authenticated fields -> Json.encode fields

  decode json = do
    let parsePublish = Publish <$> Json.decode json
    let parsePackageSetUpdate = PackageSetUpdate <$> Json.decode json
    let parseAuthenticated = Authenticated <$> Json.decode json
    parsePublish
      <|> parsePackageSetUpdate
      <|> parseAuthenticated

data AuthenticatedOperation
  = Unpublish UnpublishData
  | Transfer TransferData

derive instance Eq AuthenticatedOperation

instance RegistryJson AuthenticatedOperation where
  encode = case _ of
    Unpublish fields -> Json.encode fields
    Transfer fields -> Json.encode fields

  decode json = do
    let parseUnpublish = Unpublish <$> Json.decode json
    let parseTransfer = Transfer <$> Json.decode json
    parseUnpublish <|> parseTransfer

newtype AuthenticatedData = AuthenticatedData
  { payload :: AuthenticatedOperation
  -- We include the unparsed payload for use in verification so as to preserve
  -- any quirks of formatting that could change the input.
  , rawPayload :: String
  , signature :: Array String
  , email :: String
  }

derive instance Newtype AuthenticatedData _
derive newtype instance Eq AuthenticatedData

instance RegistryJson AuthenticatedData where
  encode (AuthenticatedData fields) = Json.encode fields
  decode json = do
    obj <- Json.decode json
    rawPayload <- obj .: "payload"
    payload <- Json.parseJson rawPayload
    signature <- obj .: "signature"
    email <- obj .: "email"
    pure $ AuthenticatedData { rawPayload, payload, signature, email }

type PublishData =
  { name :: PackageName
  , location :: Maybe Location
  , ref :: String
  , compiler :: Version
  , resolutions :: Maybe (Map PackageName Version)
  }

type UnpublishData =
  { name :: PackageName
  , version :: Version
  , reason :: String
  }

type TransferData =
  { name :: PackageName
  , newLocation :: Location
  }

type PackageSetUpdateData =
  { compiler :: Maybe Version
  , packages :: Map PackageName (Maybe Version)
  }
