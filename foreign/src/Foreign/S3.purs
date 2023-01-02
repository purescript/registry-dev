module Registry.Foreign.S3
  ( ACL(..)
  , S3
  , Space
  , SpaceKey
  , connect
  , deleteObject
  , listObjects
  , putObject
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.JSDate (JSDate)
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.Buffer (Buffer)

foreign import data S3 :: Type

type Space = { conn :: S3, bucket :: String }

type SpaceKey = { key :: String, secret :: String }

foreign import connectImpl :: EffectFn2 SpaceKey String S3

connect :: forall m. MonadEffect m => SpaceKey -> String -> String -> m Space
connect key region bucket = do
  conn <- liftEffect $ runEffectFn2 connectImpl key region
  pure { bucket, conn }

-- Add more params as needed:
-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#listObjectsV2-property
type JSListParams =
  { "Bucket" :: String
  , "Prefix" :: String
  }

type JSListResponse =
  { "Key" :: String
  , "LastModified" :: JSDate
  , "ETag" :: String
  , "Size" :: Int
  , "StorageClass" :: String
  }

type ListParams = { prefix :: String }
type ListResponse = { key :: String, size :: Int, eTag :: String }

foreign import listObjectsImpl :: EffectFn2 S3 JSListParams (Promise (Array JSListResponse))

listObjects :: forall m. MonadAff m => Space -> ListParams -> m (Array ListResponse)
listObjects space params = do
  let jsParams = { "Bucket": space.bucket, "Prefix": params.prefix }
  jsObjs <- liftAff $ Promise.toAffE (runEffectFn2 listObjectsImpl space.conn jsParams)
  for jsObjs \obj -> pure { key: obj."Key", size: obj."Size", eTag: obj."ETag" } -- TODO: pull more props if needed

-- Add more params as needed:
-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#putObject-property
type JSPutParams =
  { "Bucket" :: String
  , "Key" :: String
  , "Body" :: Buffer -- TODO: the SDK also accepts a string here, but we don't need that right now
  , "ACL" :: String -- SDK supports more, but DO only supports "private" and "public-read"
  }

type JSPutResponse = { "ETag" :: String }

data ACL = Private | PublicRead
type PutParams = { key :: String, body :: Buffer, acl :: ACL }
type PutResponse = { eTag :: String }

foreign import putObjectImpl :: EffectFn2 S3 JSPutParams (Promise JSPutResponse)

putObject :: forall m. MonadAff m => Space -> PutParams -> m PutResponse
putObject space params = do
  let
    jsACL = case params.acl of
      Private -> "private"
      PublicRead -> "public-read"
  let jsParams = { "Bucket": space.bucket, "Key": params.key, "Body": params.body, "ACL": jsACL }
  res <- liftAff $ Promise.toAffE (runEffectFn2 putObjectImpl space.conn jsParams)
  pure { eTag: res."ETag" }

-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#deleteObject-property
type JSDeleteParams =
  { "Bucket" :: String
  , "Key" :: String
  }

type JSDeleteResponse =
  { "DeleteMarker" :: Boolean
  , "VersionId" :: String
  , "RequestCharged" :: String
  }

foreign import deleteObjectImpl :: EffectFn2 S3 JSDeleteParams (Promise JSDeleteResponse)

type DeleteParams =
  { key :: String
  }

type DeleteResponse =
  { deleteMarker :: Boolean
  , versionId :: String
  , requestCharged :: String
  }

deleteObject :: forall m. MonadAff m => Space -> DeleteParams -> m DeleteResponse
deleteObject space params = do
  let jsParams = { "Bucket": space.bucket, "Key": params.key }
  result <- liftAff $ Promise.toAffE (runEffectFn2 deleteObjectImpl space.conn jsParams)
  pure
    { deleteMarker: result."DeleteMarker"
    , versionId: result."VersionId"
    , requestCharged: result."RequestCharged"
    }
