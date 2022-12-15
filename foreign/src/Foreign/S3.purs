module Registry.Foreign.S3
  ( connect
  , listObjects
  , putObject
  , deleteObject
  , S3
  , Space
  , ACL(..)
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.JSDate (JSDate)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)

foreign import data S3 :: Type

type Space = { conn :: S3, bucket :: String }

foreign import connectImpl :: Fn1 String (Effect S3)

connect :: String -> String -> Aff Space
connect region bucket = do
  conn <- liftEffect $ runFn1 connectImpl region
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

foreign import listObjectsImpl :: Fn2 S3 JSListParams (Effect (Promise (Array JSListResponse)))

listObjects :: Space -> ListParams -> Aff (Array ListResponse)
listObjects space params = do
  let jsParams = { "Bucket": space.bucket, "Prefix": params.prefix }
  (jsObjs :: Array JSListResponse) <- Promise.toAffE (runFn2 listObjectsImpl space.conn jsParams)
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

foreign import putObjectImpl :: Fn2 S3 JSPutParams (Effect (Promise JSPutResponse))

putObject :: Space -> PutParams -> Aff PutResponse
putObject space params = do
  let
    jsACL = case params.acl of
      Private -> "private"
      PublicRead -> "public-read"
  let jsParams = { "Bucket": space.bucket, "Key": params.key, "Body": params.body, "ACL": jsACL }
  res <- Promise.toAffE (runFn2 putObjectImpl space.conn jsParams)
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

foreign import deleteObjectImpl :: Fn2 S3 JSDeleteParams (Effect (Promise JSDeleteResponse))

type DeleteParams =
  { key :: String
  }

type DeleteResponse =
  { deleteMarker :: Boolean
  , versionId :: String
  , requestCharged :: String
  }

deleteObject :: Space -> DeleteParams -> Aff DeleteResponse
deleteObject space params = do
  let jsParams = { "Bucket": space.bucket, "Key": params.key }
  result <- Promise.toAffE (runFn2 deleteObjectImpl space.conn jsParams)
  pure
    { deleteMarker: result."DeleteMarker"
    , versionId: result."VersionId"
    , requestCharged: result."RequestCharged"
    }
