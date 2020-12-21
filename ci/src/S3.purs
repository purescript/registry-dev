module S3 (
  connect,
  listObjects,
  putObject,
  S3
) where

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.JSDate (JSDate)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Buffer (Buffer)

foreign import data S3 :: Type

foreign import connectImpl :: Fn1 String (Effect S3)
connect :: String -> Effect S3
connect = runFn1 connectImpl

-- Add more params as needed:
-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#listObjectsV2-property
type ListParams =
  { "Bucket" :: String
  , "Prefix" :: String
  }

type ListResponse =
  { "Key" :: String
  , "LastModified" :: JSDate
  , "ETag" :: String
  , "Size" :: Int
  , "StorageClass" :: String
  }

foreign import listObjectsImpl :: Fn2 S3 ListParams (Effect (Promise (Array ListResponse)))
listObjects :: S3 -> ListParams -> Aff (Array ListResponse)
listObjects s3 params = Promise.toAffE (runFn2 listObjectsImpl s3 params)

-- Add more params as needed:
-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#putObject-property
type PutParams =
  { "Bucket" :: String
  , "Key" :: String
  , "Body" :: Buffer -- TODO: the SDK also accepts a string here
  , "ACL" :: String -- TODO: this should really be an enum
  }

type PutResponse = { "ETag" :: String }

foreign import putObjectImpl :: Fn2 S3 PutParams (Effect (Promise PutResponse))
putObject :: S3 -> PutParams -> Aff PutResponse
putObject s3 params = Promise.toAffE (runFn2 putObjectImpl s3 params)
