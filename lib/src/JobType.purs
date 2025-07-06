module Registry.JobType where

import Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Sum as CJ.Sum
import Data.Either (Either(..), hush)

data JobType = PublishJob | UnpublishJob | TransferJob

derive instance Eq JobType

parse :: String -> Either String JobType
parse = case _ of
  "publish" -> Right PublishJob
  "unpublish" -> Right UnpublishJob
  "transfer" -> Right TransferJob
  j -> Left $ "Invalid job type " <> show j

print :: JobType -> String
print = case _ of
  PublishJob -> "publish"
  UnpublishJob -> "unpublish"
  TransferJob -> "transfer"

codec :: CJ.Codec JobType
codec = CJ.Sum.enumSum print (hush <<< parse)
