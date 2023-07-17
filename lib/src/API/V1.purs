module Registry.API.V1 where

import Prelude hiding ((/))

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CA.Sum
import Data.DateTime (DateTime)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime as DateTime
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Routing
import Routing.Duplex.Generic as RoutingG
import Routing.Duplex.Generic.Syntax ((/), (?))

data Route
  = Publish
  | Unpublish
  | Transfer
  | Jobs
  | Job JobId
      { level :: Maybe LogLevel
      , since :: Maybe DateTime
      }

derive instance Generic Route _

routes :: RouteDuplex' Route
routes = Routing.root $ Routing.prefix "api" $ Routing.prefix "v1" $ RoutingG.sum
  { "Publish": "publish" / RoutingG.noArgs
  , "Unpublish": "unpublish" / RoutingG.noArgs
  , "Transfer": "transfer" / RoutingG.noArgs
  , "Jobs": "jobs" / RoutingG.noArgs
  , "Job": "jobs" /
      ( jobIdS ?
          { level: Routing.optional <<< logLevelP <<< Routing.string
          , since: Routing.optional <<< timestampP <<< Routing.string
          }
      )
  }

jobIdS :: RouteDuplex' JobId
jobIdS = _Newtype Routing.segment

logLevelP :: RouteDuplex' String -> RouteDuplex' LogLevel
logLevelP = Routing.as printLogLevel parseLogLevel

timestampP :: RouteDuplex' String -> RouteDuplex' DateTime
timestampP = Routing.as printTimestamp parseTimestamp
  where
  printTimestamp t = DateTime.format Internal.Format.iso8601DateTime t
  parseTimestamp s = DateTime.unformat Internal.Format.iso8601DateTime s

type JobCreatedResponse = { jobId :: JobId }

jobCreatedResponseCodec :: JsonCodec JobCreatedResponse
jobCreatedResponseCodec = CA.Record.object "JobCreatedResponse" { jobId: jobIdCodec }

type Job =
  { jobId :: JobId
  , jobType :: JobType
  , packageName :: PackageName
  , ref :: String
  , createdAt :: DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , logs :: Array LogLine
  }

jobCodec :: JsonCodec Job
jobCodec = CA.Record.object "Job"
  { jobId: jobIdCodec
  , jobType: jobTypeCodec
  , packageName: PackageName.codec
  , ref: CA.string
  , createdAt: Internal.Codec.iso8601DateTime
  , finishedAt: CAR.optional Internal.Codec.iso8601DateTime
  , success: CA.boolean
  , logs: CA.array logLineCodec
  }

newtype JobId = JobId String

derive instance Newtype JobId _

jobIdCodec :: JsonCodec JobId
jobIdCodec = Profunctor.wrapIso JobId CA.string

data JobType = PublishJob | UnpublishJob | TransferJob

derive instance Eq JobType

parseJobType :: String -> Either String JobType
parseJobType = case _ of
  "publish" -> Right PublishJob
  "unpublish" -> Right UnpublishJob
  "transfer" -> Right TransferJob
  j -> Left $ "Invalid job type " <> show j

printJobType :: JobType -> String
printJobType = case _ of
  PublishJob -> "publish"
  UnpublishJob -> "unpublish"
  TransferJob -> "transfer"

jobTypeCodec :: JsonCodec JobType
jobTypeCodec = CA.Sum.enumSum printJobType (hush <<< parseJobType)

type LogLine =
  { level :: LogLevel
  , message :: String
  , jobId :: JobId
  , timestamp :: DateTime
  }

logLineCodec :: JsonCodec LogLine
logLineCodec = CA.Record.object "LogLine"
  { level: CA.Sum.enumSum printLogLevel (hush <<< parseLogLevel)
  , message: CA.string
  , jobId: jobIdCodec
  , timestamp: Internal.Codec.iso8601DateTime
  }

data LogLevel = Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

printLogLevel :: LogLevel -> String
printLogLevel = case _ of
  Debug -> "DEBUG"
  Info -> "INFO"
  Warn -> "WARN"
  Error -> "ERROR"

-- These numbers are not consecutive so that we can insert new log levels if need be
logLevelToPriority :: LogLevel -> Int
logLevelToPriority = case _ of
  Debug -> 0
  Info -> 10
  Warn -> 20
  Error -> 30

logLevelFromPriority :: Int -> Either String LogLevel
logLevelFromPriority = case _ of
  0 -> Right Debug
  10 -> Right Info
  20 -> Right Warn
  30 -> Right Error
  other -> Left $ "Invalid log level priority: " <> show other

parseLogLevel :: String -> Either String LogLevel
parseLogLevel = case _ of
  "DEBUG" -> Right Debug
  "INFO" -> Right Info
  "WARN" -> Right Warn
  "ERROR" -> Right Error
  other -> Left $ "Invalid log level: " <> other
