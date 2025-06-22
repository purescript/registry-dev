module Registry.API.V1 where

import Prelude hiding ((/))

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Codec.JSON.Sum as CJ.Sum
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
import Registry.JobType as JobType
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
  | Job JobId { level :: Maybe LogLevel, since :: Maybe DateTime }
  | Status

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
  , "Status": "status" / RoutingG.noArgs
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

jobCreatedResponseCodec :: CJ.Codec JobCreatedResponse
jobCreatedResponseCodec = CJ.named "JobCreatedResponse" $ CJ.Record.object { jobId: jobIdCodec }

type Job =
  { jobId :: JobId
  , jobType :: JobType.JobType
  , packageName :: PackageName
  , createdAt :: DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , logs :: Array LogLine
  }

jobCodec :: CJ.Codec Job
jobCodec = CJ.named "Job" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: JobType.codec
  , packageName: PackageName.codec
  , createdAt: Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  }

newtype JobId = JobId String

derive instance Newtype JobId _

jobIdCodec :: CJ.Codec JobId
jobIdCodec = Profunctor.wrapIso JobId CJ.string

type LogLine =
  { level :: LogLevel
  , message :: String
  , jobId :: JobId
  , timestamp :: DateTime
  }

logLineCodec :: CJ.Codec LogLine
logLineCodec = CJ.named "LogLine" $ CJ.Record.object
  { level: CJ.Sum.enumSum printLogLevel (hush <<< parseLogLevel)
  , message: CJ.string
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
