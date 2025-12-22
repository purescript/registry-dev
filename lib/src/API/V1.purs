module Registry.API.V1
  ( JobCreatedResponse
  , JobId(..)
  , JobInfo
  , JobType(..)
  , Job(..)
  , LogLevel(..)
  , LogLine
  , MatrixJobData
  , PackageSetJobData
  , PublishJobData
  , Route(..)
  , TransferJobData
  , UnpublishJobData
  , jobInfo
  , jobCodec
  , jobCreatedResponseCodec
  , logLevelFromPriority
  , logLevelToPriority
  , printJobType
  , printLogLevel
  , routes
  ) where

import Prelude hiding ((/))

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Alt ((<|>))
import Control.Monad.Except (Except, except)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Codec.JSON.Sum as CJ.Sum
import Data.DateTime (DateTime)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime as DateTime
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor
import Data.Symbol (class IsSymbol)
import Data.Symbol as Symbol
import JSON (JSON)
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Operation (AuthenticatedData, PackageSetOperation, PublishData)
import Registry.Operation as Operation
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Routing
import Routing.Duplex.Generic as RoutingG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Type.Proxy (Proxy(..))

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

data Job
  = PublishJob PublishJobData
  | UnpublishJob UnpublishJobData
  | TransferJob TransferJobData
  | MatrixJob MatrixJobData
  | PackageSetJob PackageSetJobData

type JobInfo r =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , logs :: Array LogLine
  | r
  }

type PublishJobData = JobInfo
  ( packageName :: PackageName
  , packageVersion :: Version
  , payload :: PublishData
  , jobType :: Proxy "publish"
  )

type UnpublishJobData = JobInfo
  ( packageName :: PackageName
  , packageVersion :: Version
  , payload :: AuthenticatedData
  , jobType :: Proxy "unpublish"
  )

type TransferJobData = JobInfo
  ( packageName :: PackageName
  , payload :: AuthenticatedData
  , jobType :: Proxy "transfer"
  )

type MatrixJobData = JobInfo
  ( packageName :: PackageName
  , packageVersion :: Version
  , compilerVersion :: Version
  , payload :: Map PackageName Version
  , jobType :: Proxy "matrix"
  )

type PackageSetJobData = JobInfo
  ( payload :: PackageSetOperation
  , jobType :: Proxy "packageset"
  )

jobCodec :: CJ.Codec Job
jobCodec = Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError Job
  decode json =
    do
      map PublishJob (Codec.decode publishJobDataCodec json)
      <|> map UnpublishJob (Codec.decode unpublishJobDataCodec json)
      <|> map TransferJob (Codec.decode transferJobDataCodec json)
      <|> map MatrixJob (Codec.decode matrixJobDataCodec json)
      <|> map PackageSetJob (Codec.decode packageSetJobDataCodec json)

  encode :: Job -> JSON
  encode = case _ of
    PublishJob j -> CJ.encode publishJobDataCodec j
    UnpublishJob j -> CJ.encode unpublishJobDataCodec j
    TransferJob j -> CJ.encode transferJobDataCodec j
    MatrixJob j -> CJ.encode matrixJobDataCodec j
    PackageSetJob j -> CJ.encode packageSetJobDataCodec j

publishJobDataCodec :: CJ.Codec PublishJobData
publishJobDataCodec = CJ.named "PublishJob" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: symbolCodec (Proxy :: _ "publish")
  , createdAt: Internal.Codec.iso8601DateTime
  , startedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  , packageName: PackageName.codec
  , packageVersion: Version.codec
  , payload: Operation.publishCodec
  }

symbolCodec :: forall sym. IsSymbol sym => Proxy sym -> CJ.Codec (Proxy sym)
symbolCodec _ = Codec.codec' decode encode
  where
  decode json = except do
    symbol <- CJ.decode CJ.string json
    let expected = Symbol.reflectSymbol (Proxy :: _ sym)
    case symbol == expected of
      false -> Left $ CJ.DecodeError.basic
        $ "Tried to decode symbol '" <> symbol <> "' as '" <> expected <> "'"
      true -> Right (Proxy :: _ sym)
  encode = CJ.encode CJ.string <<< Symbol.reflectSymbol

unpublishJobDataCodec :: CJ.Codec UnpublishJobData
unpublishJobDataCodec = CJ.named "UnpublishJob" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: symbolCodec (Proxy :: _ "unpublish")
  , createdAt: Internal.Codec.iso8601DateTime
  , startedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  , packageName: PackageName.codec
  , packageVersion: Version.codec
  , payload: Operation.authenticatedCodec
  }

transferJobDataCodec :: CJ.Codec TransferJobData
transferJobDataCodec = CJ.named "TransferJob" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: symbolCodec (Proxy :: _ "transfer")
  , createdAt: Internal.Codec.iso8601DateTime
  , startedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  , packageName: PackageName.codec
  , payload: Operation.authenticatedCodec
  }

matrixJobDataCodec :: CJ.Codec MatrixJobData
matrixJobDataCodec = CJ.named "MatrixJob" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: symbolCodec (Proxy :: _ "matrix")
  , createdAt: Internal.Codec.iso8601DateTime
  , startedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  , packageName: PackageName.codec
  , packageVersion: Version.codec
  , compilerVersion: Version.codec
  , payload: Internal.Codec.packageMap Version.codec
  }

packageSetJobDataCodec :: CJ.Codec PackageSetJobData
packageSetJobDataCodec = CJ.named "PackageSetJob" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: symbolCodec (Proxy :: _ "packageset")
  , createdAt: Internal.Codec.iso8601DateTime
  , startedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  , payload: Operation.packageSetOperationCodec
  }

jobInfo :: Job -> JobInfo ()
jobInfo = case _ of
  PublishJob { jobId, createdAt, startedAt, finishedAt, success, logs } ->
    { jobId, createdAt, startedAt, finishedAt, success, logs }
  UnpublishJob { jobId, createdAt, startedAt, finishedAt, success, logs } ->
    { jobId, createdAt, startedAt, finishedAt, success, logs }
  TransferJob { jobId, createdAt, startedAt, finishedAt, success, logs } ->
    { jobId, createdAt, startedAt, finishedAt, success, logs }
  MatrixJob { jobId, createdAt, startedAt, finishedAt, success, logs } ->
    { jobId, createdAt, startedAt, finishedAt, success, logs }
  PackageSetJob { jobId, createdAt, startedAt, finishedAt, success, logs } ->
    { jobId, createdAt, startedAt, finishedAt, success, logs }

newtype JobId = JobId String

derive instance Newtype JobId _

jobIdCodec :: CJ.Codec JobId
jobIdCodec = Profunctor.wrapIso JobId CJ.string

data JobType
  = PublishJobType
  | UnpublishJobType
  | TransferJobType
  | MatrixJobType
  | PackageSetJobType

derive instance Eq JobType

printJobType :: JobType -> String
printJobType = case _ of
  PublishJobType -> "publish"
  UnpublishJobType -> "unpublish"
  TransferJobType -> "transfer"
  MatrixJobType -> "matrix"
  PackageSetJobType -> "packageset"

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
