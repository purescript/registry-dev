-- | Types, codecs, and routes for the Registry HTTP API (v1).
module Registry.API.V1
  ( JobCreatedResponse
  , JobError
  , JobErrorCode(..)
  , JobId(..)
  , JobInfo
  , JobType(..)
  , Job(..)
  , LogLevel(..)
  , LogLine
  , MatrixJobData
  , PackageSetJobData
  , PublishJobDisposition(..)
  , PublishJobData
  , PublishJobResponse
  , PublishSubmissionDisposition(..)
  , Route(..)
  , SortOrder(..)
  , TransferJobData
  , UnpublishJobData
  , jobInfo
  , jobCodec
  , jobCreatedResponseCodec
  , logLevelFromPriority
  , logLevelToPriority
  , parseJobErrorCode
  , parsePublishJobDisposition
  , printJobErrorCode
  , printJobType
  , printLogLevel
  , printPublishJobDisposition
  , printPublishSubmissionDisposition
  , printSortOrder
  , publishJobResponseCodec
  , routes
  ) where

import Prelude hiding ((/))

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Alt ((<|>))
import Control.Monad.Except (Except, except)
import Data.Bifunctor (lmap)
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
  | PackageSets
  | Jobs { since :: Maybe DateTime, until :: Maybe DateTime, order :: Maybe SortOrder, include_completed :: Maybe Boolean }
  | Job JobId { level :: Maybe LogLevel, since :: Maybe DateTime, until :: Maybe DateTime, order :: Maybe SortOrder }
  | Status

derive instance Generic Route _

routes :: RouteDuplex' Route
routes = Routing.root $ Routing.prefix "api" $ Routing.prefix "v1" $ RoutingG.sum
  { "Publish": "publish" / RoutingG.noArgs
  , "Unpublish": "unpublish" / RoutingG.noArgs
  , "Transfer": "transfer" / RoutingG.noArgs
  , "PackageSets": "package-sets" / RoutingG.noArgs
  , "Jobs": "jobs" ?
      { since: Routing.optional <<< timestampP <<< Routing.string
      , until: Routing.optional <<< timestampP <<< Routing.string
      , order: Routing.optional <<< sortOrderP <<< Routing.string
      , include_completed: Routing.optional <<< Routing.boolean
      }
  , "Job": "jobs" /
      ( jobIdS ?
          { level: Routing.optional <<< logLevelP <<< Routing.string
          , since: Routing.optional <<< timestampP <<< Routing.string
          , until: Routing.optional <<< timestampP <<< Routing.string
          , order: Routing.optional <<< sortOrderP <<< Routing.string
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

data SortOrder = ASC | DESC

derive instance Eq SortOrder

printSortOrder :: SortOrder -> String
printSortOrder = case _ of
  ASC -> "ASC"
  DESC -> "DESC"

parseSortOrder :: String -> Either String SortOrder
parseSortOrder = case _ of
  "ASC" -> Right ASC
  "DESC" -> Right DESC
  other -> Left $ "Invalid sort order: " <> other

sortOrderP :: RouteDuplex' String -> RouteDuplex' SortOrder
sortOrderP = Routing.as printSortOrder parseSortOrder

type JobCreatedResponse = { jobId :: JobId }

jobCreatedResponseCodec :: CJ.Codec JobCreatedResponse
jobCreatedResponseCodec = CJ.named "JobCreatedResponse" $ CJ.Record.object { jobId: jobIdCodec }

-- | The outcome of submitting a publish request. The field is optional in the
-- | response codec so clients remain able to decode responses from older
-- | registry servers, but current servers always provide it.
data PublishSubmissionDisposition
  = Created
  | DuplicateActive
  | AlreadyPublishedSubmission

derive instance Eq PublishSubmissionDisposition

printPublishSubmissionDisposition :: PublishSubmissionDisposition -> String
printPublishSubmissionDisposition = case _ of
  Created -> "created"
  DuplicateActive -> "duplicate-active"
  AlreadyPublishedSubmission -> "already-published"

publishSubmissionDispositionCodec :: CJ.Codec PublishSubmissionDisposition
publishSubmissionDispositionCodec = CJ.named "PublishSubmissionDisposition" $ Codec.codec' decode encode
  where
  decode json = except do
    value <- CJ.decode CJ.string json
    case value of
      "created" -> Right Created
      "duplicate-active" -> Right DuplicateActive
      "already-published" -> Right AlreadyPublishedSubmission
      _ -> Left $ CJ.DecodeError.basic $ "Invalid publish submission disposition: " <> value

  encode = CJ.encode CJ.string <<< printPublishSubmissionDisposition

type PublishJobResponse =
  { jobId :: JobId
  , disposition :: Maybe PublishSubmissionDisposition
  }

publishJobResponseCodec :: CJ.Codec PublishJobResponse
publishJobResponseCodec = CJ.named "PublishJobResponse" $ CJ.Record.object
  { jobId: jobIdCodec
  , disposition: CJ.Record.optional publishSubmissionDispositionCodec
  }

-- | The successful terminal outcome of a publish job.
data PublishJobDisposition
  = Published
  | AlreadyPublished

derive instance Eq PublishJobDisposition

printPublishJobDisposition :: PublishJobDisposition -> String
printPublishJobDisposition = case _ of
  Published -> "published"
  AlreadyPublished -> "already-published"

parsePublishJobDisposition :: String -> Either String PublishJobDisposition
parsePublishJobDisposition = case _ of
  "published" -> Right Published
  "already-published" -> Right AlreadyPublished
  value -> Left $ "Invalid publish job disposition: " <> value

publishJobDispositionCodec :: CJ.Codec PublishJobDisposition
publishJobDispositionCodec = CJ.named "PublishJobDisposition" $ Codec.codec' decode encode
  where
  decode json = do
    value <- Codec.decode CJ.string json
    except $ lmap CJ.DecodeError.basic $ parsePublishJobDisposition value
  encode = CJ.encode CJ.string <<< printPublishJobDisposition

data JobErrorCode
  = JobFailed
  | JobTimedOut

derive instance Eq JobErrorCode

printJobErrorCode :: JobErrorCode -> String
printJobErrorCode = case _ of
  JobFailed -> "job-failed"
  JobTimedOut -> "job-timeout"

parseJobErrorCode :: String -> Either String JobErrorCode
parseJobErrorCode = case _ of
  "job-failed" -> Right JobFailed
  "job-timeout" -> Right JobTimedOut
  value -> Left $ "Invalid job error code: " <> value

jobErrorCodeCodec :: CJ.Codec JobErrorCode
jobErrorCodeCodec = CJ.named "JobErrorCode" $ Codec.codec' decode encode
  where
  decode json = do
    value <- Codec.decode CJ.string json
    except $ lmap CJ.DecodeError.basic $ parseJobErrorCode value
  encode = CJ.encode CJ.string <<< printJobErrorCode

type JobError =
  { code :: JobErrorCode
  , message :: String
  }

jobErrorCodec :: CJ.Codec JobError
jobErrorCodec = CJ.named "JobError" $ CJ.Record.object
  { code: jobErrorCodeCodec
  , message: CJ.string
  }

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
  , disposition :: Maybe PublishJobDisposition
  , error :: Maybe JobError
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
  , disposition: CJ.Record.optional publishJobDispositionCodec
  , error: CJ.Record.optional jobErrorCodec
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
derive newtype instance Eq JobId
derive newtype instance Ord JobId

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

data LogLevel = Debug | Info | Warn | Notice | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

printLogLevel :: LogLevel -> String
printLogLevel = case _ of
  Debug -> "DEBUG"
  Info -> "INFO"
  Warn -> "WARN"
  Notice -> "NOTICE"
  Error -> "ERROR"

-- These numbers are not consecutive so that we can insert new log levels if need be
logLevelToPriority :: LogLevel -> Int
logLevelToPriority = case _ of
  Debug -> 0
  Info -> 10
  Warn -> 20
  Notice -> 25
  Error -> 30

logLevelFromPriority :: Int -> Either String LogLevel
logLevelFromPriority = case _ of
  0 -> Right Debug
  10 -> Right Info
  20 -> Right Warn
  25 -> Right Notice
  30 -> Right Error
  other -> Left $ "Invalid log level priority: " <> show other

parseLogLevel :: String -> Either String LogLevel
parseLogLevel = case _ of
  "DEBUG" -> Right Debug
  "INFO" -> Right Info
  "WARN" -> Right Warn
  "NOTICE" -> Right Notice
  "ERROR" -> Right Error
  other -> Left $ "Invalid log level: " <> other
