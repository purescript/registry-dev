module Registry.API.V1
  ( AdminJobData
  , AdminJobType(..)
  , JobCreatedResponse
  , JobId(..)
  , JobInfo
  , JobType(..)
  , Job(..)
  , LegacyImportMode(..)
  , LogLevel(..)
  , LogLine
  , MatrixJobData
  , PackageSetUpdateMode(..)
  , PublishJobData
  , Route(..)
  , TransferJobData
  , UnpublishJobData
  , adminJobTypeCodec
  , adminJobTypeKey
  , jobInfo
  , jobCodec
  , jobCreatedResponseCodec
  , logLevelFromPriority
  , logLevelToPriority
  , printJobType
  , printLegacyImportMode
  , printLogLevel
  , printPackageSetUpdateMode
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
import Data.Maybe (Maybe(..))
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
  | Jobs { since :: Maybe DateTime, include_completed :: Maybe Boolean }
  | Job JobId { level :: Maybe LogLevel, since :: Maybe DateTime }
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
      , include_completed: Routing.optional <<< Routing.boolean
      }
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
  | AdminJob AdminJobData

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

-- | Admin job types for scheduled operations and manual package set updates
data AdminJobType
  = AdminPackageTransfer
  | AdminLegacyImport LegacyImportMode
  | AdminPackageSetUpdate PackageSetUpdateMode
  | AdminPackageSetOperation PackageSetOperation -- For manual API requests

derive instance Eq AdminJobType

data LegacyImportMode = DryRun | GenerateRegistry | UpdateRegistry

derive instance Eq LegacyImportMode

data PackageSetUpdateMode = GeneratePackageSet | CommitPackageSet

derive instance Eq PackageSetUpdateMode

-- | Returns the key used in the database for an admin job type
adminJobTypeKey :: AdminJobType -> String
adminJobTypeKey = case _ of
  AdminPackageTransfer -> "package_transfer"
  AdminLegacyImport _ -> "legacy_import"
  AdminPackageSetUpdate _ -> "package_set_update"
  AdminPackageSetOperation _ -> "package_set_operation"

type AdminJobData = JobInfo
  ( adminJobType :: AdminJobType
  , jobType :: Proxy "admin"
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
      <|> map AdminJob (Codec.decode adminJobDataCodec json)

  encode :: Job -> JSON
  encode = case _ of
    PublishJob j -> CJ.encode publishJobDataCodec j
    UnpublishJob j -> CJ.encode unpublishJobDataCodec j
    TransferJob j -> CJ.encode transferJobDataCodec j
    MatrixJob j -> CJ.encode matrixJobDataCodec j
    AdminJob j -> CJ.encode adminJobDataCodec j

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

adminJobDataCodec :: CJ.Codec AdminJobData
adminJobDataCodec = CJ.named "AdminJob" $ CJ.Record.object
  { jobId: jobIdCodec
  , jobType: symbolCodec (Proxy :: _ "admin")
  , createdAt: Internal.Codec.iso8601DateTime
  , startedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , finishedAt: CJ.Record.optional Internal.Codec.iso8601DateTime
  , success: CJ.boolean
  , logs: CJ.array logLineCodec
  , adminJobType: adminJobTypeCodec
  }

adminJobTypeCodec :: CJ.Codec AdminJobType
adminJobTypeCodec = Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError AdminJobType
  decode json = do
    obj <- Codec.decode (CJ.Record.object { type: CJ.string }) json
    case obj.type of
      "package_transfer" -> pure AdminPackageTransfer
      "legacy_import" ->
        map (\{ mode } -> AdminLegacyImport mode)
          (Codec.decode (CJ.Record.object { mode: legacyImportModeCodec }) json)
      "package_set_update" ->
        map (\{ mode } -> AdminPackageSetUpdate mode)
          (Codec.decode (CJ.Record.object { mode: packageSetUpdateModeCodec }) json)
      "package_set_operation" ->
        map (\{ payload } -> AdminPackageSetOperation payload)
          (Codec.decode (CJ.Record.object { payload: Operation.packageSetOperationCodec }) json)
      other -> except $ Left $ CJ.DecodeError.basic $ "Unknown admin job type: " <> other

  encode :: AdminJobType -> JSON
  encode = case _ of
    AdminPackageTransfer ->
      CJ.encode (CJ.Record.object { type: CJ.string }) { type: "package_transfer" }
    AdminLegacyImport mode ->
      CJ.encode (CJ.Record.object { type: CJ.string, mode: legacyImportModeCodec })
        { type: "legacy_import", mode }
    AdminPackageSetUpdate mode ->
      CJ.encode (CJ.Record.object { type: CJ.string, mode: packageSetUpdateModeCodec })
        { type: "package_set_update", mode }
    AdminPackageSetOperation payload ->
      CJ.encode (CJ.Record.object { type: CJ.string, payload: Operation.packageSetOperationCodec })
        { type: "package_set_operation", payload }

legacyImportModeCodec :: CJ.Codec LegacyImportMode
legacyImportModeCodec = CJ.Sum.enumSum printLegacyImportMode parseLegacyImportMode
  where
  parseLegacyImportMode = case _ of
    "dry_run" -> Just DryRun
    "generate_registry" -> Just GenerateRegistry
    "update_registry" -> Just UpdateRegistry
    _ -> Nothing

printLegacyImportMode :: LegacyImportMode -> String
printLegacyImportMode = case _ of
  DryRun -> "dry_run"
  GenerateRegistry -> "generate_registry"
  UpdateRegistry -> "update_registry"

packageSetUpdateModeCodec :: CJ.Codec PackageSetUpdateMode
packageSetUpdateModeCodec = CJ.Sum.enumSum printPackageSetUpdateMode parsePackageSetUpdateMode
  where
  parsePackageSetUpdateMode = case _ of
    "generate" -> Just GeneratePackageSet
    "commit" -> Just CommitPackageSet
    _ -> Nothing

printPackageSetUpdateMode :: PackageSetUpdateMode -> String
printPackageSetUpdateMode = case _ of
  GeneratePackageSet -> "generate"
  CommitPackageSet -> "commit"

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
  AdminJob { jobId, createdAt, startedAt, finishedAt, success, logs } ->
    { jobId, createdAt, startedAt, finishedAt, success, logs }

newtype JobId = JobId String

derive instance Newtype JobId _
derive newtype instance Eq JobId

jobIdCodec :: CJ.Codec JobId
jobIdCodec = Profunctor.wrapIso JobId CJ.string

data JobType
  = PublishJobType
  | UnpublishJobType
  | TransferJobType
  | MatrixJobType
  | AdminJobType

derive instance Eq JobType

printJobType :: JobType -> String
printJobType = case _ of
  PublishJobType -> "publish"
  UnpublishJobType -> "unpublish"
  TransferJobType -> "transfer"
  MatrixJobType -> "matrix"
  AdminJobType -> "admin"

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
