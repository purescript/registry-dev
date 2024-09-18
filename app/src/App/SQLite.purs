-- | Bindings for the specific SQL queries we emit to the SQLite database. Use the
-- | Registry.App.Effect.Db module in production code instead of this module;
-- | the bindings here are still quite low-level and simply exist to provide a
-- | nicer interface with PureScript types for higher-level modules to use.

-- TOMORROW:
--
-- * Add the job executor to server startup
-- * Move the various job details to the API.V1 module since it'll be returned by the UI
-- * Update the router to just create a job when received, and on lookup to return relevant details from the db
-- * Update the router to have an endpoint for creating a package set job and compiler matrix job using the
--   same authentication requirements as for GitHub today.
-- * Move the compiler matrix out of publish into its own functionality so it can be called. We want to
--   be able to spawn a matrix job at any time for a compiler/package version pair, but need a helper to
--   do the whole toposort thing.
-- * Update job execution to actually call the relevant publish/unpublish/transfer/package set API fn
--
-- LATER
-- * Update tests that refer to the DB effect
-- * Adjust the integration test(s) to verify we're getting enforced concurrency control
-- * Update the GitHub issue module so it only submits a request to the registry and returns
--   a job id, rather than actually running the fns directly. Poll for a result still and
--   comment when the job completes.
--
-- FOLLOWUP
-- * Punt on the squash commit until later.
module Registry.App.SQLite
  ( SQLite
  , ConnectOptions
  , connect
  , JobInfo
  , selectJobInfo
  , InsertPackageJob
  , insertPackageJob
  , InsertMatrixJob
  , insertMatrixJob
  , InsertPackageSetJob
  , insertPackageSetJob
  , FinishJob
  , finishJob
  , StartJob
  , startJob
  , deleteIncompleteJobs
  , insertLogLine
  , selectLogsByJob
  , PackageJobDetails
  , selectNextPackageJob
  , MatrixJobDetails
  , selectNextMatrixJob
  , PackageSetJobDetails
  , selectNextPackageSetJob
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as JSON.DecodeError
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Data.Nullable as Nullable
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4)
import Effect.Uncurried as Uncurried
import Registry.API.V1 (JobId(..), JobType, LogLevel, LogLine)
import Registry.API.V1 as API.V1
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Operation (PackageOperation, PackageSetOperation)
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Version as Version

-- | An active database connection acquired with `connect`
data SQLite

foreign import connectImpl :: EffectFn2 FilePath (EffectFn1 String Unit) SQLite

type ConnectOptions =
  { database :: FilePath
  , logger :: String -> Effect Unit
  }

-- Connect to the indicated SQLite database
connect :: ConnectOptions -> Effect SQLite
connect { database, logger } = Uncurried.runEffectFn2 connectImpl database (Uncurried.mkEffectFn1 logger)

-- | Metadata about a particular package, package set, or matrix job.
type JobInfo =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  }

type JSJobInfo =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  }

jobInfoFromJSRep :: JSJobInfo -> Either String JobInfo
jobInfoFromJSRep { jobId, createdAt, startedAt, finishedAt, success } = do
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
  isSuccess <- case success of
    0 -> Right false
    1 -> Right true
    _ -> Left $ "Invalid success value " <> show success
  pure
    { jobId: JobId jobId
    , createdAt: created
    , startedAt: started
    , finishedAt: finished
    , success: isSuccess
    }

foreign import selectJobInfoImpl :: EffectFn2 SQLite String (Nullable JSJobInfo)

selectJobInfo :: SQLite -> JobId -> Effect (Either String (Maybe JobInfo))
selectJobInfo db (JobId jobId) = do
  maybeJobInfo <- map toMaybe $ Uncurried.runEffectFn2 selectJobInfoImpl db jobId
  pure $ traverse jobInfoFromJSRep maybeJobInfo

type FinishJob =
  { jobId :: JobId
  , success :: Boolean
  , finishedAt :: DateTime
  }

type JSFinishJob =
  { jobId :: String
  , success :: Int
  , finishedAt :: String
  }

finishJobToJSRep :: FinishJob -> JSFinishJob
finishJobToJSRep { jobId, success, finishedAt } =
  { jobId: un JobId jobId
  , success: if success then 1 else 0
  , finishedAt: DateTime.format Internal.Format.iso8601DateTime finishedAt
  }

foreign import finishJobImpl :: EffectFn2 SQLite JSFinishJob Unit

finishJob :: SQLite -> FinishJob -> Effect Unit
finishJob db = Uncurried.runEffectFn2 finishJobImpl db <<< finishJobToJSRep

type StartJob =
  { jobId :: JobId
  , startedAt :: DateTime
  }

type JSStartJob =
  { jobId :: String
  , startedAt :: String
  }

startJobToJSRep :: StartJob -> JSStartJob
startJobToJSRep { jobId, startedAt } =
  { jobId: un JobId jobId
  , startedAt: DateTime.format Internal.Format.iso8601DateTime startedAt
  }

foreign import startJobImpl :: EffectFn2 SQLite JSStartJob Unit

startJob :: SQLite -> StartJob -> Effect Unit
startJob db = Uncurried.runEffectFn2 startJobImpl db <<< startJobToJSRep

foreign import deleteIncompleteJobsImpl :: EffectFn1 SQLite Unit

deleteIncompleteJobs :: SQLite -> Effect Unit
deleteIncompleteJobs = Uncurried.runEffectFn1 deleteIncompleteJobsImpl

type InsertPackageJob =
  { jobId :: JobId
  , jobType :: JobType
  , packageName :: PackageName
  , packageVersion :: Version
  , payload :: PackageOperation
  }

type JSInsertPackageJob =
  { jobId :: String
  , jobType :: String
  , packageName :: String
  , packageVersion :: String
  , payload :: String
  }

insertPackageJobToJSRep :: InsertPackageJob -> JSInsertPackageJob
insertPackageJobToJSRep { jobId, jobType, packageName, packageVersion, payload } =
  { jobId: un JobId jobId
  , jobType: API.V1.printJobType jobType
  , packageName: PackageName.print packageName
  , packageVersion: Version.print packageVersion
  , payload: stringifyJson Operation.packageOperationCodec payload
  }

foreign import insertPackageJobImpl :: EffectFn2 SQLite JSInsertPackageJob Unit

-- | Insert a new package job, ie. a publish, unpublish, or transfer.
insertPackageJob :: SQLite -> InsertPackageJob -> Effect Unit
insertPackageJob db = Uncurried.runEffectFn2 insertPackageJobImpl db <<< insertPackageJobToJSRep

type InsertMatrixJob =
  { jobId :: JobId
  , packageName :: PackageName
  , packageVersion :: Version
  , compilerVersion :: Version
  , payload :: Map PackageName Version
  }

type JSInsertMatrixJob =
  { jobId :: String
  , packageName :: String
  , packageVersion :: String
  , compilerVersion :: String
  , payload :: String
  }

insertMatrixJobToJSRep :: InsertMatrixJob -> JSInsertMatrixJob
insertMatrixJobToJSRep { jobId, packageName, packageVersion, compilerVersion, payload } =
  { jobId: un JobId jobId
  , packageName: PackageName.print packageName
  , packageVersion: Version.print packageVersion
  , compilerVersion: Version.print compilerVersion
  , payload: stringifyJson (Internal.Codec.packageMap Version.codec) payload
  }

foreign import insertMatrixJobImpl :: EffectFn2 SQLite JSInsertMatrixJob Unit

insertMatrixJob :: SQLite -> InsertMatrixJob -> Effect Unit
insertMatrixJob db = Uncurried.runEffectFn2 insertMatrixJobImpl db <<< insertMatrixJobToJSRep

type InsertPackageSetJob =
  { jobId :: JobId
  , payload :: PackageSetOperation
  }

type JSInsertPackageSetJob =
  { jobId :: String
  , payload :: String
  }

insertPackageSetJobToJSRep :: InsertPackageSetJob -> JSInsertPackageSetJob
insertPackageSetJobToJSRep { jobId, payload } =
  { jobId: un JobId jobId
  , payload: stringifyJson Operation.packageSetOperationCodec payload
  }

foreign import insertPackageSetJobImpl :: EffectFn2 SQLite JSInsertPackageSetJob Unit

insertPackageSetJob :: SQLite -> InsertPackageSetJob -> Effect Unit
insertPackageSetJob db = Uncurried.runEffectFn2 insertPackageSetJobImpl db <<< insertPackageSetJobToJSRep

type PackageJobDetails =
  { jobId :: JobId
  , jobType :: JobType
  , packageName :: PackageName
  , packageVersion :: Version
  , payload :: PackageOperation
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  }

type JSPackageJobDetails =
  { jobId :: String
  , jobType :: String
  , packageName :: String
  , packageVersion :: String
  , payload :: String
  , createdAt :: String
  , startedAt :: Nullable String
  }

packageJobDetailsFromJSRep :: JSPackageJobDetails -> Either String PackageJobDetails
packageJobDetailsFromJSRep { jobId, jobType, packageName, packageVersion, payload, createdAt, startedAt } = do
  ty <- API.V1.parseJobType jobType
  name <- PackageName.parse packageName
  version <- Version.parse packageVersion
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  parsed <- lmap JSON.DecodeError.print $ parseJson Operation.packageOperationCodec payload
  pure
    { jobId: JobId jobId
    , jobType: ty
    , packageName: name
    , packageVersion: version
    , payload: parsed
    , createdAt: created
    , startedAt: started
    }

foreign import selectNextPackageJobImpl :: EffectFn1 SQLite (Nullable JSPackageJobDetails)

selectNextPackageJob :: SQLite -> Effect (Either String (Maybe PackageJobDetails))
selectNextPackageJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn1 selectNextPackageJobImpl db
  pure $ traverse packageJobDetailsFromJSRep maybeJobDetails

type MatrixJobDetails =
  { jobId :: JobId
  , packageName :: PackageName
  , packageVersion :: Version
  , compilerVersion :: Version
  , payload :: Map PackageName Version
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  }

type JSMatrixJobDetails =
  { jobId :: String
  , packageName :: String
  , packageVersion :: String
  , compilerVersion :: String
  , payload :: String
  , createdAt :: String
  , startedAt :: Nullable String
  }

matrixJobDetailsFromJSRep :: JSMatrixJobDetails -> Either String MatrixJobDetails
matrixJobDetailsFromJSRep { jobId, packageName, packageVersion, compilerVersion, payload, createdAt, startedAt } = do
  name <- PackageName.parse packageName
  version <- Version.parse packageVersion
  compiler <- Version.parse compilerVersion
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  parsed <- lmap JSON.DecodeError.print $ parseJson (Internal.Codec.packageMap Version.codec) payload
  pure
    { jobId: JobId jobId
    , packageName: name
    , packageVersion: version
    , compilerVersion: compiler
    , payload: parsed
    , createdAt: created
    , startedAt: started
    }

foreign import selectNextMatrixJobImpl :: EffectFn1 SQLite (Nullable JSMatrixJobDetails)

selectNextMatrixJob :: SQLite -> Effect (Either String (Maybe MatrixJobDetails))
selectNextMatrixJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn1 selectNextMatrixJobImpl db
  pure $ traverse matrixJobDetailsFromJSRep maybeJobDetails

type PackageSetJobDetails =
  { jobId :: JobId
  , payload :: PackageSetOperation
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  }

type JSPackageSetJobDetails =
  { jobId :: String
  , payload :: String
  , createdAt :: String
  , startedAt :: Nullable String
  }

packageSetJobDetailsFromJSRep :: JSPackageSetJobDetails -> Either String PackageSetJobDetails
packageSetJobDetailsFromJSRep { jobId, payload, createdAt, startedAt } = do
  parsed <- lmap JSON.DecodeError.print $ parseJson Operation.packageSetOperationCodec payload
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  pure
    { jobId: JobId jobId
    , payload: parsed
    , createdAt: created
    , startedAt: started
    }

foreign import selectNextPackageSetJobImpl :: EffectFn1 SQLite (Nullable JSPackageSetJobDetails)

selectNextPackageSetJob :: SQLite -> Effect (Either String (Maybe PackageSetJobDetails))
selectNextPackageSetJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn1 selectNextPackageSetJobImpl db
  pure $ traverse packageSetJobDetailsFromJSRep maybeJobDetails

type JSLogLine =
  { level :: Int
  , message :: String
  , jobId :: String
  , timestamp :: String
  }

logLineToJSRep :: LogLine -> JSLogLine
logLineToJSRep { level, message, jobId, timestamp } =
  { level: API.V1.logLevelToPriority level
  , message
  , jobId: un JobId jobId
  , timestamp: DateTime.format Internal.Format.iso8601DateTime timestamp
  }

logLineFromJSRep :: JSLogLine -> Either String LogLine
logLineFromJSRep { level, message, jobId, timestamp } = do
  logLevel <- API.V1.logLevelFromPriority level
  time <- DateTime.unformat Internal.Format.iso8601DateTime timestamp
  pure
    { level: logLevel
    , message
    , jobId: JobId jobId
    , timestamp: time
    }

foreign import insertLogLineImpl :: EffectFn2 SQLite JSLogLine Unit

insertLogLine :: SQLite -> LogLine -> Effect Unit
insertLogLine db = Uncurried.runEffectFn2 insertLogLineImpl db <<< logLineToJSRep

foreign import selectLogsByJobImpl :: EffectFn4 SQLite String Int (Nullable String) (Array JSLogLine)

-- | Select all logs for a given job at or above the indicated log level. To get all
-- | logs, pass the DEBUG log level.
selectLogsByJob :: SQLite -> JobId -> LogLevel -> Maybe DateTime -> Effect { fail :: Array String, success :: Array LogLine }
selectLogsByJob db jobId level since = do
  let timestamp = map (DateTime.format Internal.Format.iso8601DateTime) since
  jsLogLines <-
    Uncurried.runEffectFn4
      selectLogsByJobImpl
      db
      (un JobId jobId)
      (API.V1.logLevelToPriority level)
      (Nullable.toNullable timestamp)
  pure $ partitionEithers $ map logLineFromJSRep jsLogLines
