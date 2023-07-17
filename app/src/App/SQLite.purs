module Registry.App.SQLite
  ( Job
  , JobLogs
  , JobResult
  , NewJob
  , SQLite
  , connect
  , createJob
  , deleteIncompleteJobs
  , finishJob
  , insertLog
  , runningJobForPackage
  , selectJob
  , selectLogsByJob
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Uncurried as Uncurried
import Registry.API.V1 (JobId(..), JobType, LogLevel, LogLine)
import Registry.API.V1 as API.V1
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName

data SQLite

foreign import connectImpl :: EffectFn2 FilePath (EffectFn1 String Unit) SQLite

foreign import insertLogImpl :: EffectFn2 SQLite JSLogLine Unit

foreign import selectLogsByJobImpl :: EffectFn3 SQLite String Int (Array JSLogLine)

foreign import createJobImpl :: EffectFn2 SQLite JSNewJob Unit

foreign import finishJobImpl :: EffectFn2 SQLite JSJobResult Unit

foreign import selectJobImpl :: EffectFn2 SQLite String (Nullable JSJob)

foreign import runningJobForPackageImpl :: EffectFn2 SQLite String (Nullable JSJob)

foreign import deleteIncompleteJobsImpl :: EffectFn1 SQLite Unit

type ConnectOptions =
  { database :: FilePath
  , logger :: String -> Effect Unit
  }

connect :: ConnectOptions -> Effect SQLite
connect { database, logger } = Uncurried.runEffectFn2 connectImpl database (Uncurried.mkEffectFn1 logger)

type JSLogLine =
  { level :: Int
  , message :: String
  , timestamp :: String
  , jobId :: String
  }

jsLogLineToLogLine :: JSLogLine -> Either String LogLine
jsLogLineToLogLine { level: rawLevel, message, timestamp: rawTimestamp, jobId } = case API.V1.logLevelFromPriority rawLevel, DateTime.unformat Internal.Format.iso8601DateTime rawTimestamp of
  Left err, _ -> Left err
  _, Left err -> Left $ "Invalid timestamp " <> show rawTimestamp <> ": " <> err
  Right level, Right timestamp -> Right { level, message, jobId: JobId jobId, timestamp }

logLineToJSLogLine :: LogLine -> JSLogLine
logLineToJSLogLine { level, message, timestamp, jobId: JobId jobId } =
  { level: API.V1.logLevelToPriority level
  , message
  , timestamp: DateTime.format Internal.Format.iso8601DateTime timestamp
  , jobId
  }

insertLog :: SQLite -> LogLine -> Effect Unit
insertLog db = Uncurried.runEffectFn2 insertLogImpl db <<< logLineToJSLogLine

type JobLogs = { fail :: Array String, success :: Array LogLine }

selectLogsByJob :: SQLite -> JobId -> LogLevel -> Maybe DateTime -> Effect JobLogs
selectLogsByJob db (JobId jobId) level maybeDatetime = do
  logs <- Uncurried.runEffectFn3 selectLogsByJobImpl db jobId (API.V1.logLevelToPriority level)
  let { success, fail } = partitionEithers $ map jsLogLineToLogLine logs
  pure { fail, success: Array.filter (\{ timestamp } -> timestamp > (fromMaybe bottom maybeDatetime)) success }

type NewJob =
  { jobId :: JobId
  , jobType :: JobType
  , createdAt :: DateTime
  , packageName :: PackageName
  , ref :: String
  }

type JSNewJob =
  { jobId :: String
  , jobType :: String
  , createdAt :: String
  , packageName :: String
  , ref :: String
  }

newJobToJSNewJob :: NewJob -> JSNewJob
newJobToJSNewJob { jobId: JobId jobId, jobType, createdAt, packageName, ref } =
  { jobId
  , jobType: API.V1.printJobType jobType
  , createdAt: DateTime.format Internal.Format.iso8601DateTime createdAt
  , packageName: PackageName.print packageName
  , ref
  }

type JobResult =
  { jobId :: JobId
  , finishedAt :: DateTime
  , success :: Boolean
  }

type JSJobResult =
  { jobId :: String
  , finishedAt :: String
  , success :: Int
  }

jobResultToJSJobResult :: JobResult -> JSJobResult
jobResultToJSJobResult { jobId: JobId jobId, finishedAt, success } =
  { jobId
  , finishedAt: DateTime.format Internal.Format.iso8601DateTime finishedAt
  , success: if success then 1 else 0
  }

type Job =
  { jobId :: JobId
  , jobType :: JobType
  , packageName :: PackageName
  , ref :: String
  , createdAt :: DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  }

type JSJob =
  { jobId :: String
  , jobType :: String
  , packageName :: String
  , ref :: String
  , createdAt :: String
  , finishedAt :: Nullable String
  , success :: Int
  }

jsJobToJob :: JSJob -> Either String Job
jsJobToJob raw = do
  let jobId = JobId raw.jobId
  jobType <- API.V1.parseJobType raw.jobType
  packageName <- PackageName.parse raw.packageName
  createdAt <- DateTime.unformat Internal.Format.iso8601DateTime raw.createdAt
  finishedAt <- case toMaybe raw.finishedAt of
    Nothing -> pure Nothing
    Just rawFinishedAt -> Just <$> DateTime.unformat Internal.Format.iso8601DateTime rawFinishedAt
  success <- case raw.success of
    0 -> Right false
    1 -> Right true
    _ -> Left $ "Invalid success value " <> show raw.success
  pure $ { jobId, jobType, createdAt, finishedAt, success, packageName, ref: raw.ref }

createJob :: SQLite -> NewJob -> Effect Unit
createJob db = Uncurried.runEffectFn2 createJobImpl db <<< newJobToJSNewJob

finishJob :: SQLite -> JobResult -> Effect Unit
finishJob db = Uncurried.runEffectFn2 finishJobImpl db <<< jobResultToJSJobResult

selectJob :: SQLite -> JobId -> Effect (Either String Job)
selectJob db (JobId jobId) = do
  maybeJob <- toMaybe <$> Uncurried.runEffectFn2 selectJobImpl db jobId
  pure $ jsJobToJob =<< note ("Couldn't find job with id " <> jobId) maybeJob

runningJobForPackage :: SQLite -> PackageName -> Effect (Either String Job)
runningJobForPackage db packageName = do
  let pkgStr = PackageName.print packageName
  maybeJSJob <- toMaybe <$> Uncurried.runEffectFn2 runningJobForPackageImpl db pkgStr
  pure $ jsJobToJob =<< note ("Couldn't find running job for package " <> pkgStr) maybeJSJob

deleteIncompleteJobs :: SQLite -> Effect Unit
deleteIncompleteJobs = Uncurried.runEffectFn1 deleteIncompleteJobsImpl
