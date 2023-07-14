module Registry.App.Effect.Db
  ( Db
  , connect
  , createJob
  , finishJob
  , selectJob
  , insertLog
  , selectLogsByJob
  , runningJobForPackage
  , deleteIncompleteJobs
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Effect.Class.Console (log)
import Registry.API.V1 (JobId(..), JobType, LogLevel, LogLine, logLevelFromPriority, logLevelToPriority, parseJobType, printJobType)
import Registry.App.Prelude as Nullable
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName

foreign import data Db :: Type

foreign import connect :: FilePath -> Effect Db

foreign import insertLogImpl :: Db -> LogLineJS -> Effect Unit

foreign import selectLogsByJobImpl :: Db -> String -> Int -> Effect (Array LogLineJS)

foreign import createJobImpl :: Db -> NewJobJs -> Effect Unit

foreign import finishJobImpl :: Db -> JobResultJs -> Effect Unit

foreign import selectJobImpl :: Db -> String -> Effect (Nullable JobJs)

foreign import runningJobForPackageImpl :: Db -> String -> Effect (Nullable JobJs)

foreign import deleteIncompleteJobsImpl :: Db -> Effect Unit

type LogLineJS =
  { level :: Int
  , message :: String
  , timestamp :: String
  , jobId :: String
  }

fromLogLineJS :: LogLineJS -> Either String LogLine
fromLogLineJS { level: rawLevel, message, timestamp: rawTimestamp, jobId } = case logLevelFromPriority rawLevel, DateTime.unformat Internal.Format.iso8601DateTime rawTimestamp of
  Left err, _ -> Left err
  _, Left err -> Left $ "Invalid timestamp " <> show rawTimestamp <> ": " <> err
  Right level, Right timestamp -> Right { level, message, jobId: JobId jobId, timestamp }

toLogLineJS :: LogLine -> LogLineJS
toLogLineJS { level, message, timestamp, jobId: JobId jobId } =
  { level: logLevelToPriority level
  , message
  , timestamp: DateTime.format Internal.Format.iso8601DateTime timestamp
  , jobId
  }

selectLogsByJob :: Db -> JobId -> LogLevel -> Maybe DateTime -> Effect (Array LogLine)
selectLogsByJob db (JobId jobId) level maybeDatetime = do
  logs <- selectLogsByJobImpl db jobId (logLevelToPriority level)

  let { success, fail } = partitionEithers $ map fromLogLineJS logs
  -- TODO: port this in Run so we can use the logger. But the logger needs the Db...
  when (Array.length fail > 0) do
    log $ "Failed to parse " <> show (Array.length fail) <> " log lines"

  pure $ Array.filter (\{ timestamp } -> timestamp > (fromMaybe bottom maybeDatetime)) success

insertLog :: Db -> LogLine -> Effect Unit
insertLog db logLine = insertLogImpl db $ toLogLineJS logLine

type NewJob =
  { jobId :: JobId
  , jobType :: JobType
  , createdAt :: DateTime
  , packageName :: PackageName
  , ref :: String
  }

type NewJobJs =
  { jobId :: String
  , jobType :: String
  , createdAt :: String
  , packageName :: String
  , ref :: String
  }

type JobResult =
  { jobId :: JobId
  , finishedAt :: DateTime
  , success :: Boolean
  }

type JobResultJs =
  { jobId :: String
  , finishedAt :: String
  , success :: Int
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

type JobJs =
  { jobId :: String
  , jobType :: String
  , packageName :: String
  , ref :: String
  , createdAt :: String
  , finishedAt :: Nullable String
  , success :: Int
  }

toNewJobJs :: NewJob -> NewJobJs
toNewJobJs { jobId: JobId jobId, jobType, createdAt, packageName, ref } =
  { jobId
  , jobType: printJobType jobType
  , createdAt: DateTime.format Internal.Format.iso8601DateTime createdAt
  , packageName: PackageName.print packageName
  , ref
  }

toJobResultJs :: JobResult -> JobResultJs
toJobResultJs { jobId: JobId jobId, finishedAt, success } =
  { jobId
  , finishedAt: DateTime.format Internal.Format.iso8601DateTime finishedAt
  , success: if success then 1 else 0
  }

fromJobJs :: JobJs -> Either String Job
fromJobJs raw = do
  let jobId = JobId raw.jobId
  jobType <- parseJobType raw.jobType
  packageName <- PackageName.parse raw.packageName
  createdAt <- DateTime.unformat Internal.Format.iso8601DateTime raw.createdAt
  finishedAt <- case Nullable.toMaybe raw.finishedAt of
    Nothing -> pure Nothing
    Just rawFinishedAt -> Just <$> DateTime.unformat Internal.Format.iso8601DateTime rawFinishedAt
  success <- case raw.success of
    0 -> Right false
    1 -> Right true
    _ -> Left $ "Invalid success value " <> show raw.success
  pure $ { jobId, jobType, createdAt, finishedAt, success, packageName, ref: raw.ref }

createJob :: Db -> NewJob -> Effect Unit
createJob db newJob = createJobImpl db $ toNewJobJs newJob

finishJob :: Db -> JobResult -> Effect Unit
finishJob db result = finishJobImpl db $ toJobResultJs result

selectJob :: Db -> JobId -> Effect (Either String Job)
selectJob db (JobId jobId) = do
  maybeJob <- Nullable.toMaybe <$> selectJobImpl db jobId
  pure $ (note ("Couldn't find job with id " <> jobId) maybeJob) >>= fromJobJs

runningJobForPackage :: Db -> PackageName -> Effect (Either String Job)
runningJobForPackage db packageName = do
  let pkgStr = PackageName.print packageName
  maybeJobJs <- Nullable.toMaybe <$> runningJobForPackageImpl db pkgStr
  pure $ (note ("Couldn't find running job for package " <> pkgStr) maybeJobJs) >>= fromJobJs

deleteIncompleteJobs :: Db -> Effect Unit
deleteIncompleteJobs db = deleteIncompleteJobsImpl db
