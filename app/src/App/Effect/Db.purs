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
import Effect.Console as Console
import Registry.API.V1 (JobId(..), JobType, LogLevel, LogLine)
import Registry.API.V1 as API.V1
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName

foreign import data Db :: Type

foreign import connect :: FilePath -> Effect Db

foreign import insertLogImpl :: Db -> LogLineJS -> Effect Unit

foreign import selectLogsByJobImpl :: Db -> String -> Int -> Effect (Array LogLineJS)

foreign import createJobImpl :: Db -> NewJobJS -> Effect Unit

foreign import finishJobImpl :: Db -> JobResultJS -> Effect Unit

foreign import selectJobImpl :: Db -> String -> Effect (Nullable JobJS)

foreign import runningJobForPackageImpl :: Db -> String -> Effect (Nullable JobJS)

foreign import deleteIncompleteJobsImpl :: Db -> Effect Unit

type LogLineJS =
  { level :: Int
  , message :: String
  , timestamp :: String
  , jobId :: String
  }

fromLogLineJS :: LogLineJS -> Either String LogLine
fromLogLineJS { level: rawLevel, message, timestamp: rawTimestamp, jobId } = case API.V1.logLevelFromPriority rawLevel, DateTime.unformat Internal.Format.iso8601DateTime rawTimestamp of
  Left err, _ -> Left err
  _, Left err -> Left $ "Invalid timestamp " <> show rawTimestamp <> ": " <> err
  Right level, Right timestamp -> Right { level, message, jobId: JobId jobId, timestamp }

toLogLineJS :: LogLine -> LogLineJS
toLogLineJS { level, message, timestamp, jobId: JobId jobId } =
  { level: API.V1.logLevelToPriority level
  , message
  , timestamp: DateTime.format Internal.Format.iso8601DateTime timestamp
  , jobId
  }

selectLogsByJob :: Db -> JobId -> LogLevel -> Maybe DateTime -> Effect (Array LogLine)
selectLogsByJob db (JobId jobId) level maybeDatetime = do
  logs <- selectLogsByJobImpl db jobId (API.V1.logLevelToPriority level)

  let { success, fail } = partitionEithers $ map fromLogLineJS logs
  -- TODO: port this in Run so we can use the logger. But the logger needs the Db...
  when (Array.length fail > 0) do
    Console.log $ "Failed to parse " <> show (Array.length fail) <> " log lines"

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

type NewJobJS =
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

type JobResultJS =
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

type JobJS =
  { jobId :: String
  , jobType :: String
  , packageName :: String
  , ref :: String
  , createdAt :: String
  , finishedAt :: Nullable String
  , success :: Int
  }

toNewJobJS :: NewJob -> NewJobJS
toNewJobJS { jobId: JobId jobId, jobType, createdAt, packageName, ref } =
  { jobId
  , jobType: API.V1.printJobType jobType
  , createdAt: DateTime.format Internal.Format.iso8601DateTime createdAt
  , packageName: PackageName.print packageName
  , ref
  }

toJobResultJS :: JobResult -> JobResultJS
toJobResultJS { jobId: JobId jobId, finishedAt, success } =
  { jobId
  , finishedAt: DateTime.format Internal.Format.iso8601DateTime finishedAt
  , success: if success then 1 else 0
  }

fromJobJS :: JobJS -> Either String Job
fromJobJS raw = do
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

createJob :: Db -> NewJob -> Effect Unit
createJob db newJob = createJobImpl db $ toNewJobJS newJob

finishJob :: Db -> JobResult -> Effect Unit
finishJob db result = finishJobImpl db $ toJobResultJS result

selectJob :: Db -> JobId -> Effect (Either String Job)
selectJob db (JobId jobId) = do
  maybeJob <- toMaybe <$> selectJobImpl db jobId
  pure $ (note ("Couldn't find job with id " <> jobId) maybeJob) >>= fromJobJS

runningJobForPackage :: Db -> PackageName -> Effect (Either String Job)
runningJobForPackage db packageName = do
  let pkgStr = PackageName.print packageName
  maybeJobJS <- toMaybe <$> runningJobForPackageImpl db pkgStr
  pure $ (note ("Couldn't find running job for package " <> pkgStr) maybeJobJS) >>= fromJobJS

deleteIncompleteJobs :: Db -> Effect Unit
deleteIncompleteJobs db = deleteIncompleteJobsImpl db
