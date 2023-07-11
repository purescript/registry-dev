module Registry.App.Effect.Db
  ( Db
  , JobId(..)
  , JobType(..)
  , LogLine
  , connect
  , createJob
  , finishJob
  , selectJob
  , insertLog
  , jobIdCodec
  , jobTypeCodec
  , logLineCodec
  , selectLogsByJob
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Sum as CA.Sum
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Data.Profunctor as Profunctor
import Effect.Class.Console (log)
import Registry.App.Prelude as Nullable
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format

foreign import data Db :: Type

foreign import connect :: FilePath -> Effect Db

foreign import insertLogImpl :: Db -> LogLineJS -> Effect Unit

foreign import selectLogsByJobImpl :: Db -> String -> Int -> Effect (Array LogLineJS)

foreign import createJobImpl :: Db -> NewJobJs -> Effect Unit

foreign import finishJobImpl :: Db -> JobResultJs -> Effect Unit

foreign import selectJobImpl :: Db -> String -> Effect (Nullable JobJs)

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

newtype JobId = JobId String

derive instance Newtype JobId _

jobIdCodec :: JsonCodec JobId
jobIdCodec = Profunctor.wrapIso JobId CA.string

selectLogsByJob :: Db -> JobId -> LogLevel -> Maybe DateTime -> Effect (Array LogLine)
selectLogsByJob db (JobId jobId) level maybeDatetime = do
  logs <- selectLogsByJobImpl db jobId (logLevelToPriority level)

  let { success, fail } = partitionEithers $ map fromLogLineJS logs
  -- TODO: port this in Run so we can use the logger. But the logger needs the Db...
  when (Array.length fail > 0) do
    log $ "Failed to parse " <> show (Array.length fail) <> " log lines"

  pure $ Array.filter (\{ timestamp } -> timestamp >= (fromMaybe bottom maybeDatetime)) success

insertLog :: Db -> LogLine -> Effect Unit
insertLog db logLine = insertLogImpl db $ toLogLineJS logLine

type NewJob =
  { jobId :: JobId
  , jobType :: JobType
  , createdAt :: DateTime
  }

type NewJobJs =
  { jobId :: String
  , jobType :: String
  , createdAt :: String
  }

data JobType = Publish | Unpublish | Transfer

parseJobType :: String -> Either String JobType
parseJobType = case _ of
  "publish" -> Right Publish
  "unpublish" -> Right Unpublish
  "transfer" -> Right Transfer
  j -> Left $ "Invalid job type " <> show j

printJobType :: JobType -> String
printJobType = case _ of
  Publish -> "publish"
  Unpublish -> "unpublish"
  Transfer -> "transfer"

jobTypeCodec :: JsonCodec JobType
jobTypeCodec = CA.Sum.enumSum printJobType (hush <<< parseJobType)

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
  , createdAt :: DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  }

type JobJs =
  { jobId :: String
  , jobType :: String
  , createdAt :: String
  , finishedAt :: Nullable String
  , success :: Int
  }

toNewJobJs :: NewJob -> NewJobJs
toNewJobJs { jobId: JobId jobId, jobType, createdAt } =
  { jobId
  , jobType: printJobType jobType
  , createdAt: DateTime.format Internal.Format.iso8601DateTime createdAt
  }

toJobResultJs :: JobResult -> JobResultJs
toJobResultJs { jobId: JobId jobId, finishedAt, success } =
  { jobId
  , finishedAt: DateTime.format Internal.Format.iso8601DateTime finishedAt
  , success: if success then 1 else 0
  }

fromJobJs :: JobJs -> Either String Job
fromJobJs { jobId: rawJobId, jobType: rawJobType, createdAt: rawCreatedAt, finishedAt: maybeFinishedAt, success: rawSuccess } = do
  let jobId = JobId rawJobId
  jobType <- parseJobType rawJobType
  createdAt <- DateTime.unformat Internal.Format.iso8601DateTime rawCreatedAt
  finishedAt <- case Nullable.toMaybe maybeFinishedAt of
    Nothing -> pure Nothing
    Just rawFinishedAt -> Just <$> DateTime.unformat Internal.Format.iso8601DateTime rawFinishedAt
  success <- case rawSuccess of
    0 -> Right false
    1 -> Right false
    _ -> Left $ "Invalid success value " <> show rawSuccess
  pure $ { jobId, jobType, createdAt, finishedAt, success }

createJob :: Db -> NewJob -> Effect Unit
createJob db newJob = createJobImpl db $ toNewJobJs newJob

finishJob :: Db -> JobResult -> Effect Unit
finishJob db result = finishJobImpl db $ toJobResultJs result

selectJob :: Db -> JobId -> Effect (Either String Job)
selectJob db (JobId jobId) = do
  maybeJob <- Nullable.toMaybe <$> selectJobImpl db jobId
  pure $ (note ("Couldn't find job with id " <> jobId) maybeJob) >>= fromJobJs
