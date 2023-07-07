module Registry.App.Effect.Db
  ( selectLogsByJob
  , insertLog
  , connect
  , Db
  , JobId(..)
  , logLineCodec
  , jobIdCodec
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
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format

foreign import data Db :: Type

foreign import connect :: Effect Db

foreign import insertLogImpl :: Db -> LogLineJS -> Effect Unit

foreign import selectLogsByJobImpl :: Db -> String -> Int -> Effect (Array LogLineJS)

type LogLineJS =
  { level :: String
  , message :: String
  , timestamp :: String
  , jobId :: String
  }

fromLogLineJS :: LogLineJS -> Either String LogLine
fromLogLineJS { level: rawLevel, message, timestamp: rawTimestamp, jobId } = case parseLogLevel rawLevel, DateTime.unformat Internal.Format.iso8601DateTime rawTimestamp of
  Left err, _ -> Left err
  _, Left err -> Left $ "Invalid timestamp " <> show rawTimestamp <> ": " <> err
  Right level, Right timestamp -> Right { level, message, jobId: JobId jobId, timestamp }

toLogLineJS :: LogLine -> LogLineJS
toLogLineJS { level, message, timestamp, jobId: JobId jobId } =
  { level: printLogLevel level
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
