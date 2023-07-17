module Registry.App.Effect.Db where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.String as String
import Registry.API.V1 (JobId, LogLevel, LogLine)
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.SQLite (JobResult, NewJob, SQLite)
import Registry.App.SQLite as SQLite
import Run (EFFECT, Run)
import Run as Run

-- We could separate these by database if it grows too large. Also, for now these
-- simply lift their Effect-based equivalents in the SQLite module, but ideally
-- that module would expose lower-level building blocks for accessing the database
-- and we'd implement these in terms of those in this module.
--
-- Also, this does not currently include setup and teardown (those are handled
-- outside the effect), but we may wish to add those in the future if they'll
-- be part of app code we want to test.
data Db a
  = InsertLog LogLine a
  | SelectLogsByJob JobId LogLevel (Maybe DateTime) (Array LogLine -> a)
  | CreateJob NewJob a
  | FinishJob JobResult a
  | SelectJob JobId (Either String SQLite.Job -> a)
  | RunningJobForPackage PackageName (Either String SQLite.Job -> a)

derive instance Functor Db

-- | An effect for accessing the database.
type DB r = (db :: Db | r)

_db :: Proxy "db"
_db = Proxy

-- | Insert a new log line into the database.
insertLog :: forall r. LogLine -> Run (DB + r) Unit
insertLog log = Run.lift _db (InsertLog log unit)

-- | Select all logs for a given job, filtered by loglevel and a time cutoff.
selectLogsByJob :: forall r. JobId -> LogLevel -> Maybe DateTime -> Run (DB + r) (Array LogLine)
selectLogsByJob jobId logLevel since = Run.lift _db (SelectLogsByJob jobId logLevel since identity)

-- | Create a new job in the database.
createJob :: forall r. NewJob -> Run (DB + r) Unit
createJob newJob = Run.lift _db (CreateJob newJob unit)

-- | Set a job in the database to the 'finished' state.
finishJob :: forall r. JobResult -> Run (DB + r) Unit
finishJob jobResult = Run.lift _db (FinishJob jobResult unit)

-- | Select a job by ID from the database.
selectJob :: forall r. JobId -> Run (DB + r) (Either String SQLite.Job)
selectJob jobId = Run.lift _db (SelectJob jobId identity)

-- | Select a job by package name from the database, failing if there is no
-- | current job available for that package name.
runningJobForPackage :: forall r. PackageName -> Run (DB + r) (Either String SQLite.Job)
runningJobForPackage name = Run.lift _db (RunningJobForPackage name identity)

interpret :: forall r a. (Db ~> Run r) -> Run (DB + r) a -> Run r a
interpret handler = Run.interpret (Run.on _db handler Run.send)

type SQLiteEnv = { db :: SQLite }

-- | Interpret DB by interacting with the SQLite database on disk.
handleSQLite :: forall r a. SQLiteEnv -> Db a -> Run (LOG + EFFECT + r) a
handleSQLite env = case _ of
  InsertLog log next -> do
    Run.liftEffect $ SQLite.insertLog env.db log
    pure next

  SelectLogsByJob jobId logLevel since reply -> do
    logs <- Run.liftEffect $ SQLite.selectLogsByJob env.db jobId logLevel since
    unless (Array.null logs.fail) do
      Log.warn $ "Some logs are not readable: " <> String.joinWith "\n" logs.fail
    pure $ reply logs.success

  CreateJob newJob next -> do
    Run.liftEffect $ SQLite.createJob env.db newJob
    pure next

  FinishJob jobResult next -> do
    Run.liftEffect $ SQLite.finishJob env.db jobResult
    pure next

  SelectJob jobId reply -> do
    job <- Run.liftEffect $ SQLite.selectJob env.db jobId
    pure $ reply job

  RunningJobForPackage name reply -> do
    job <- Run.liftEffect $ SQLite.runningJobForPackage env.db name
    pure $ reply job
