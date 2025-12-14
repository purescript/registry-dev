module Registry.App.Effect.Db where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.String as String
import Registry.API.V1 (JobId, LogLevel, LogLine)
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.SQLite (FinishJob, InsertMatrixJob, InsertPackageJob, InsertPackageSetJob, JobInfo, MatrixJobDetails, PackageJobDetails, PackageSetJobDetails, SQLite, StartJob)
import Registry.App.SQLite as SQLite
import Run (EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- We could separate these by database if it grows too large. Also, for now these
-- simply lift their Effect-based equivalents in the SQLite module, but ideally
-- that module would expose lower-level building blocks for accessing the database
-- and we'd implement these in terms of those in this module.
--
-- Also, this does not currently include setup and teardown (those are handled
-- outside the effect), but we may wish to add those in the future if they'll
-- be part of app code we want to test.

data Db a
  = InsertPackageJob InsertPackageJob a
  | InsertMatrixJob InsertMatrixJob a
  | InsertPackageSetJob InsertPackageSetJob a
  | FinishJob FinishJob a
  | StartJob StartJob a
  | SelectJobInfo JobId (Either String (Maybe JobInfo) -> a)
  | SelectNextPackageJob (Either String (Maybe PackageJobDetails) -> a)
  | SelectNextMatrixJob (Either String (Maybe MatrixJobDetails) -> a)
  | SelectNextPackageSetJob (Either String (Maybe PackageSetJobDetails) -> a)
  | InsertLogLine LogLine a
  | SelectLogsByJob JobId LogLevel (Maybe DateTime) (Array LogLine -> a)
  | ResetIncompleteJobs a

derive instance Functor Db

-- | An effect for accessing the database.
type DB r = (db :: Db | r)

_db :: Proxy "db"
_db = Proxy

-- | Insert a new log line into the database.
insertLog :: forall r. LogLine -> Run (DB + r) Unit
insertLog log = Run.lift _db (InsertLogLine log unit)

-- | Select all logs for a given job, filtered by loglevel.
selectLogsByJob :: forall r. JobId -> LogLevel -> Maybe DateTime -> Run (DB + r) (Array LogLine)
selectLogsByJob jobId logLevel since = Run.lift _db (SelectLogsByJob jobId logLevel since identity)

-- | Set a job in the database to the 'finished' state.
finishJob :: forall r. FinishJob -> Run (DB + r) Unit
finishJob job = Run.lift _db (FinishJob job unit)

-- | Select a job by ID from the database.
selectJobInfo :: forall r. JobId -> Run (DB + EXCEPT String + r) (Maybe JobInfo)
selectJobInfo jobId = Run.lift _db (SelectJobInfo jobId identity) >>= Except.rethrow

-- | Insert a new package job into the database.
insertPackageJob :: forall r. InsertPackageJob -> Run (DB + r) Unit
insertPackageJob job = Run.lift _db (InsertPackageJob job unit)

-- | Insert a new matrix job into the database.
insertMatrixJob :: forall r. InsertMatrixJob -> Run (DB + r) Unit
insertMatrixJob job = Run.lift _db (InsertMatrixJob job unit)

-- | Insert a new package set job into the database.
insertPackageSetJob :: forall r. InsertPackageSetJob -> Run (DB + r) Unit
insertPackageSetJob job = Run.lift _db (InsertPackageSetJob job unit)

-- | Start a job in the database.
startJob :: forall r. StartJob -> Run (DB + r) Unit
startJob job = Run.lift _db (StartJob job unit)

-- | Select the next package job from the database.
selectNextPackageJob :: forall r. Run (DB + EXCEPT String + r) (Maybe PackageJobDetails)
selectNextPackageJob = Run.lift _db (SelectNextPackageJob identity) >>= Except.rethrow

-- | Select the next matrix job from the database.
selectNextMatrixJob :: forall r. Run (DB + EXCEPT String + r) (Maybe MatrixJobDetails)
selectNextMatrixJob = Run.lift _db (SelectNextMatrixJob identity) >>= Except.rethrow

-- | Select the next package set job from the database.
selectNextPackageSetJob :: forall r. Run (DB + EXCEPT String + r) (Maybe PackageSetJobDetails)
selectNextPackageSetJob = Run.lift _db (SelectNextPackageSetJob identity) >>= Except.rethrow

-- | Delete all incomplete jobs from the database.
resetIncompleteJobs :: forall r. Run (DB + r) Unit
resetIncompleteJobs = Run.lift _db (ResetIncompleteJobs unit)

interpret :: forall r a. (Db ~> Run r) -> Run (DB + r) a -> Run r a
interpret handler = Run.interpret (Run.on _db handler Run.send)

type SQLiteEnv = { db :: SQLite }

-- | Interpret DB by interacting with the SQLite database on disk.
handleSQLite :: forall r a. SQLiteEnv -> Db a -> Run (LOG + EFFECT + r) a
handleSQLite env = case _ of
  InsertPackageJob job next -> do
    Run.liftEffect $ SQLite.insertPackageJob env.db job
    pure next

  InsertMatrixJob job next -> do
    Run.liftEffect $ SQLite.insertMatrixJob env.db job
    pure next

  InsertPackageSetJob job next -> do
    Run.liftEffect $ SQLite.insertPackageSetJob env.db job
    pure next

  FinishJob job next -> do
    Run.liftEffect $ SQLite.finishJob env.db job
    pure next

  StartJob job next -> do
    Run.liftEffect $ SQLite.startJob env.db job
    pure next

  SelectJobInfo jobId reply -> do
    result <- Run.liftEffect $ SQLite.selectJobInfo env.db jobId
    pure $ reply result

  SelectNextPackageJob reply -> do
    result <- Run.liftEffect $ SQLite.selectNextPackageJob env.db
    pure $ reply result

  SelectNextMatrixJob reply -> do
    result <- Run.liftEffect $ SQLite.selectNextMatrixJob env.db
    pure $ reply result

  SelectNextPackageSetJob reply -> do
    result <- Run.liftEffect $ SQLite.selectNextPackageSetJob env.db
    pure $ reply result

  InsertLogLine log next -> do
    Run.liftEffect $ SQLite.insertLogLine env.db log
    pure next

  SelectLogsByJob jobId logLevel since reply -> do
    { fail, success } <- Run.liftEffect $ SQLite.selectLogsByJob env.db jobId logLevel since
    unless (Array.null fail) do
      Log.warn $ "Some logs are not readable: " <> String.joinWith "\n" fail
    pure $ reply success

  ResetIncompleteJobs next -> do
    Run.liftEffect $ SQLite.resetIncompleteJobs env.db
    pure next
