module Registry.App.Effect.Db where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.String as String
import Registry.API.V1 (Job, JobId, LogLevel, LogLine)
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.SQLite
  ( FinishJob
  , InsertMatrixJob
  , InsertPackageSetJob
  , InsertPublishJob
  , InsertTransferJob
  , InsertUnpublishJob
  , MatrixJobDetails
  , PackageSetJobDetails
  , PublishJobDetails
  , SQLite
  , SelectJobRequest
  , StartJob
  , TransferJobDetails
  , UnpublishJobDetails
  )
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
  = InsertPublishJob InsertPublishJob (JobId -> a)
  | InsertUnpublishJob InsertUnpublishJob (JobId -> a)
  | InsertTransferJob InsertTransferJob (JobId -> a)
  | InsertMatrixJob InsertMatrixJob (JobId -> a)
  | InsertPackageSetJob InsertPackageSetJob (JobId -> a)
  | FinishJob FinishJob a
  | StartJob StartJob a
  | SelectJob SelectJobRequest (Either String (Maybe Job) -> a)
  | SelectNextPublishJob (Either String (Maybe PublishJobDetails) -> a)
  | SelectNextUnpublishJob (Either String (Maybe UnpublishJobDetails) -> a)
  | SelectNextTransferJob (Either String (Maybe TransferJobDetails) -> a)
  | SelectNextMatrixJob (Either String (Maybe MatrixJobDetails) -> a)
  | SelectNextPackageSetJob (Either String (Maybe PackageSetJobDetails) -> a)
  | InsertLogLine LogLine a
  | SelectLogsByJob JobId LogLevel DateTime (Array LogLine -> a)
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
selectLogsByJob :: forall r. JobId -> LogLevel -> DateTime -> Run (DB + r) (Array LogLine)
selectLogsByJob jobId logLevel since = Run.lift _db (SelectLogsByJob jobId logLevel since identity)

-- | Set a job in the database to the 'finished' state.
finishJob :: forall r. FinishJob -> Run (DB + r) Unit
finishJob job = Run.lift _db (FinishJob job unit)

-- | Select a job by ID from the database.
selectJob :: forall r. SelectJobRequest -> Run (DB + EXCEPT String + r) (Maybe Job)
selectJob request = Run.lift _db (SelectJob request identity) >>= Except.rethrow

-- | Insert a new publish job into the database.
insertPublishJob :: forall r. InsertPublishJob -> Run (DB + r) JobId
insertPublishJob job = Run.lift _db (InsertPublishJob job identity)

-- | Insert a new unpublish job into the database.
insertUnpublishJob :: forall r. InsertUnpublishJob -> Run (DB + r) JobId
insertUnpublishJob job = Run.lift _db (InsertUnpublishJob job identity)

-- | Insert a new transfer job into the database.
insertTransferJob :: forall r. InsertTransferJob -> Run (DB + r) JobId
insertTransferJob job = Run.lift _db (InsertTransferJob job identity)

-- | Insert a new matrix job into the database.
insertMatrixJob :: forall r. InsertMatrixJob -> Run (DB + r) JobId
insertMatrixJob job = Run.lift _db (InsertMatrixJob job identity)

-- | Insert a new package set job into the database.
insertPackageSetJob :: forall r. InsertPackageSetJob -> Run (DB + r) JobId
insertPackageSetJob job = Run.lift _db (InsertPackageSetJob job identity)

-- | Start a job in the database.
startJob :: forall r. StartJob -> Run (DB + r) Unit
startJob job = Run.lift _db (StartJob job unit)

-- | Select the next publish job from the database.
selectNextPublishJob :: forall r. Run (DB + EXCEPT String + r) (Maybe PublishJobDetails)
selectNextPublishJob = Run.lift _db (SelectNextPublishJob identity) >>= Except.rethrow

-- | Select the next unpublish job from the database.
selectNextUnpublishJob :: forall r. Run (DB + EXCEPT String + r) (Maybe UnpublishJobDetails)
selectNextUnpublishJob = Run.lift _db (SelectNextUnpublishJob identity) >>= Except.rethrow

-- | Select the next transfer job from the database.
selectNextTransferJob :: forall r. Run (DB + EXCEPT String + r) (Maybe TransferJobDetails)
selectNextTransferJob = Run.lift _db (SelectNextTransferJob identity) >>= Except.rethrow

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
  InsertPublishJob job reply -> do
    result <- Run.liftEffect $ SQLite.insertPublishJob env.db job
    pure $ reply result

  InsertUnpublishJob job reply -> do
    result <- Run.liftEffect $ SQLite.insertUnpublishJob env.db job
    pure $ reply result

  InsertTransferJob job reply -> do
    result <- Run.liftEffect $ SQLite.insertTransferJob env.db job
    pure $ reply result

  InsertMatrixJob job reply -> do
    result <- Run.liftEffect $ SQLite.insertMatrixJob env.db job
    pure $ reply result

  InsertPackageSetJob job reply -> do
    result <- Run.liftEffect $ SQLite.insertPackageSetJob env.db job
    pure $ reply result

  FinishJob job next -> do
    Run.liftEffect $ SQLite.finishJob env.db job
    pure next

  StartJob job next -> do
    Run.liftEffect $ SQLite.startJob env.db job
    pure next

  SelectJob request reply -> do
    result <- Run.liftEffect $ SQLite.selectJob env.db request
    pure $ reply result

  SelectNextPublishJob reply -> do
    result <- Run.liftEffect $ SQLite.selectNextPublishJob env.db
    pure $ reply result

  SelectNextUnpublishJob reply -> do
    result <- Run.liftEffect $ SQLite.selectNextUnpublishJob env.db
    pure $ reply result

  SelectNextTransferJob reply -> do
    result <- Run.liftEffect $ SQLite.selectNextTransferJob env.db
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
