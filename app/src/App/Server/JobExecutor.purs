module Registry.App.Server.JobExecutor (runJobExecutor) where

import Registry.App.Prelude hiding ((/))

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel as Parallel
import Data.DateTime (DateTime)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Registry.App.API as API
import Registry.App.Effect.Db (DB)
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log as Log
import Registry.App.SQLite (MatrixJobDetails, PackageJobDetails, PackageSetJobDetails)
import Registry.App.Server.Env (ServerEffects, ServerEnv, runEffects)
import Registry.Operation as Operation
import Run (Run)
import Run.Except (EXCEPT)

data JobDetails
  = PackageJob PackageJobDetails
  | MatrixJob MatrixJobDetails
  | PackageSetJob PackageSetJobDetails

runJobExecutor :: ServerEnv -> Aff (Either Aff.Error Unit)
runJobExecutor env = runEffects env do
  Log.info "Starting Job Executor"
  Db.deleteIncompleteJobs
  loop
  where
  loop = do
    maybeJob <- findNextAvailableJob
    case maybeJob of
      Nothing -> do
        liftAff $ Aff.delay (Milliseconds 1000.0)
        loop

      Just job -> do
        now <- nowUTC
        let
          jobId = case job of
            PackageJob details -> details.jobId
            MatrixJob details -> details.jobId
            PackageSetJob details -> details.jobId

        Db.startJob { jobId, startedAt: now }

        -- We race the job execution against a timeout; if the timeout happens first,
        -- we kill the job and move on to the next one.
        jobResult <- liftAff do
          let execute = Just <$> (runEffects env $ executeJob now job)
          let delay = 1000.0 * 60.0 * 5.0 -- 5 minutes
          let timeout = Aff.delay (Milliseconds delay) $> Nothing
          Parallel.sequential $ Parallel.parallel execute <|> Parallel.parallel timeout

        success <- case jobResult of
          Nothing -> do
            Log.error $ "Job " <> unwrap jobId <> " timed out."
            pure false

          Just (Left err) -> do
            Log.warn $ "Job " <> unwrap jobId <> " failed:\n" <> Aff.message err
            pure false

          Just (Right _) -> do
            Log.info $ "Job " <> unwrap jobId <> " succeeded."
            pure true

        Db.finishJob { jobId, finishedAt: now, success }
        loop

findNextAvailableJob :: forall r. Run (DB + EXCEPT String + r) (Maybe JobDetails)
findNextAvailableJob = runMaybeT
  $ (PackageJob <$> MaybeT Db.selectNextPackageJob)
  <|> (MatrixJob <$> MaybeT Db.selectNextMatrixJob)
  <|> (PackageSetJob <$> MaybeT Db.selectNextPackageSetJob)

executeJob :: DateTime -> JobDetails -> Run ServerEffects Unit
executeJob _ = case _ of
  PackageJob { payload: Operation.Publish p } ->
    API.publish Nothing p
  PackageJob { payload: Operation.Authenticated auth } ->
    API.authenticated auth

  MatrixJob _details ->
    pure unit -- UNIMPLEMENTED
  PackageSetJob _details ->
    pure unit -- UNIMPLEMENTED
