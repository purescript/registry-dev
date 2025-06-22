module Registry.App.JobExecutor where

import Registry.App.Prelude hiding ((/))

import Control.Parallel as Parallel
import Data.DateTime (DateTime)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Registry.API.V1 (JobId(..))
import Registry.App.Effect.Db (DB)
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log as Log
import Registry.App.SQLite (MatrixJobDetails, PackageJobDetails, PackageSetJobDetails)
import Registry.App.Server.Env (ServerEnv, ServerEffects, runEffects)
import Run (Run)
import Run.Except (EXCEPT)

data JobDetails
  = PackageJob PackageJobDetails
  | MatrixJob MatrixJobDetails
  | PackageSetJob PackageSetJobDetails

findNextAvailableJob :: forall r. Run (DB + EXCEPT String + r) (Maybe JobDetails)
findNextAvailableJob = do
  Db.selectNextPackageJob >>= case _ of
    Just job -> pure $ Just $ PackageJob job
    Nothing -> Db.selectNextMatrixJob >>= case _ of
      Just job -> pure $ Just $ MatrixJob job
      Nothing -> Db.selectNextPackageSetJob >>= case _ of
        Just job -> pure $ Just $ PackageSetJob job
        Nothing -> pure Nothing

runJobExecutor :: ServerEnv -> Aff (Either Aff.Error Unit)
runJobExecutor env = do
  runEffects env Db.deleteIncompleteJobs >>= case _ of
    Left err -> pure $ Left err
    Right _ -> loop
  where
  loop = runEffects env findNextAvailableJob >>= case _ of
    Left err ->
      pure $ Left err

    Right Nothing -> do
      Aff.delay (Milliseconds 100.0)
      loop

    Right (Just job) -> do
      now <- nowUTC

      let
        jobId = case job of
          PackageJob details -> details.jobId
          MatrixJob details -> details.jobId
          PackageSetJob details -> details.jobId

      -- We race the job execution against a timeout; if the timeout happens first,
      -- we kill the job and move on to the next one.
      jobResult <- do
        let execute = Just <$> runEffects env (executeJob now job)
        let delay = 1000.0 * 60.0 * 5.0 -- 5 minutes
        let timeout = Aff.delay (Milliseconds delay) $> Nothing
        Parallel.sequential $ Parallel.parallel execute <|> Parallel.parallel timeout

      finishResult <- runEffects env case jobResult of
        Nothing -> do
          Log.error $ "Job " <> un JobId jobId <> " timed out."
          Db.finishJob { jobId, finishedAt: now, success: false }

        Just (Left err) -> do
          Log.warn $ "Job " <> un JobId jobId <> " failed:\n" <> Aff.message err
          Db.finishJob { jobId, finishedAt: now, success: false }

        Just (Right _) -> do
          Log.info $ "Job " <> un JobId jobId <> " succeeded."
          Db.finishJob { jobId, finishedAt: now, success: true }

      case finishResult of
        Left err -> pure $ Left err
        Right _ -> loop

executeJob :: DateTime -> JobDetails -> Run ServerEffects Unit
executeJob now = case _ of
  PackageJob { jobId } -> do
    Db.startJob { jobId, startedAt: now }
    pure unit -- UNIMPLEMENTED
  MatrixJob _details ->
    pure unit -- UNIMPLEMENTED
  PackageSetJob _details ->
    pure unit -- UNIMPLEMENTED
