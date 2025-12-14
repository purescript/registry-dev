module Registry.App.Server.JobExecutor
  ( runJobExecutor
  , newJobId
  ) where

import Registry.App.Prelude hiding ((/))

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel as Parallel
import Data.DateTime (DateTime)
import Data.UUID.Random as UUID
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Registry.API.V1 (JobId(..))
import Registry.App.API as API
import Registry.App.Effect.Db (DB)
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log as Log
import Registry.App.SQLite (MatrixJobDetails, PackageJobDetails, PackageSetJobDetails)
import Registry.App.Server.Env (ServerEffects, ServerEnv, runEffects)
import Registry.App.Server.MatrixBuilder as MatrixBuilder
import Registry.ManifestIndex as ManifestIndex
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
  Db.resetIncompleteJobs
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

-- TODO: here we only get a single package for each operation, but really we should
-- have all of them and toposort them. There is something in ManifestIndex but not
-- sure that's what we need
findNextAvailableJob :: forall r. Run (DB + EXCEPT String + r) (Maybe JobDetails)
findNextAvailableJob = runMaybeT
  $ (PackageJob <$> MaybeT Db.selectNextPackageJob)
  <|> (MatrixJob <$> MaybeT Db.selectNextMatrixJob)
  <|> (PackageSetJob <$> MaybeT Db.selectNextPackageSetJob)

newJobId :: forall m. MonadEffect m => m JobId
newJobId = do
  id <- UUID.make
  pure $ JobId $ UUID.toString id

executeJob :: DateTime -> JobDetails -> Run ServerEffects Unit
executeJob _ = case _ of
  PackageJob { payload: Operation.Publish payload@{ compiler, name, version } } -> do
    maybeDependencies <- API.publish Nothing payload
    -- The above operation will throw if not successful, and return a map of
    -- dependencies of the package only if it has not been published before.
    for_ maybeDependencies \dependencies -> do
      -- At this point this package has been verified with one compiler only.
      -- So we need to enqueue compilation jobs for (1) same package, all the other
      -- compilers, and (2) same compiler, all packages that depend on this one
      -- TODO here we are building the compiler index, but we should really cache it
      compilerIndex <- MatrixBuilder.readCompilerIndex
      let solverData = { compiler, name, version, dependencies, compilerIndex }
      samePackageAllCompilers <- MatrixBuilder.solveForAllCompilers solverData
      sameCompilerAllDependants <- MatrixBuilder.solveDependantsForCompiler solverData
      for (samePackageAllCompilers <> sameCompilerAllDependants) \matrixJob -> do
        Log.info $ "Enqueuing matrix job" -- TODO print details
        jobId <- newJobId
        Db.insertMatrixJob { jobId, payload: matrixJob }
  PackageJob { payload: Operation.Authenticated auth } ->
    API.authenticated auth
  MatrixJob details ->
    -- TODO this job should return the success result, because if successful we need
    -- to enqueue more matrix jobs: all its dependents for this same compiler version
    MatrixBuilder.runMatrixJob details
  PackageSetJob _details ->
    -- TODO: need to pass in the package_sets effect
    -- API.packageSetUpdate2 details
    pure unit
