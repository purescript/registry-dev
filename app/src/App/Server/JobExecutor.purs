module Registry.App.Server.JobExecutor
  ( runJobExecutor
  ) where

import Registry.App.Prelude hiding ((/))

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Parallel as Parallel
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Set as Set
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Record as Record
import Registry.API.V1 (Job(..))
import Registry.API.V1 as V1
import Registry.App.API as API
import Registry.App.Effect.Db (DB)
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Server.Env (ServerEffects, ServerEnv, runEffects)
import Registry.App.Server.MatrixBuilder as MatrixBuilder
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (Run)
import Run.Except (EXCEPT)

runJobExecutor :: ServerEnv -> Aff (Either Aff.Error Unit)
runJobExecutor env = runEffects env do
  Log.info "Starting Job Executor"
  -- Before starting the executor we check if we need to run a whole-registry
  -- compiler update: whenever a new compiler is published we need to see which
  -- packages are compatible with it; this is a responsibility of the MatrixBuilder,
  -- but it needs to be triggered to know there's a new version out.
  -- To do that, we ask PursVersions what the compilers are, then we look for
  -- the compatibility list of the latest `prelude` version. If the new compiler
  -- is missing, then we know that we have not attempted to check compatibility
  -- with it (since the latest `prelude` has to be compatible by definition),
  -- and we can enqueue a "compile everything" here, which will be the first
  -- thing that the JobExecutor picks up
  void $ MatrixBuilder.checkIfNewCompiler
    >>= traverse upgradeRegistryToNewCompiler

  -- Reset incomplete jobs. The DB tracks how many times each job has been reset.
  -- Returns job IDs that have been reset 3+ times (livelocked).
  livelockedJobIds <- Db.resetIncompleteJobs
  unless (Array.null livelockedJobIds) do
    -- FIXME: Add proper alerting to trustees (webhook, email, etc.)
    for_ livelockedJobIds \jobId ->
      Log.error $ "LIVELOCK DETECTED: Job " <> unwrap jobId
        <> " has been reset 3+ times. This indicates a persistent failure "
        <> "that requires investigation."
    Log.error $ "Pausing job processing for 5 minutes to allow investigation..."
    liftAff $ Aff.delay (Milliseconds (1000.0 * 60.0 * 5.0))

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
          jobId = (V1.jobInfo job).jobId

        Db.startJob { jobId, startedAt: now }

        -- We race the job execution against a timeout; if the timeout happens first,
        -- we kill the job and move on to the next one.
        -- Note: we set env.jobId so that logs are written to the database.
        jobResult <- liftAff do
          let envWithJobId = env { jobId = Just jobId }
          let execute = Just <$> (runEffects envWithJobId $ executeJob now job)
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

        finishedAt <- nowUTC
        Db.finishJob { jobId, finishedAt, success }
        loop

-- TODO: here we only get a single package for each operation, but really we should
-- have all of them and toposort them. There is something in ManifestIndex but not
-- sure that's what we need
findNextAvailableJob :: forall r. Run (DB + EXCEPT String + r) (Maybe Job)
findNextAvailableJob = runMaybeT
  $ (PublishJob <<< Record.merge { logs: [], jobType: Proxy :: _ "publish" } <$> MaybeT Db.selectNextPublishJob)
  <|> (UnpublishJob <<< Record.merge { logs: [], jobType: Proxy :: _ "unpublish" } <$> MaybeT Db.selectNextUnpublishJob)
  <|> (TransferJob <<< Record.merge { logs: [], jobType: Proxy :: _ "transfer" } <$> MaybeT Db.selectNextTransferJob)
  <|> (MatrixJob <<< Record.merge { logs: [], jobType: Proxy :: _ "matrix" } <$> MaybeT Db.selectNextMatrixJob)
  <|> (PackageSetJob <<< Record.merge { logs: [], jobType: Proxy :: _ "packageset" } <$> MaybeT Db.selectNextPackageSetJob)

executeJob :: DateTime -> Job -> Run ServerEffects Unit
executeJob _ = case _ of
  PublishJob { payload: payload@{ name } } -> do
    -- `publish` will throw on error, or return `Nothing` if the pipeline
    -- exited with a valid status but without publishing a package (for instance,
    -- the package already existed), or return `Just` if a new version was
    -- published and we need to queue matrix jobs.
    maybeResult <- API.publish payload
    for_ maybeResult \{ compiler, dependencies, version } -> do
      -- At this point this package has been verified with one compiler only.
      -- So we need to enqueue compilation jobs for (1) same package, all the other
      -- compilers, and (2) same compiler, all packages that depend on this one
      -- TODO here we are building the compiler index, but we should really cache it
      compilerIndex <- MatrixBuilder.readCompilerIndex
      let solverData = { compiler, name, version, dependencies, compilerIndex }
      samePackageAllCompilers <- MatrixBuilder.solveForAllCompilers solverData
      sameCompilerAllDependants <- MatrixBuilder.solveDependantsForCompiler solverData
      for (Array.fromFoldable $ Set.union samePackageAllCompilers sameCompilerAllDependants) \{ compiler: solvedCompiler, resolutions, name: solvedPackage, version: solvedVersion } -> do
        Log.info $ Array.fold
          [ "Enqueuing matrix job: compiler "
          , Version.print solvedCompiler
          , ", package "
          , PackageName.print solvedPackage
          , "@"
          , Version.print solvedVersion
          ]
        Db.insertMatrixJob
          { payload: resolutions
          , compilerVersion: solvedCompiler
          , packageName: solvedPackage
          , packageVersion: solvedVersion
          }
  UnpublishJob { payload } -> API.authenticated payload
  TransferJob { payload } -> API.authenticated payload
  MatrixJob details@{ packageName, packageVersion } -> do
    -- After publishing a matrix job, we check if any dependents need to also have
    -- a job queued (for instance a new compiler version came out, we want to have
    -- packages trigger jobs for their dependents so it cascades through the registry)
    dependencies <- MatrixBuilder.runMatrixJob details

    -- TODO here we are building the compiler index, but we should really cache it
    compilerIndex <- MatrixBuilder.readCompilerIndex
    let solverData = { compiler: details.compilerVersion, name: packageName, version: packageVersion, dependencies, compilerIndex }
    sameCompilerAllDependants <- MatrixBuilder.solveDependantsForCompiler solverData
    for_ (Array.fromFoldable sameCompilerAllDependants) \{ compiler: solvedCompiler, resolutions, name: solvedPackage, version: solvedVersion } -> do
      Log.info $ Array.fold
        [ "Enqueuing matrix job: compiler "
        , Version.print solvedCompiler
        , ", package "
        , PackageName.print solvedPackage
        , "@"
        , Version.print solvedVersion
        ]
      Db.insertMatrixJob
        { payload: resolutions
        , compilerVersion: solvedCompiler
        , packageName: solvedPackage
        , packageVersion: solvedVersion
        }
  PackageSetJob payload -> API.packageSetUpdate payload

upgradeRegistryToNewCompiler :: forall r. Version -> Run (DB + LOG + EXCEPT String + REGISTRY + r) Unit
upgradeRegistryToNewCompiler newCompilerVersion = do
  Log.info $ "New compiler found: " <> Version.print newCompilerVersion
  Log.info "Starting upgrade of the whole registry to the new compiler..."
  allManifests <- Registry.readAllManifests
  for_ (ManifestIndex.toArray allManifests) \(Manifest manifest) -> do
    -- Note: we enqueue compilation jobs only for packages with no dependencies,
    -- because from them we should be able to reach the whole of the registry,
    -- as they complete new jobs for their dependants will be queued up.
    when (Map.isEmpty manifest.dependencies) do
      Log.info $ Array.fold
        [ "Enqueuing matrix job for _new_ compiler "
        , Version.print newCompilerVersion
        , ", package "
        , PackageName.print manifest.name
        , "@"
        , Version.print manifest.version
        ]
      void $ Db.insertMatrixJob
        { payload: Map.empty
        , compilerVersion: newCompilerVersion
        , packageName: manifest.name
        , packageVersion: manifest.version
        }
