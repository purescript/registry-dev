module Registry.App.Server.Router where

import Registry.App.Prelude hiding ((/))

import Control.Monad.Cont (ContT)
import Data.Codec.JSON as CJ
import Data.UUID.Random as UUID
import HTTPurple (Method(..), Request, Response)
import HTTPurple as HTTPurple
import HTTPurple.Status as Status
import Registry.API.V1 (JobId(..), LogLevel(..), Route(..))
import Registry.API.V1 as V1
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log as Log
import Registry.App.Server.Env (ServerEffects, ServerEnv, jsonDecoder, jsonOk)
import Registry.Operation (PackageOperation)
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Run (Run)
import Run.Except as Run.Except

router :: ServerEnv -> Request Route -> Run ServerEffects Response
router env { route, method, body } = HTTPurple.usingCont case route, method of
  Publish, Post -> do
    publish <- HTTPurple.fromJson (jsonDecoder Operation.publishCodec) body
    lift $ Log.info $ "Received Publish request: " <> printJson Operation.publishCodec publish
    forkPackageJob $ Operation.Publish publish

  Unpublish, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Unpublish payload -> do
        lift $ Log.info $ "Received Unpublish request: " <> printJson Operation.unpublishCodec payload
        forkPackageJob $ Operation.Authenticated auth
      _ ->
        HTTPurple.badRequest "Expected unpublish operation."

  Transfer, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Transfer payload -> do
        lift $ Log.info $ "Received Transfer request: " <> printJson Operation.transferCodec payload
        forkPackageJob $ Operation.Authenticated auth
      _ ->
        HTTPurple.badRequest "Expected transfer operation."

  Jobs, Get -> do
    jsonOk (CJ.array V1.jobCodec) []

  Job jobId { level: maybeLogLevel, since }, Get -> do
    let logLevel = fromMaybe Error maybeLogLevel
    logs <- lift $ Db.selectLogsByJob jobId logLevel since
    lift (Run.Except.runExcept (Db.selectJobInfo jobId)) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching job: " <> err
        HTTPurple.notFound
      Right Nothing ->
        HTTPurple.notFound
      Right (Just job) -> do
        HTTPurple.emptyResponse Status.ok
        -- TODO: Return the job details (will need to update the jobCodec and move the various
        -- details into the API module).
        -- jsonOk V1.jobCodec (jobDetailstoV1Job job logs)

  Status, Get ->
    HTTPurple.emptyResponse Status.ok

  Status, Head ->
    HTTPurple.emptyResponse Status.ok

  _, _ ->
    HTTPurple.notFound
  where
  forkPackageJob :: PackageOperation -> ContT Response (Run _) Response
  forkPackageJob operation = do
    lift $ Log.info $ "Enqueuing job for package " <> PackageName.print (Operation.packageName operation)
    jobId <- newJobId
    lift $ Db.insertPackageJob { jobId, payload: operation }
    jsonOk V1.jobCreatedResponseCodec { jobId }

  newJobId :: forall m. MonadEffect m => m JobId
  newJobId = liftEffect do
    id <- UUID.make
    pure $ JobId $ UUID.toString id
