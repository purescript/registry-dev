module Registry.App.Server.Router where

import Registry.App.Prelude hiding ((/))

import Control.Monad.Cont (ContT)
import Data.Codec.JSON as CJ
import Data.String as String
import Data.UUID.Random as UUID
import Effect.Aff as Aff
import Effect.Class.Console as Console
import HTTPurple (Method(..), Request, Response)
import HTTPurple as HTTPurple
import HTTPurple.Status as Status
import Registry.API.V1 (JobId(..), LogLevel(..), Route(..))
import Registry.API.V1 as V1
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log as Log
import Registry.App.Server.Env (ServerEffects, ServerEnv, jsonDecoder, jsonOk, runEffects)
import Registry.Operation (PackageOperation)
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Run (Run)
import Run.Except as Run.Except

runRouter :: ServerEnv -> Effect Unit
runRouter env = do
  void $ HTTPurple.serve
    { hostname: "0.0.0.0"
    , port: 8080
    , onStarted
    }
    { route: V1.routes
    , router: runServer
    }
  where
  onStarted :: Effect Unit
  onStarted = do
    Console.log $ String.joinWith "\n"
      [ " ┌───────────────────────────────────────────┐"
      , " │ Server now up on port 8080                │"
      , " │                                           │"
      , " │ To test, run:                             │"
      , " │  > curl -v localhost:8080/api/v1/jobs     │"
      , " └───────────────────────────────────────────┘"
      ]

  runServer :: Request Route -> Aff Response
  runServer request = do
    result <- runEffects env (router request)
    case result of
      Left error -> HTTPurple.badRequest (Aff.message error)
      Right response -> pure response

router :: Request Route -> Run ServerEffects Response
router { route, method, body } = HTTPurple.usingCont case route, method of
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
    jsonOk (CJ.array V1.jobCodec) [{ jobId: wrap "foo", createdAt: bottom, finishedAt: Nothing, success: true, logs: [] }]

  Job jobId { level: maybeLogLevel, since }, Get -> do
    let logLevel = fromMaybe Error maybeLogLevel
    logs <- lift $ Db.selectLogsByJob jobId logLevel since
    lift (Run.Except.runExcept $ Db.selectJobInfo jobId) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching job: " <> err
        HTTPurple.notFound
      Right Nothing ->
        HTTPurple.notFound
      Right (Just job) ->
        jsonOk V1.jobCodec
          { jobId
          , createdAt: job.createdAt
          , finishedAt: job.finishedAt
          , success: job.success
          , logs
          }

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
