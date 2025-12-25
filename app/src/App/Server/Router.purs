module Registry.App.Server.Router where

import Registry.App.Prelude hiding ((/))

import Data.Codec.JSON as CJ
import Effect.Aff as Aff
import HTTPurple (Method(..), Request, Response)
import HTTPurple as HTTPurple
import HTTPurple.Status as Status
import Registry.API.V1 (Route(..))
import Registry.API.V1 as V1
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log as Log
import Registry.App.Server.Env (ServerEffects, ServerEnv, jsonDecoder, jsonOk, runEffects)
import Registry.Operation as Operation
import Run (Run)
import Run.Except as Run.Except

runRouter :: ServerEnv -> Effect Unit
runRouter env = do
  -- Read port from SERVER_PORT env var (optional, HTTPurple defaults to 8080)
  port <- liftEffect $ Env.lookupOptional Env.serverPort
  void $ HTTPurple.serve
    { hostname: "0.0.0.0"
    , port
    }
    { route: V1.routes
    , router: runServer
    }
  where
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
    jobId <- lift $ Db.insertPublishJob { payload: publish }
    jsonOk V1.jobCreatedResponseCodec { jobId }

  Unpublish, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Unpublish payload -> do
        lift $ Log.info $ "Received Unpublish request: " <> printJson Operation.unpublishCodec payload
        jobId <- lift $ Db.insertUnpublishJob
          { payload: payload
          , rawPayload: auth.rawPayload
          , signature: auth.signature
          }
        jsonOk V1.jobCreatedResponseCodec { jobId }
      _ ->
        HTTPurple.badRequest "Expected unpublish operation."

  Transfer, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Transfer payload -> do
        lift $ Log.info $ "Received Transfer request: " <> printJson Operation.transferCodec payload
        jobId <- lift $ Db.insertTransferJob
          { payload: payload
          , rawPayload: auth.rawPayload
          , signature: auth.signature
          }
        jsonOk V1.jobCreatedResponseCodec { jobId }
      _ ->
        HTTPurple.badRequest "Expected transfer operation."

  Jobs { since, include_completed }, Get -> do
    -- TODO should probably be 1h ago instead of now
    now <- liftEffect nowUTC
    lift
      ( Run.Except.runExcept $ Db.selectJobs
          { includeCompleted: fromMaybe false include_completed
          , since: fromMaybe now since
          }
      ) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching jobs: " <> err
        HTTPurple.internalServerError $ "Error while fetching jobs: " <> err
      Right jobs -> jsonOk (CJ.array V1.jobCodec) jobs

  Job jobId { level: maybeLogLevel, since }, Get -> do
    now <- liftEffect nowUTC
    lift (Run.Except.runExcept $ Db.selectJob { jobId, level: maybeLogLevel, since: fromMaybe now since }) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching job: " <> err
        HTTPurple.internalServerError $ "Error while fetching job: " <> err
      Right Nothing -> do
        HTTPurple.notFound
      Right (Just job) -> jsonOk V1.jobCodec job

  Status, Get ->
    HTTPurple.emptyResponse Status.ok

  Status, Head ->
    HTTPurple.emptyResponse Status.ok

  _, _ ->
    HTTPurple.notFound
