module Registry.App.Server.Router where

import Registry.App.Prelude hiding ((/))

import Data.Codec.JSON as CJ
import Effect.Aff as Aff
import Effect.Class.Console as Console
import HTTPurple (Method(..), Request, Response)
import HTTPurple as HTTPurple
import HTTPurple.Status as Status
import Registry.API.V1 (Route(..))
import Registry.API.V1 as V1
import Registry.App.API as API
import Registry.App.Auth as Auth
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log as Log
import Registry.App.Server.Env (ServerEffects, ServerEnv, jsonDecoder, jsonOk, runEffects)
import Registry.Operation (PackageSetOperation(..))
import Registry.Operation as Operation
import Run (Run)
import Run as Run
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
    , router: corsMiddleware runServer
    }
  where
  runServer :: Request Route -> Aff Response
  runServer request = do
    result <- runEffects env (router request)
    case result of
      Left error -> do
        Console.log $ "Bad request: " <> Aff.message error
        HTTPurple.badRequest (Aff.message error)
      Right response -> pure response

-- | CORS middleware that wraps the router.
-- | - OPTIONS requests return a 204 preflight response
-- | - All other responses have CORS headers appended
corsMiddleware :: (Request Route -> Aff Response) -> Request Route -> Aff Response
corsMiddleware next request = case request.method of
  Options ->
    HTTPurple.emptyResponse' Status.noContent preflightHeaders
  _ -> do
    response <- next request
    pure $ response { headers = response.headers <> corsHeaders }
  where
  corsHeaders =
    HTTPurple.header "Access-Control-Allow-Origin" "*"
      <> HTTPurple.header "Access-Control-Allow-Methods" "GET, HEAD, POST, OPTIONS"
      <> HTTPurple.header "Vary" "Origin"

  preflightHeaders =
    corsHeaders
      <> HTTPurple.header "Access-Control-Allow-Headers" "Content-Type"
      <> HTTPurple.header "Access-Control-Max-Age" "86400"

router :: Request Route -> Run ServerEffects Response
router { route, method, body } = HTTPurple.usingCont case route, method of
  Publish, Post -> do
    publish <- HTTPurple.fromJson (jsonDecoder Operation.publishCodec) body
    lift $ Log.info $ "Received Publish request: " <> printJson Operation.publishCodec publish

    jobId <- lift (Db.selectPublishJob publish.name publish.version) >>= case _ of
      Just job -> do
        lift $ Log.warn $ "Duplicate publish job insertion, returning existing one: " <> unwrap job.jobId
        pure job.jobId
      Nothing -> do
        lift $ Db.insertPublishJob { payload: publish }

    jsonOk V1.jobCreatedResponseCodec { jobId }

  Unpublish, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Unpublish payload -> do
        lift $ Log.info $ "Received Unpublish request: " <> printJson Operation.unpublishCodec payload

        jobId <- lift (Db.selectUnpublishJob payload.name payload.version) >>= case _ of
          Just job -> do
            lift $ Log.warn $ "Duplicate unpublish job insertion, returning existing one: " <> unwrap job.jobId
            pure job.jobId
          Nothing -> do
            lift $ Db.insertUnpublishJob
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

        jobId <- lift (Db.selectTransferJob payload.name) >>= case _ of
          Just job -> do
            lift $ Log.warn $ "Duplicate transfer job insertion, returning existing one: " <> unwrap job.jobId
            pure job.jobId
          Nothing -> do
            lift $ Db.insertTransferJob
              { payload: payload
              , rawPayload: auth.rawPayload
              , signature: auth.signature
              }

        jsonOk V1.jobCreatedResponseCodec { jobId }
      _ ->
        HTTPurple.badRequest "Expected transfer operation."

  Jobs { since, until: until', include_completed }, Get -> do
    -- If neither since nor until is provided, default until to now
    until <- case since, until' of
      Nothing, Nothing -> Just <$> liftEffect nowUTC
      _, _ -> pure until'
    lift
      ( Run.Except.runExcept $ Db.selectJobs
          { includeCompleted: fromMaybe false include_completed
          , since
          , until
          }
      ) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching jobs: " <> err
        HTTPurple.internalServerError $ "Error while fetching jobs: " <> err
      Right jobs -> jsonOk (CJ.array V1.jobCodec) jobs

  Job jobId { level: maybeLogLevel, since, until: until' }, Get -> do
    -- If neither since nor until is provided, default until to now
    until <- case since, until' of
      Nothing, Nothing -> Just <$> liftEffect nowUTC
      _, _ -> pure until'
    lift (Run.Except.runExcept $ Db.selectJob { jobId, level: maybeLogLevel, since, until }) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching job: " <> err
        HTTPurple.internalServerError $ "Error while fetching job: " <> err
      Right Nothing -> do
        HTTPurple.notFound
      Right (Just job) -> jsonOk V1.jobCodec job

  PackageSets, Post -> do
    request <- HTTPurple.fromJson (jsonDecoder Operation.packageSetUpdateRequestCodec) body
    lift $ Log.info $ "Received PackageSet request: " <> request.rawPayload

    -- Check if the operation requires authentication (compiler change or package removal)
    let
      PackageSetUpdate payload = request.payload
      didChangeCompiler = isJust payload.compiler
      didRemovePackages = any isNothing payload.packages
      requiresAuth = didChangeCompiler || didRemovePackages

    -- If restricted operation, verify pacchettibotti signature
    authResult <-
      if requiresAuth then do
        pacchettiBotti <- lift API.getPacchettiBotti
        lift $ Run.liftAff $ Auth.verifyPackageSetPayload pacchettiBotti request
      else
        pure (Right unit)

    case authResult of
      Left err -> do
        lift $ Log.error $ "Package set authentication failed: " <> err
        HTTPurple.badRequest err
      Right _ -> do
        when requiresAuth do
          lift $ Log.info "Package set authentication successful."

        -- Check for duplicate pending job with the same payload
        jobId <- lift (Db.selectPackageSetJobByPayload request.payload) >>= case _ of
          Just job -> do
            lift $ Log.warn $ "Duplicate package set job insertion, returning existing one: " <> unwrap job.jobId
            pure job.jobId
          Nothing -> do
            lift $ Db.insertPackageSetJob
              { payload: request.payload
              , rawPayload: request.rawPayload
              , signature: request.signature
              }

        jsonOk V1.jobCreatedResponseCodec { jobId }

  Status, Get ->
    HTTPurple.emptyResponse Status.ok

  Status, Head ->
    HTTPurple.emptyResponse Status.ok

  _, _ ->
    HTTPurple.notFound
