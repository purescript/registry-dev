-- | HTTP client for making requests to the registry server during E2E tests.
-- | This module provides typed helpers for interacting with the Registry API.
module Test.E2E.Support.Client
  ( Config
  , ClientError(..)
  , JobFilter(..)
  , defaultConfig
  , configFromEnv
  , getJobs
  , getJobsWith
  , getJob
  , getStatus
  , publish
  , unpublish
  , transfer
  , pollJob
  , printClientError
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Effect.Exception (Error, error)
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Registry.API.V1 (Job, JobId(..), LogLevel)
import Registry.API.V1 as V1
import Registry.App.Effect.Env as Env
import Registry.Internal.Format as Internal.Format
import Registry.Operation (AuthenticatedData, PublishData)
import Registry.Operation as Operation

-- | Configuration for the E2E test client
type Config =
  { baseUrl :: String
  , pollInterval :: Milliseconds
  , maxPollAttempts :: Int
  }

-- | Default configuration for production use (port 8080 matches HTTPurple default)
defaultConfig :: Config
defaultConfig =
  { baseUrl: "http://localhost:8080"
  , pollInterval: Milliseconds 2000.0
  , maxPollAttempts: 30
  }

-- | Create config from environment, reading SERVER_PORT.
-- |
-- | SERVER_PORT is required and must be set by the test environment.
-- | See `nix/lib/env.nix` for the centralized environment configuration.
configFromEnv :: Effect Config
configFromEnv = do
  port <- Env.lookupRequired Env.serverPort
  pure $ defaultConfig { baseUrl = "http://localhost:" <> show port }

-- | Errors that can occur during client operations
data ClientError
  = HttpError { status :: Int, body :: String }
  | ParseError { msg :: String, raw :: String }
  | Timeout String
  | NetworkError String

printClientError :: ClientError -> String
printClientError = case _ of
  HttpError { status, body } -> "HTTP Error " <> Int.toStringAs Int.decimal status <> ": " <> body
  ParseError { msg, raw } -> "Parse Error: " <> msg <> "\nOriginal: " <> raw
  Timeout msg -> "Timeout: " <> msg
  NetworkError msg -> "Network Error: " <> msg

-- | Convert a ClientError to an Effect Error for throwing
toError :: ClientError -> Error
toError = error <<< printClientError

-- | Parse JSON response body using a codec
parseResponse :: forall a. CJ.Codec a -> String -> Either String a
parseResponse codec body = do
  json <- lmap (append "JSON parse error: ") $ JSON.parse body
  lmap CJ.DecodeError.print $ CJ.decode codec json

-- | Make a GET request and decode the response
get :: forall a. CJ.Codec a -> Config -> String -> Aff (Either ClientError a)
get codec config path = runExceptT do
  response <- lift $ Fetch.fetch (config.baseUrl <> path) { method: GET }
  body <- lift response.text
  if response.status >= 200 && response.status < 300 then
    case parseResponse codec body of
      Left err -> throwError $ ParseError { msg: err, raw: body }
      Right a -> pure a
  else
    throwError $ HttpError { status: response.status, body }

-- | Make a POST request with JSON body and decode the response
post :: forall req res. CJ.Codec req -> CJ.Codec res -> Config -> String -> req -> Aff (Either ClientError res)
post reqCodec resCodec config path reqBody = runExceptT do
  let jsonBody = JSON.print $ CJ.encode reqCodec reqBody
  response <- lift $ Fetch.fetch (config.baseUrl <> path)
    { method: POST
    , headers: { "Content-Type": "application/json" }
    , body: jsonBody
    }
  responseBody <- lift response.text
  if response.status >= 200 && response.status < 300 then
    case parseResponse resCodec responseBody of
      Left err -> throwError $ ParseError { msg: err, raw: responseBody }
      Right a -> pure a
  else
    throwError $ HttpError { status: response.status, body: responseBody }

data JobFilter = ActiveOnly | IncludeCompleted

-- | Get the list of jobs with a configurable filter
getJobsWith :: JobFilter -> Config -> Aff (Either ClientError (Array Job))
getJobsWith filter config =
  let
    flag = case filter of
      ActiveOnly -> "false"
      IncludeCompleted -> "true"
  in
    get (CJ.array V1.jobCodec) config ("/api/v1/jobs?include_completed=" <> flag)

-- | Get the list of jobs (includes completed jobs)
getJobs :: Config -> Aff (Either ClientError (Array Job))
getJobs = getJobsWith IncludeCompleted

-- | Get a specific job by ID, with optional log filtering
getJob :: Config -> JobId -> Maybe LogLevel -> Maybe DateTime -> Maybe Int -> Aff (Either ClientError Job)
getJob config (JobId jobId) level since limit = do
  let
    params = Array.catMaybes
      [ level <#> \l -> "level=" <> V1.printLogLevel l
      , since <#> \s -> "since=" <> Formatter.DateTime.format Internal.Format.iso8601DateTime s
      , limit <#> \n -> "limit=" <> Int.toStringAs Int.decimal n
      ]
    query = case params of
      [] -> ""
      ps -> "?" <> Array.intercalate "&" ps
  get V1.jobCodec config ("/api/v1/jobs/" <> jobId <> query)

-- | Check if the server is healthy
getStatus :: Config -> Aff (Either ClientError Unit)
getStatus config = runExceptT do
  response <- lift $ Fetch.fetch (config.baseUrl <> "/api/v1/status") { method: GET }
  if response.status == 200 then
    pure unit
  else do
    body <- lift response.text
    throwError $ HttpError { status: response.status, body }

-- | Publish a package
publish :: Config -> PublishData -> Aff (Either ClientError V1.JobCreatedResponse)
publish config = post Operation.publishCodec V1.jobCreatedResponseCodec config "/api/v1/publish"

-- | Unpublish a package (requires authentication)
unpublish :: Config -> AuthenticatedData -> Aff (Either ClientError V1.JobCreatedResponse)
unpublish config = post Operation.authenticatedCodec V1.jobCreatedResponseCodec config "/api/v1/unpublish"

-- | Transfer a package to a new location (requires authentication)
transfer :: Config -> AuthenticatedData -> Aff (Either ClientError V1.JobCreatedResponse)
transfer config = post Operation.authenticatedCodec V1.jobCreatedResponseCodec config "/api/v1/transfer"

-- | Poll a job until it completes or times out.
-- |
-- | This is the recommended way to wait for job completion in E2E tests.
-- | Do not implement custom polling loops; use this function or the higher-level
-- | helpers in Test.E2E.Support.Env (pollJobOrFail, pollJobExpectFailure).
pollJob :: forall m. MonadAff m => MonadThrow Error m => Config -> JobId -> m Job
pollJob config jobId = go 1
  where
  go attempt
    | attempt > config.maxPollAttempts =
        throwError $ toError $ Timeout $ "Job " <> unwrap jobId <> " did not complete after " <> Int.toStringAs Int.decimal config.maxPollAttempts <> " attempts"
    | otherwise = do
        liftAff $ delay config.pollInterval
        result <- liftAff $ getJob config jobId (Just V1.Debug) Nothing Nothing
        case result of
          Left err -> throwError $ toError err
          Right job ->
            case (V1.jobInfo job).finishedAt of
              Just _ -> pure job
              Nothing -> do
                when (attempt `mod` 10 == 0) do
                  liftAff $ Console.log $ "Polling job " <> unwrap jobId <> " (attempt " <> Int.toStringAs Int.decimal attempt <> ")"
                go (attempt + 1)
