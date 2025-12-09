-- | HTTP client for making requests to the registry server during E2E tests.
-- | This module provides typed helpers for interacting with the Registry API.
module Registry.Test.E2E.Client
  ( Config
  , ClientError(..)
  , defaultConfig
  , configFromEnv
  , getJobs
  , getJob
  , getStatus
  , publish
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
import Effect.Exception (Error, error)
import Effect.Exception as Effect.Exception
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Node.Process as Process
import Registry.API.V1 (Job, JobId(..), LogLevel)
import Registry.API.V1 as V1
import Registry.Internal.Format as Internal.Format
import Registry.Operation (PublishData)
import Registry.Operation as Operation

-- | Configuration for the E2E test client
type Config =
  { baseUrl :: String
  , timeout :: Milliseconds
  , pollInterval :: Milliseconds
  , maxPollAttempts :: Int
  }

-- | Default configuration for production use (port 8080 matches HTTPurple default)
defaultConfig :: Config
defaultConfig =
  { baseUrl: "http://localhost:8080"
  , timeout: Milliseconds 30000.0
  , pollInterval: Milliseconds 2000.0
  , maxPollAttempts: 30
  }

-- | Create config from environment, reading SERVER_PORT.
-- |
-- | SERVER_PORT is required and must be set by the test environment.
-- | See `nix/lib/env.nix` for the centralized environment configuration.
configFromEnv :: Effect Config
configFromEnv = do
  maybePort <- Process.lookupEnv "SERVER_PORT"
  case maybePort of
    Nothing -> Effect.Exception.throw "SERVER_PORT environment variable is not set. Run tests via 'nix run .#test-env' or 'nix build .#checks.x86_64-linux.integration'."
    Just port -> pure $ defaultConfig { baseUrl = "http://localhost:" <> port }

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

-- | Get the list of jobs
getJobs :: Config -> Aff (Either ClientError (Array Job))
getJobs config = get (CJ.array V1.jobCodec) config "/api/v1/jobs"

-- | Get a specific job by ID, with optional log filtering
getJob :: Config -> JobId -> Maybe LogLevel -> Maybe DateTime -> Aff (Either ClientError Job)
getJob config (JobId jobId) level since = do
  let
    params = Array.catMaybes
      [ level <#> \l -> "level=" <> V1.printLogLevel l
      , since <#> \s -> "since=" <> Formatter.DateTime.format Internal.Format.iso8601DateTime s
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
publish config publishData =
  post Operation.publishCodec V1.jobCreatedResponseCodec config "/api/v1/publish" publishData

-- | Poll a job until it completes or times out
pollJob
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => Config
  -> JobId
  -> m Job
pollJob config jobId = go 1
  where
  go attempt
    | attempt > config.maxPollAttempts =
        throwError $ toError $ Timeout $ "Job " <> unwrap jobId <> " did not complete after " <> Int.toStringAs Int.decimal config.maxPollAttempts <> " attempts"
    | otherwise = do
        liftAff $ delay config.pollInterval
        result <- liftAff $ getJob config jobId (Just V1.Debug) Nothing
        case result of
          Left err -> throwError $ toError err
          Right job ->
            case job.finishedAt of
              Just _ -> pure job
              Nothing -> go (attempt + 1)
