-- | HTTP client for making requests to the registry server during E2E tests.
-- | This module provides typed helpers for interacting with the Registry API.
-- |
-- | All client functions operate in the E2E monad (ReaderT TestEnv Aff) and
-- | throw on HTTP or parse errors. Use the `try*` variants (e.g., `tryGetJob`)
-- | when testing error responses - they return `Either ClientError a` with
-- | typed HTTP status codes.
module Test.E2E.Support.Client
  ( ClientError(..)
  , JobFilter(..)
  , getJobs
  , getJobsWith
  , getJob
  , tryGetJob
  , getStatus
  , publish
  , unpublish
  , transfer
  , packageSets
  , tryPackageSets
  , pollJob
  , printClientError
  , clientErrorStatus
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Reader (ask)
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.Int as Int
import Effect.Aff (delay)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Exception
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Registry.API.V1 (Job, JobId, LogLevel, Route(..))
import Registry.API.V1 as V1
import Registry.Operation (AuthenticatedData, PackageSetUpdateRequest, PublishData)
import Registry.Operation as Operation
import Routing.Duplex as Routing
import Test.E2E.Support.Types (E2E)

-- | Errors that can occur during client operations
data ClientError
  = HttpError { status :: Int, body :: String }
  | ParseError { msg :: String, raw :: String }
  | Timeout String

printClientError :: ClientError -> String
printClientError = case _ of
  HttpError { status, body } -> "HTTP Error " <> Int.toStringAs Int.decimal status <> ": " <> body
  ParseError { msg, raw } -> "Parse Error: " <> msg <> "\nOriginal: " <> raw
  Timeout msg -> "Timeout: " <> msg

-- | Extract the HTTP status code from a ClientError, if it's an HttpError
clientErrorStatus :: ClientError -> Maybe Int
clientErrorStatus = case _ of
  HttpError { status } -> Just status
  _ -> Nothing

-- | Convert a ClientError to an Effect Error for throwing
toError :: ClientError -> Error
toError = Exception.error <<< printClientError

-- | Throw a ClientError as an Aff error
throw :: forall a. ClientError -> Aff a
throw = Aff.throwError <<< toError

-- | Print a Route to its URL path using the route codec
printRoute :: Route -> String
printRoute = Routing.print V1.routes

-- | Make a GET request and decode the response, returning Either on error.
tryGet :: forall a. CJ.Codec a -> String -> String -> Aff (Either ClientError a)
tryGet codec baseUrl path = do
  response <- Fetch.fetch (baseUrl <> path) { method: GET }
  body <- response.text
  if response.status >= 200 && response.status < 300 then
    case parseJson codec body of
      Left err -> pure $ Left $ ParseError { msg: CJ.DecodeError.print err, raw: body }
      Right a -> pure $ Right a
  else
    pure $ Left $ HttpError { status: response.status, body }

-- | Make a GET request and decode the response. Throws on error.
get :: forall a. CJ.Codec a -> String -> String -> Aff a
get codec baseUrl path = tryGet codec baseUrl path >>= either throw pure

-- | Make a POST request with JSON body, returning Either on error.
tryPost :: forall req res. CJ.Codec req -> CJ.Codec res -> String -> String -> req -> Aff (Either ClientError res)
tryPost reqCodec resCodec baseUrl path reqBody = do
  let jsonBody = JSON.print $ CJ.encode reqCodec reqBody
  response <- Fetch.fetch (baseUrl <> path)
    { method: POST
    , headers: { "Content-Type": "application/json" }
    , body: jsonBody
    }
  responseBody <- response.text
  if response.status >= 200 && response.status < 300 then
    case parseJson resCodec responseBody of
      Left err -> pure $ Left $ ParseError { msg: CJ.DecodeError.print err, raw: responseBody }
      Right a -> pure $ Right a
  else
    pure $ Left $ HttpError { status: response.status, body: responseBody }

-- | Make a POST request with JSON body and decode the response. Throws on error.
post :: forall req res. CJ.Codec req -> CJ.Codec res -> String -> String -> req -> Aff res
post reqCodec resCodec baseUrl path reqBody = tryPost reqCodec resCodec baseUrl path reqBody >>= either throw pure

data JobFilter = ActiveOnly | IncludeCompleted

-- | Get the list of jobs with a configurable filter
getJobsWith :: JobFilter -> E2E (Array Job)
getJobsWith filter = do
  { clientConfig } <- ask
  let
    includeCompleted = case filter of
      ActiveOnly -> Just false
      IncludeCompleted -> Just true
    route = Jobs { since: Nothing, include_completed: includeCompleted }
  liftAff $ get (CJ.array V1.jobCodec) clientConfig.baseUrl (printRoute route)

-- | Get the list of jobs (includes completed jobs)
getJobs :: E2E (Array Job)
getJobs = getJobsWith IncludeCompleted

-- | Get a specific job by ID, with optional log filtering
getJob :: JobId -> Maybe LogLevel -> Maybe DateTime -> E2E Job
getJob jobId level since = do
  { clientConfig } <- ask
  let route = Job jobId { level, since }
  liftAff $ get V1.jobCodec clientConfig.baseUrl (printRoute route)

-- | Try to get a specific job by ID, returning Left on HTTP/parse errors.
-- | Use this when testing error responses (e.g., expecting 404).
tryGetJob :: JobId -> Maybe LogLevel -> Maybe DateTime -> E2E (Either ClientError Job)
tryGetJob jobId level since = do
  { clientConfig } <- ask
  let route = Job jobId { level, since }
  liftAff $ tryGet V1.jobCodec clientConfig.baseUrl (printRoute route)

-- | Check if the server is healthy
getStatus :: E2E Unit
getStatus = do
  { clientConfig } <- ask
  liftAff do
    response <- Fetch.fetch (clientConfig.baseUrl <> printRoute Status) { method: GET }
    if response.status == 200 then
      pure unit
    else do
      body <- response.text
      throw $ HttpError { status: response.status, body }

-- | Publish a package
publish :: PublishData -> E2E V1.JobCreatedResponse
publish reqBody = do
  { clientConfig } <- ask
  liftAff $ post Operation.publishCodec V1.jobCreatedResponseCodec clientConfig.baseUrl (printRoute Publish) reqBody

-- | Unpublish a package (requires authentication)
unpublish :: AuthenticatedData -> E2E V1.JobCreatedResponse
unpublish authData = do
  { clientConfig } <- ask
  liftAff $ post Operation.authenticatedCodec V1.jobCreatedResponseCodec clientConfig.baseUrl (printRoute Unpublish) authData

-- | Transfer a package to a new location (requires authentication)
transfer :: AuthenticatedData -> E2E V1.JobCreatedResponse
transfer authData = do
  { clientConfig } <- ask
  liftAff $ post Operation.authenticatedCodec V1.jobCreatedResponseCodec clientConfig.baseUrl (printRoute Transfer) authData

-- | Submit a package set update request
packageSets :: PackageSetUpdateRequest -> E2E V1.JobCreatedResponse
packageSets request = do
  { clientConfig } <- ask
  liftAff $ post Operation.packageSetUpdateRequestCodec V1.jobCreatedResponseCodec clientConfig.baseUrl (printRoute PackageSets) request

-- | Try to submit a package set update, returning Left on HTTP/parse errors.
-- | Use this when testing error responses (e.g., expecting 400 for unauthorized restricted ops).
tryPackageSets :: PackageSetUpdateRequest -> E2E (Either ClientError V1.JobCreatedResponse)
tryPackageSets request = do
  { clientConfig } <- ask
  liftAff $ tryPost Operation.packageSetUpdateRequestCodec V1.jobCreatedResponseCodec clientConfig.baseUrl (printRoute PackageSets) request

-- | Poll a job until it completes or times out.
-- |
-- | This is the recommended way to wait for job completion in E2E tests.
-- | Do not implement custom polling loops; use this function or the higher-level
-- | helpers in Test.E2E.Support.Env (pollJobOrFail, pollJobExpectFailure).
pollJob :: JobId -> E2E Job
pollJob jobId = do
  { clientConfig } <- ask
  go clientConfig 1
  where
  go config attempt
    | attempt > config.maxPollAttempts =
        liftAff $ throw $ Timeout $ "Job " <> unwrap jobId <> " did not complete after " <> Int.toStringAs Int.decimal config.maxPollAttempts <> " attempts"
    | otherwise = do
        liftAff $ delay config.pollInterval
        job <- getJob jobId (Just V1.Debug) Nothing
        case (V1.jobInfo job).finishedAt of
          Just _ -> pure job
          Nothing -> do
            when (attempt `mod` 10 == 0) do
              Console.log $ "Polling job " <> unwrap jobId <> " (attempt " <> Int.toStringAs Int.decimal attempt <> ")"
            go config (attempt + 1)
