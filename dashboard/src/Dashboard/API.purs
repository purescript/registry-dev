-- | HTTP client for making requests to the registry server from the dashboard.
-- | Provides typed helpers for fetching job data from the Registry API.
module Dashboard.API
  ( ApiConfig
  , ApiError(..)
  , defaultConfig
  , fetchJobs
  , fetchJob
  , printApiError
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Exception as Exception
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Registry.API.V1 (Job, JobId, LogLevel, Route(..), SortOrder)
import Registry.API.V1 as V1
import Routing.Duplex as Routing

-- | Configuration for the API client.
type ApiConfig =
  { baseUrl :: String
  }

-- | Default API configuration pointing to the production registry server.
defaultConfig :: ApiConfig
defaultConfig =
  { baseUrl: "https://registry.purescript.org"
  }

-- | Errors that can occur when making API requests.
data ApiError
  = HttpError { status :: Int, body :: String }
  | ParseError { message :: String, raw :: String }

-- | Render an ApiError as a human-readable string.
printApiError :: ApiError -> String
printApiError = case _ of
  HttpError { status, body } ->
    "HTTP " <> show status <> ": " <> body
  ParseError { message, raw } ->
    "Parse error: " <> message <> "\nResponse: " <> String.take 500 raw

-- | Print a V1 Route to its URL path string.
printRoute :: Route -> String
printRoute = Routing.print V1.routes

-- | Parse a JSON string using a codec, returning Either ApiError.
parseJson :: forall a. CJ.Codec a -> String -> Either ApiError a
parseJson codec str = case JSON.parse str of
  Left jsonErr ->
    Left $ ParseError { message: "JSON: " <> jsonErr, raw: str }
  Right json -> case CJ.decode codec json of
    Left decodeErr ->
      Left $ ParseError { message: CJ.DecodeError.print decodeErr, raw: str }
    Right a ->
      Right a

-- | Request timeout in milliseconds.
requestTimeout :: Milliseconds
requestTimeout = Milliseconds 10000.0

-- | Run an Aff action with a timeout. Returns Nothing if the action does not
-- | complete within the given duration, or Just the result if it does.
timeout :: forall a. Milliseconds -> Aff a -> Aff (Maybe a)
timeout ms action = sequential do
  parallel (Just <$> action) <|> parallel (Nothing <$ Aff.delay ms)

-- | Make a GET request to the given URL path and decode the response body.
get :: forall a. CJ.Codec a -> ApiConfig -> String -> Aff (Either ApiError a)
get codec config path = do
  result <- Aff.try $ timeout requestTimeout do
    response <- Fetch.fetch (config.baseUrl <> path) { method: GET }
    body <- response.text
    pure { status: response.status, body }
  case result of
    Left err ->
      pure $ Left $ HttpError { status: 0, body: Exception.message err }
    Right Nothing ->
      pure $ Left $ HttpError { status: 0, body: "Request timed out" }
    Right (Just { status, body })
      | status >= 200 && status < 300 ->
          pure $ parseJson codec body
      | otherwise ->
          pure $ Left $ HttpError { status, body }

-- | Fetch the list of jobs from the registry server.
-- |
-- | Parameters:
-- | - `since`: Only return jobs created after this time
-- | - `until`: Only return jobs created before this time
-- | - `order`: Sort order for results (ASC or DESC)
-- | - `includeCompleted`: When true, include finished jobs in the results
fetchJobs
  :: ApiConfig
  -> { since :: Maybe DateTime, until :: Maybe DateTime, order :: Maybe SortOrder, includeCompleted :: Maybe Boolean }
  -> Aff (Either ApiError (Array Job))
fetchJobs config params = do
  let route = Jobs { since: params.since, until: params.until, order: params.order, include_completed: params.includeCompleted }
  get (CJ.array V1.jobCodec) config (printRoute route)

-- | Fetch a single job by its ID.
-- |
-- | Parameters:
-- | - `level`: Minimum log level to include in the response
-- | - `since`: Only return log lines after this time
-- | - `until`: Only return log lines before this time
-- | - `order`: Sort order for log lines (ASC or DESC)
fetchJob
  :: ApiConfig
  -> JobId
  -> { level :: Maybe LogLevel, since :: Maybe DateTime, until :: Maybe DateTime, order :: Maybe SortOrder }
  -> Aff (Either ApiError Job)
fetchJob config jobId params = do
  let route = Job jobId { level: params.level, since: params.since, until: params.until, order: params.order }
  get V1.jobCodec config (printRoute route)
