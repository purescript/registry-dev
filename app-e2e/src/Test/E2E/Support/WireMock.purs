-- | WireMock admin API client for verifying HTTP requests in E2E tests.
-- |
-- | This module provides helpers to query WireMock's request journal, allowing
-- | tests to assert on what HTTP requests were made to mock services.
-- |
-- | Also provides helpers for managing WireMock scenarios (stateful mocking).
-- | Scenarios allow responses to change based on state transitions - e.g., a
-- | package tarball returns 404 until it's been "uploaded" via PUT, after which
-- | it returns 200.
module Test.E2E.Support.WireMock
  ( WireMockConfig
  , WireMockRequest
  , WireMockError(..)
  , configFromEnv
  , configForService
  , configForStorage
  , getRequests
  , getRequestsOrFail
  , clearRequests
  , clearRequestsOrFail
  , resetScenarios
  , resetScenariosOrFail
  , filterByMethod
  , filterByUrlContaining
  , printWireMockError
  , formatRequests
  , failWithRequests
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error)
import Effect.Exception as Effect.Exception
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Node.Process as Process

-- | Configuration for connecting to WireMock admin API
type WireMockConfig =
  { baseUrl :: String
  }

-- | A recorded request from WireMock's journal
type WireMockRequest =
  { method :: String
  , url :: String
  , body :: Maybe String
  }

-- | Error type for WireMock operations
data WireMockError
  = HttpError { status :: Int, body :: String }
  | ParseError { msg :: String, raw :: String }

printWireMockError :: WireMockError -> String
printWireMockError = case _ of
  HttpError { status, body } -> "HTTP Error " <> Int.toStringAs Int.decimal status <> ": " <> body
  ParseError { msg, raw } -> "Parse Error: " <> msg <> "\nOriginal: " <> raw

-- | Create config from GITHUB_API_URL environment variable.
-- | Convenience for tests that need to inspect GitHub mock requests.
-- | Each WireMock instance has its own admin API on the same port.
configFromEnv :: Effect WireMockConfig
configFromEnv = configForService "GITHUB_API_URL"

-- | Create config from a specific environment variable.
-- | Useful for accessing different WireMock services (S3, Pursuit, etc.)
configForService :: String -> Effect WireMockConfig
configForService envVar = do
  maybeUrl <- Process.lookupEnv envVar
  case maybeUrl of
    Nothing -> Effect.Exception.throw $ envVar <> " environment variable is not set."
    Just baseUrl -> pure { baseUrl }

-- | Create config for the unified storage WireMock instance.
-- | The storage instance handles S3, bucket, and Pursuit APIs with stateful scenarios.
-- | Use this for scenario resets and storage-related request assertions.
configForStorage :: Effect WireMockConfig
configForStorage = configForService "S3_API_URL"

-- | Codec for a single request entry in WireMock's response
requestCodec :: CJ.Codec WireMockRequest
requestCodec = CJ.named "WireMockRequest" $ CJ.Record.object
  { method: CJ.string
  , url: CJ.string
  , body: CJ.Record.optional CJ.string
  }

-- | Codec for the nested request object in WireMock's journal response
journalEntryCodec :: CJ.Codec { request :: WireMockRequest }
journalEntryCodec = CJ.named "JournalEntry" $ CJ.Record.object
  { request: requestCodec
  }

-- | Codec for the full journal response
journalCodec :: CJ.Codec { requests :: Array { request :: WireMockRequest } }
journalCodec = CJ.named "Journal" $ CJ.Record.object
  { requests: CJ.array journalEntryCodec
  }

-- | Parse JSON response body using a codec
parseResponse :: forall a. CJ.Codec a -> String -> Either String a
parseResponse codec body = do
  json <- lmap (append "JSON parse error: ") $ JSON.parse body
  lmap CJ.DecodeError.print $ CJ.decode codec json

-- | Get all recorded requests from WireMock's journal
getRequests :: WireMockConfig -> Aff (Either WireMockError (Array WireMockRequest))
getRequests config = runExceptT do
  response <- lift $ Fetch.fetch (config.baseUrl <> "/__admin/requests") { method: GET }
  body <- lift response.text
  if response.status == 200 then
    case parseResponse journalCodec body of
      Left err -> throwError $ ParseError { msg: err, raw: body }
      Right journal -> pure $ map _.request journal.requests
  else
    throwError $ HttpError { status: response.status, body }

-- | Clear all recorded requests from WireMock's journal
clearRequests :: WireMockConfig -> Aff (Either WireMockError Unit)
clearRequests config = runExceptT do
  response <- lift $ Fetch.fetch (config.baseUrl <> "/__admin/requests") { method: DELETE }
  if response.status == 200 then
    pure unit
  else do
    body <- lift response.text
    throwError $ HttpError { status: response.status, body }

-- | Get requests, throwing on error. Useful in tests where failure should abort.
getRequestsOrFail :: WireMockConfig -> Aff (Array WireMockRequest)
getRequestsOrFail config = do
  result <- getRequests config
  case result of
    Left err ->
      throwError $ Aff.error $ "Failed to get WireMock requests: " <> printWireMockError err
    Right rs ->
      pure rs

-- | Clear requests, throwing on error. Useful in test setup.
clearRequestsOrFail :: WireMockConfig -> Aff Unit
clearRequestsOrFail config = do
  result <- clearRequests config
  case result of
    Left err ->
      Aff.throwError $ Aff.error $ "Failed to clear WireMock journal: " <> printWireMockError err
    Right _ ->
      pure unit

-- | Reset all WireMock scenarios to their initial state ("Started").
-- | This is essential for test isolation when using stateful mocking.
-- | After reset, unpublished packages return 404, published packages return 200.
resetScenarios :: WireMockConfig -> Aff (Either WireMockError Unit)
resetScenarios config = runExceptT do
  response <- lift $ Fetch.fetch (config.baseUrl <> "/__admin/scenarios/reset") { method: POST }
  if response.status == 200 then
    pure unit
  else do
    body <- lift response.text
    throwError $ HttpError { status: response.status, body }

-- | Reset scenarios, throwing on error. Useful in test setup/teardown.
resetScenariosOrFail :: WireMockConfig -> Aff Unit
resetScenariosOrFail config = do
  result <- resetScenarios config
  case result of
    Left err ->
      Aff.throwError $ Aff.error $ "Failed to reset WireMock scenarios: " <> printWireMockError err
    Right _ ->
      pure unit

-- | Filter requests by HTTP method
filterByMethod :: String -> Array WireMockRequest -> Array WireMockRequest
filterByMethod method = Array.filter (\r -> r.method == method)

-- | Filter requests by URL substring
filterByUrlContaining :: String -> Array WireMockRequest -> Array WireMockRequest
filterByUrlContaining substring = Array.filter (\r -> String.contains (String.Pattern substring) r.url)

-- | Format an array of requests for debugging output
formatRequests :: Array WireMockRequest -> String
formatRequests requests = String.joinWith "\n" $ map formatRequest requests
  where
  formatRequest r = r.method <> " " <> r.url <> case r.body of
    Nothing -> ""
    Just b -> "\n  Body: " <> b

-- | Fail a test with a message and debug info about captured requests.
failWithRequests :: forall m a. MonadThrow Error m => String -> Array WireMockRequest -> m a
failWithRequests msg requests = throwError $ Effect.Exception.error $
  msg <> "\n\nCaptured requests:\n" <> formatRequests requests
