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
  ( WireMockRequest
  , WireMockError(..)
  , getGithubRequests
  , getStorageRequests
  , clearGithubRequests
  , clearStorageRequests
  , resetStorageScenarios
  , filterByMethod
  , filterByUrlContaining
  , printWireMockError
  , formatRequests
  , failWithRequests
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Int as Int
import Data.String as String
import Effect.Aff as Aff
import Effect.Exception (Error)
import Effect.Exception as Effect.Exception
import Fetch (Method(..))
import Fetch as Fetch
import JSON as JSON
import Test.E2E.Support.Types (E2E)

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

-- | Get all recorded requests from a WireMock instance
getRequestsFrom :: String -> Aff (Either WireMockError (Array WireMockRequest))
getRequestsFrom baseUrl = runExceptT do
  response <- lift $ Fetch.fetch (baseUrl <> "/__admin/requests") { method: GET }
  body <- lift response.text
  if response.status == 200 then
    case parseResponse journalCodec body of
      Left err -> throwError $ ParseError { msg: err, raw: body }
      Right journal -> pure $ map _.request journal.requests
  else
    throwError $ HttpError { status: response.status, body }

-- | Clear all recorded requests from a WireMock instance
clearRequestsFrom :: String -> Aff (Either WireMockError Unit)
clearRequestsFrom baseUrl = runExceptT do
  response <- lift $ Fetch.fetch (baseUrl <> "/__admin/requests") { method: DELETE }
  if response.status == 200 then
    pure unit
  else do
    body <- lift response.text
    throwError $ HttpError { status: response.status, body }

-- | Reset all scenarios to initial state on a WireMock instance
resetScenariosOn :: String -> Aff (Either WireMockError Unit)
resetScenariosOn baseUrl = runExceptT do
  response <- lift $ Fetch.fetch (baseUrl <> "/__admin/scenarios/reset") { method: POST }
  if response.status == 200 then
    pure unit
  else do
    body <- lift response.text
    throwError $ HttpError { status: response.status, body }

-- | Helper to run a WireMock operation and throw on error
orFail :: forall a. String -> Either WireMockError a -> Aff a
orFail context = case _ of
  Left err -> Aff.throwError $ Aff.error $ context <> ": " <> printWireMockError err
  Right a -> pure a

-- | Get captured requests from the GitHub WireMock.
getGithubRequests :: E2E (Array WireMockRequest)
getGithubRequests = do
  { githubWireMock } <- ask
  liftAff $ getRequestsFrom githubWireMock.baseUrl >>= orFail "Failed to get GitHub WireMock requests"

-- | Get captured requests from the storage WireMock (S3, Pursuit).
getStorageRequests :: E2E (Array WireMockRequest)
getStorageRequests = do
  { storageWireMock } <- ask
  liftAff $ getRequestsFrom storageWireMock.baseUrl >>= orFail "Failed to get storage WireMock requests"

-- | Clear the GitHub WireMock request journal.
clearGithubRequests :: E2E Unit
clearGithubRequests = do
  { githubWireMock } <- ask
  liftAff $ clearRequestsFrom githubWireMock.baseUrl >>= orFail "Failed to clear GitHub WireMock requests"

-- | Clear the storage WireMock request journal.
clearStorageRequests :: E2E Unit
clearStorageRequests = do
  { storageWireMock } <- ask
  liftAff $ clearRequestsFrom storageWireMock.baseUrl >>= orFail "Failed to clear storage WireMock requests"

-- | Reset all storage WireMock scenarios to their initial state.
resetStorageScenarios :: E2E Unit
resetStorageScenarios = do
  { storageWireMock } <- ask
  liftAff $ resetScenariosOn storageWireMock.baseUrl >>= orFail "Failed to reset storage WireMock scenarios"

-- | Filter requests by HTTP method
filterByMethod :: String -> Array WireMockRequest -> Array WireMockRequest
filterByMethod method = Array.filter (\r -> r.method == method)

-- | Filter requests by URL substring
filterByUrlContaining :: String -> Array WireMockRequest -> Array WireMockRequest
filterByUrlContaining substring = Array.filter (\r -> String.contains (String.Pattern substring) r.url)

-- | Format an array of requests for debugging output
formatRequests :: Array WireMockRequest -> String
formatRequests = String.joinWith "\n" <<< map formatRequest
  where
  formatRequest req = req.method <> " " <> req.url <> case req.body of
    Nothing -> ""
    Just body -> "\n  Body: " <> body

-- | Fail a test with a message and debug info about captured requests.
failWithRequests :: forall m a. MonadThrow Error m => String -> Array WireMockRequest -> m a
failWithRequests msg requests = throwError $ Effect.Exception.error $ String.joinWith "\n" [ msg, "\nCaptured requests:", formatRequests requests ]
