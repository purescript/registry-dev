module Fetch.Retry
  ( RetryRequestError(..)
  , module ReExport
  , printRetryRequestError
  , withRetryRequest
  ) where

import Registry.App.Prelude

import Effect.Aff (Error)
import Effect.Aff as Aff
import Fetch (class ToCoreRequestOptions, HighlevelRequestOptions, Response, new)
import Fetch (Response) as ReExport
import Fetch.Internal.Request as Request
import Fetch.Internal.Response as Response
import JS.Fetch as Core
import JS.Fetch.Request as CoreRequest
import Prim.Row (class Union)
import Promise.Aff as Promise.Aff

data RetryRequestError
  = FetchError Error
  | StatusError Response

printRetryRequestError :: RetryRequestError -> String
printRetryRequestError = case _ of
  FetchError error -> "Fetch Error: " <> Aff.message error
  StatusError response -> "Status Error (" <> show response.status <> "): " <> response.statusText

withRetryRequest
  :: forall input output thruIn thruOut headers body
   . Union input thruIn (HighlevelRequestOptions headers body)
  => Union output thruOut CoreRequest.UnsafeRequestOptions
  => ToCoreRequestOptions input output
  => String
  -> { | input }
  -> Aff (RetryResult RetryRequestError Response)
withRetryRequest url opts = withRetry retry do
  (Aff.attempt $ fetch @thruIn url opts) <#> either (Left <<< FetchError) onFetchResponse
  where
  onFetchResponse :: Response -> Either RetryRequestError Response
  onFetchResponse response =
    if response.status >= 400 then Left $ StatusError response
    else Right response

  retry =
    { timeout: defaultRetry.timeout
    , retryOnCancel: defaultRetry.retryOnCancel
    , retryOnFailure: \attempt -> case _ of
        FetchError _ -> false
        StatusError { status } ->
          -- We retry on 500-level errors in case the server is temporarily
          -- unresponsive, and fail otherwise.
          if status >= 500 then
            attempt < 3
          else false
    }

-- | Copied and adapted from fetch source code to allow us to disambiguate type variables.
-- | The upstream library will soon be updated to use Visible Type Application
-- | and then we can drop this redefinition:
-- | https://github.com/rowtype-yoga/purescript-fetch/issues/9
fetch
  :: forall input output @thruIn thruOut headers body
   . Union input thruIn (HighlevelRequestOptions headers body)
  => Union output thruOut CoreRequest.UnsafeRequestOptions
  => ToCoreRequestOptions input output
  => String
  -> { | input }
  -> Aff Response
fetch url input = do
  request <- liftEffect $ new url $ Request.convert input
  coreResponse <- Promise.Aff.toAffE $ Core.fetch request
  pure $ Response.convert coreResponse
