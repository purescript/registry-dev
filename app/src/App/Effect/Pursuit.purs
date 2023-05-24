-- | An effect for the Pursuit API.
module Registry.App.Effect.Pursuit where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.HTTP.Method as Method
import Data.MediaType.Common as MediaType
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Foreign.Octokit (GitHubToken(..))
import Run (AFF, Run)
import Run as Run

-- | An effect for interacting with Pursuit
data Pursuit a = Publish Json (Either String Unit -> a)

derive instance Functor Pursuit

type PURSUIT r = (pursuit :: Pursuit | r)

_pursuit :: Proxy "pursuit"
_pursuit = Proxy

-- | Publish a package to Pursuit using the JSON output of the compiler.
publish :: forall r. Json -> Run (PURSUIT + r) (Either String Unit)
publish json = Run.lift _pursuit (Publish json identity)

-- | Run the PURSUIT effect given a handler.
interpret :: forall r a. (Pursuit ~> Run r) -> Run (PURSUIT + r) a -> Run r a
interpret handler = Run.interpret (Run.on _pursuit handler Run.send)

-- | Handle Pursuit by skipping all calls.
handlePure :: forall r a. Pursuit a -> Run r a
handlePure = case _ of
  Publish _ reply -> pure $ reply $ Right unit

-- | Handle Pursuit by executing HTTP requests using the provided auth token.
handleAff :: forall r a. GitHubToken -> Pursuit a -> Run (LOG + AFF + r) a
handleAff (GitHubToken token) = case _ of
  Publish payload reply -> do
    Log.debug "Pushing to Pursuit..."

    let
      loop n = do
        result <- Run.liftAff $ withBackoff' $ Affjax.Node.request
          { content: Just $ RequestBody.json payload
          , headers:
              [ RequestHeader.Accept MediaType.applicationJSON
              , RequestHeader.RequestHeader "Authorization" ("token " <> token)
              ]
          , method: Left Method.POST
          , username: Nothing
          , withCredentials: false
          , password: Nothing
          , responseFormat: ResponseFormat.string
          , timeout: Nothing
          , url: "https://pursuit.purescript.org/packages"
          }

        case result of
          Nothing -> do
            Log.error $ "Pursuit failed to connect after several retries."
            pure $ Left $ "Expected to receive a 201 status from Pursuit, but failed to connect after several retries."
          Just (Right { status: StatusCode status }) | status == 201 -> do
            Log.debug "Received 201 status, which indicates the upload was successful."
            pure $ Right unit
          Just (Right { body, status: StatusCode status }) | status == 502 -> do
            if n == 0 then do
              Log.error $ "Pursuit publishing failed with status " <> show status <> " and body\n" <> body
              pure $ Left $ "Expected to receive a 201 status from Pursuit, but received " <> show status <> " instead."
            else do
              Log.debug $ "Received 502 status, retrying..."
              Run.liftAff $ Aff.delay $ Milliseconds 1000.0
              loop (n - 1)
          Just (Right { body, status: StatusCode status }) -> do
            Log.error $ "Pursuit publishing failed with status " <> show status <> " and body\n" <> body
            pure $ Left $ "Expected to receive a 201 status from Pursuit, but received " <> show status <> " instead."
          Just (Left httpError) -> do
            let printedError = Affjax.Node.printError httpError
            Log.error $ "Pursuit publishing failed because of an HTTP error: " <> printedError
            pure $ Left "Could not reach Pursuit due to an HTTP error."

    reply <$> loop 2
