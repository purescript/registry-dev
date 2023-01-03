-- | An effect for the Pursuit API.
module Registry.App.Monad.Pursuit where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Reader (class MonadAsk, asks)
import Data.HTTP.Method as Method
import Data.MediaType.Common as MediaType
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
import Registry.Foreign.Octokit (GitHubToken(..))

class Monad m <= MonadPursuit m where
  publish :: Json -> m (Either String Unit)

instance MonadPursuit m => MonadPursuit (ExceptT e m) where
  publish = lift <<< publish

handlePursuitPure :: forall m. Monad m => Json -> m (Either String Unit)
handlePursuitPure _ = pure $ Right unit

type PursuitEnv =
  { token :: GitHubToken
  }

type PURSUIT r = (pursuit :: PursuitEnv | r)

-- | Handle Pursuit by executing HTTP requests using the provided auth token.
handlePursuitHttp
  :: forall m r
   . MonadAsk { | PURSUIT + r } m
  => MonadLog m
  => MonadAff m
  => Json
  -> m (Either String Unit)
handlePursuitHttp payload = do
  env <- asks _.pursuit
  Log.debug "Pushing to Pursuit..."
  result <- liftAff $ Affjax.Node.request
    { content: Just $ RequestBody.json payload
    , headers:
        [ RequestHeader.Accept MediaType.applicationJSON
        , RequestHeader.RequestHeader "Authorization" ("token " <> un GitHubToken env.token)
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
    Right { status } | status == StatusCode 201 -> do
      Log.debug "Received 201 status, which indicates the upload was successful."
      pure $ Right unit
    Right { body, status: StatusCode status } -> do
      Log.error $ "Pursuit publishing failed with status " <> show status <> " and body\n" <> body
      pure $ Left $ "Expected to receive a 201 status from Pursuit, but received " <> show status <> " instead."
    Left httpError -> do
      let printedError = Affjax.Node.printError httpError
      Log.error $ "Pursuit publishing failed because of an HTTP error: " <> printedError
      pure $ Left "Could not reach Pursuit due to an HTTP error."
