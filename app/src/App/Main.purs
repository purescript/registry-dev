module Registry.App.Main where

import Registry.App.Prelude hiding ((/))

import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch.Retry as Fetch.Retry
import HTTPurple (Request, Response)
import HTTPurple as HTTPurple
import Node.Process as Process
import Registry.API.V1 (Route)
import Registry.API.V1 as V1
import Registry.App.Effect.Env as Env
import Registry.App.Server.Env (ServerEnv, createServerEnv, runEffects)
import Registry.App.Server.Router as Router

main :: Effect Unit
main =
  createServerEnv # Aff.runAff_ case _ of
    Left error -> do
      Console.log $ "Failed to start server: " <> Aff.message error
      Process.exit' 1
    Right env -> do
      -- Start healthcheck ping loop if URL is configured
      case env.vars.resourceEnv.healthchecksUrl of
        Nothing -> Console.log "HEALTHCHECKS_URL not set, healthcheck pinging disabled"
        Just healthchecksUrl -> do
          _healthcheck <- Aff.launchAff do
            let
              limit = 10
              oneMinute = Aff.Milliseconds (1000.0 * 60.0)
              fiveMinutes = Aff.Milliseconds (1000.0 * 60.0 * 5.0)

              loop n =
                Fetch.Retry.withRetryRequest healthchecksUrl {} >>= case _ of
                  Succeeded { status } | status == 200 -> do
                    Aff.delay fiveMinutes
                    loop n

                  Cancelled | n >= 0 -> do
                    Console.warn $ "Healthchecks cancelled, will retry..."
                    Aff.delay oneMinute
                    loop (n - 1)

                  Failed error | n >= 0 -> do
                    Console.warn $ "Healthchecks failed, will retry: " <> Fetch.Retry.printRetryRequestError error
                    Aff.delay oneMinute
                    loop (n - 1)

                  Succeeded { status } | status /= 200, n >= 0 -> do
                    Console.error $ "Healthchecks returned non-200 status, will retry: " <> show status
                    Aff.delay oneMinute
                    loop (n - 1)

                  Cancelled ->
                    Console.error "Healthchecks cancelled and failure limit reached, will not retry."

                  Failed error -> do
                    Console.error $ "Healthchecks failed and failure limit reached, will not retry: " <> Fetch.Retry.printRetryRequestError error

                  Succeeded _ -> do
                    Console.error $ "Healthchecks returned non-200 status and failure limit reached, will not retry."

            loop limit
          pure unit

      -- Read port from SERVER_PORT env var (optional, HTTPurple defaults to 8080)
      port <- liftEffect $ Env.lookupOptional Env.serverPort

      _close <- HTTPurple.serve
        { hostname: "0.0.0.0"
        , port
        }
        { route: V1.routes
        , router: runServer env
        }
      pure unit
  where
  runServer :: ServerEnv -> Request Route -> Aff Response
  runServer env request = do
    result <- runEffects env (Router.router env request)
    case result of
      Left error -> HTTPurple.badRequest (Aff.message error)
      Right response -> pure response
