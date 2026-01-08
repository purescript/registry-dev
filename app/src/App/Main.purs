module Registry.App.Main where

import Registry.App.Prelude hiding ((/))

import Data.DateTime (diff)
import Data.Time.Duration (Minutes(..), Seconds(..), fromDuration)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch.Retry as Fetch.Retry
import Node.Process as Process
import Registry.App.Server.Env (createServerEnv)
import Registry.App.Server.JobExecutor as JobExecutor
import Registry.App.Server.Router as Router
import Registry.App.Server.Scheduler as CronJobs

main :: Effect Unit
main = do
  createServerEnv # Aff.runAff_ case _ of
    Left error -> do
      Console.log $ "Failed to start server: " <> Aff.message error
      Process.exit' 1
    Right env -> do
      case env.vars.resourceEnv.healthchecksUrl of
        Nothing -> Console.log "HEALTHCHECKS_URL not set, healthcheck pinging disabled"
        Just healthchecksUrl -> Aff.launchAff_ $ healthcheck healthchecksUrl
      Aff.launchAff_ $ withRestartLoop "Scheduler" (CronJobs.runScheduler env)
      Aff.launchAff_ $ withRestartLoop "Job executor" (JobExecutor.runJobExecutor env)
      Router.runRouter env
  where
  healthcheck :: String -> Aff Unit
  healthcheck healthchecksUrl = loop limit
    where
    limit = 10
    oneMinute = fromDuration (Minutes 1.0)
    fiveMinutes = fromDuration (Minutes 5.0)

    loop n = do
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

        Cancelled -> do
          Console.error
            "Healthchecks cancelled and failure limit reached, will not retry."

        Failed error -> do
          Console.error $ "Healthchecks failed and failure limit reached, will not retry: " <> Fetch.Retry.printRetryRequestError error

        Succeeded _ -> do
          Console.error "Healthchecks returned non-200 status and failure limit reached, will not retry."

  -- | Run an Aff action in an infinite loop with exponential backoff on failure.
  -- | If the action keeps crashing immediately, restart delay doubles each time.
  -- | Once the action runs for more than a minute, the delay resets.
  withRestartLoop :: String -> Aff (Either Aff.Error Unit) -> Aff Unit
  withRestartLoop name action = loop initialRestartDelay
    where
    initialRestartDelay = fromDuration (Seconds 0.1)

    loop restartDelay = do
      start <- nowUTC
      result <- action
      end <- nowUTC

      Console.error case result of
        Left error -> name <> " failed: " <> Aff.message error
        Right _ -> name <> " exited for no reason."

      -- This is a heuristic: if the fiber keeps crashing immediately, we
      -- restart with an exponentially increasing delay, but once the executor
      -- had a run longer than a minute, we start over with a small delay.
      let
        nextRestartDelay
          | end `diff` start > Seconds 60.0 = initialRestartDelay
          | otherwise = restartDelay <> restartDelay

      Aff.delay nextRestartDelay
      loop nextRestartDelay
