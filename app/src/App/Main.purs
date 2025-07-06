module Registry.App.Main where

import Registry.App.Prelude hiding ((/))

import Data.DateTime (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Debug (traceM)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Fetch.Retry as Fetch.Retry
import Node.Process as Process
import Registry.App.Server.Env (ServerEnv, createServerEnv)
import Registry.App.Server.JobExecutor as JobExecutor
import Registry.App.Server.Router as Router

main :: Effect Unit
main = do
  traceM 1
  createServerEnv # Aff.runAff_ case _ of
    Left error -> do
      traceM 2
      Console.log $ "Failed to start server: " <> Aff.message error
      Process.exit' 1
    Right env -> do
      traceM 3
      Aff.launchAff_ $ healthcheck env
      Aff.launchAff_ $ jobExecutor env
      Router.runRouter env
  where
  healthcheck :: ServerEnv -> Aff Unit
  healthcheck env = loop limit
    where
    limit = 10
    oneMinute = Aff.Milliseconds (1000.0 * 60.0)
    fiveMinutes = Aff.Milliseconds (1000.0 * 60.0 * 5.0)

    loop n = do
      traceM 4
      Fetch.Retry.withRetryRequest env.vars.resourceEnv.healthchecksUrl {} >>= case _ of
        Succeeded { status } | status == 200 -> do
          traceM 5
          Aff.delay fiveMinutes
          loop n

        Cancelled | n >= 0 -> do
          traceM 6
          Console.warn $ "Healthchecks cancelled, will retry..."
          Aff.delay oneMinute
          loop (n - 1)

        Failed error | n >= 0 -> do
          traceM 7
          Console.warn $ "Healthchecks failed, will retry: " <> Fetch.Retry.printRetryRequestError error
          Aff.delay oneMinute
          loop (n - 1)

        Succeeded { status } | status /= 200, n >= 0 -> do
          traceM 8
          Console.error $ "Healthchecks returned non-200 status, will retry: " <> show status
          Aff.delay oneMinute
          loop (n - 1)

        Cancelled -> do
          traceM 9
          Console.error
            "Healthchecks cancelled and failure limit reached, will not retry."

        Failed error -> do
          traceM 10
          Console.error $ "Healthchecks failed and failure limit reached, will not retry: " <> Fetch.Retry.printRetryRequestError error

        Succeeded _ -> do
          traceM 11
          Console.error "Healthchecks returned non-200 status and failure limit reached, will not retry."

  jobExecutor :: ServerEnv -> Aff Unit
  jobExecutor env = do
    traceM 12
    loop initialRestartDelay
    where
    initialRestartDelay = Milliseconds 100.0

    loop restartDelay = do
      traceM 13
      start <- nowUTC
      result <- JobExecutor.runJobExecutor env
      end <- nowUTC

      traceM 14
      Console.error case result of
        Left error -> "Job executor failed: " <> Aff.message error
        Right _ -> "Job executor exited for no reason."

      -- This is a heuristic: if the executor keeps crashing immediately, we
      -- restart with an exponentially increasing delay, but once the executor
      -- had a run longer than a minute, we start over with a small delay.
      let
        nextRestartDelay
          | end `diff` start > Seconds 60.0 = initialRestartDelay
          | otherwise = restartDelay <> restartDelay

      Aff.delay nextRestartDelay
      loop nextRestartDelay
