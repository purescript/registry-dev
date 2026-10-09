module Registry.App.Main where

import Registry.App.Prelude hiding ((/))

import Data.DateTime (diff)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.EventEmitter as EventEmitter
import Node.Process as Process
import Registry.App.Server.Env (createServerEnv)
import Registry.App.Server.Env as Env
import Registry.App.Server.Healthcheck as Healthcheck
import Registry.App.Server.JobExecutor as JobExecutor
import Registry.App.Server.Router as Router

main :: Effect Unit
main = createServerEnv # Aff.runAff_ case _ of
  Left error -> liftEffect do
    Console.log $ "Failed to start server: " <> Aff.message error
    Process.exit' 1
  Right env -> do
    when env.vars.readOnly do
      Console.log "READONLY mode enabled: git push, S3 upload, and Pursuit publish are disabled."
    healthcheck <- case env.vars.resourceEnv.healthchecksUrl of
      Nothing -> do
        Console.log "HEALTHCHECKS_URL not set, healthcheck pinging disabled"
        pure Nothing
      Just healthchecksUrl -> Just <$> Aff.launchAff (Healthcheck.run env healthchecksUrl)
    executor <- Aff.launchAff $ withRetryLoop "Job executor" do
      result <- JobExecutor.runJobExecutor env
      liftEffect $ Ref.write Env.Restarting env.executorStatus
      pure result
    close <- Router.runRouter env
    shuttingDown <- Ref.new false
    let
      shutdown = do
        alreadyStopping <- Ref.read shuttingDown
        unless alreadyStopping do
          Ref.write true shuttingDown
          close $ Console.log "Shutting down registry server"
          Aff.launchAff_ do
            let reason = Aff.error "Registry server shutting down"
            for_ healthcheck $ Aff.killFiber reason
            Aff.killFiber reason executor
    Process.process # EventEmitter.on_ (Process.mkSignalH' "SIGTERM") shutdown
    Process.process # EventEmitter.on_ (Process.mkSignalH' "SIGINT") shutdown
  where
  -- | Run an Aff action in a loop with exponential backoff on failure.
  -- | If the action runs for longer than 60 seconds before failing,
  -- | the restart delay resets to the initial value (heuristic for stability).
  withRetryLoop :: String -> Aff (Either Aff.Error Unit) -> Aff Unit
  withRetryLoop name action = loop initialRestartDelay
    where
    initialRestartDelay = Milliseconds 100.0

    loop restartDelay = do
      start <- nowUTC
      result <- action
      end <- nowUTC

      Console.error case result of
        Left error -> name <> " failed: " <> Aff.message error
        Right _ -> name <> " exited for no reason."

      -- This is a heuristic: if the executor keeps crashing immediately, we
      -- restart with an exponentially increasing delay, but once the executor
      -- had a run longer than a minute, we start over with a small delay.
      let
        nextRestartDelay
          | end `diff` start > Seconds 60.0 = initialRestartDelay
          | otherwise = restartDelay <> restartDelay

      Aff.delay nextRestartDelay
      loop nextRestartDelay
