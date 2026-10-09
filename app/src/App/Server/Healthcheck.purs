module Registry.App.Server.Healthcheck (run, report) where

import Registry.App.Prelude

import Control.Parallel as Parallel
import Data.DateTime (diff)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Fetch as Fetch
import Registry.App.Effect.Db as Db
import Registry.App.Server.Env (ServerEnv, runEffects)
import Registry.App.Server.Env as Env

-- | Give startup one minute, then report on a five-minute cadence. Failures to
-- | deliver a ping never stop the reporter; the next scheduled ping retries.
run :: ServerEnv -> String -> Aff Unit
run env url = Aff.delay (Milliseconds 60_000.0) *> loop
  where
  interval = Milliseconds 300_000.0
  loop = do
    start <- nowUTC
    database <- runEffects env $ void Db.selectNextPublishJob
    executor <- liftEffect $ Ref.read env.executorStatus
    let
      health = case database of
        Left _ -> Left "Database query failed; see server logs"
        Right _ -> case executor of
          Env.Initializing -> Left "Executor initializing"
          Env.Operational -> Right unit
          Env.Paused -> Left "Executor paused after repeated job resets"
          Env.Restarting -> Left "Executor restarting after unexpected exit; see server logs"
    result <- Aff.attempt $ report url health
    for_ (either Just (const Nothing) result) \_ ->
      Console.warn "Healthcheck report failed; retrying next scheduled report"
    end <- nowUTC
    let Milliseconds elapsed = end `diff` start
    Aff.delay $ Milliseconds $ max 0.0 (unwrap interval - elapsed)
    loop

-- | Bound requests and use abortable fetch so an unreachable monitoring service
-- | cannot hold up subsequent reports. Only send sanitized operational details.
report :: String -> Either String Unit -> Aff Unit
report url health = do
  result <- Parallel.sequential $ Parallel.parallel (Aff.attempt send) <|> Parallel.parallel timeout
  either Aff.throwError pure result
  where
  send = do
    let
      target = either (const $ url <> "/fail") (const url) health
      body = either identity (const "Database reachable; executor operational (idle or processing jobs)") health
    response <- Fetch.fetch target { method: Fetch.POST, body }
    unless (response.status == 200) $ Aff.throwError $ Aff.error "Healthchecks returned non-200"
    text <- response.text
    unless (text == "OK") $ Aff.throwError $ Aff.error "Healthchecks did not accept the report"
  timeout = do
    Aff.delay $ Milliseconds 10_000.0
    pure $ Left $ Aff.error "Healthcheck request timed out"
