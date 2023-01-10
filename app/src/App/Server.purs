module Registry.App.Server where

import Registry.App.Prelude hiding ((/))

import Data.Codec.Argonaut as CA
import Data.Formatter.DateTime as Formatter.DateTime
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import HTTPure (Method(..))
import HTTPure as HTTPure
import Node.Path as Path
import Node.Process as Process
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.Effect.Cache (CacheRef)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Git (Debouncer, GIT)
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LogVerbosity(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify (NOTIFY)
import Registry.App.Effect.Notify as Notify
import Registry.App.Effect.Pursuit (PURSUIT)
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Manifest (LEGACY_CACHE, _legacyCache)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (GitHubToken, Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Operation as Operation
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

main :: Effect Unit
main = do
  createServerEnv # Aff.runAff_ case _ of
    Left error -> do
      Console.log $ "Failed to start server: " <> Aff.message error
      Process.exit 1
    Right env -> do
      _close <- HTTPure.serve 8080 (runServer env router) do
        Console.log $ String.joinWith "\n"
          [ " ┌───────────────────────────────────────────┐"
          , " │ Server now up on port 8080                │"
          , " │                                           │"
          , " │ To test, run:                             │"
          , " │  > curl -v localhost:8080/api/v1/publish  │"
          , " └───────────────────────────────────────────┘"
          ]
      pure unit

type ServerEnvVars =
  { token :: GitHubToken
  , publicKey :: String
  , privateKey :: String
  , spacesKey :: String
  , spacesSecret :: String
  }

readServerEnvVars :: Aff ServerEnvVars
readServerEnvVars = do
  Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret
  pure { token, publicKey, privateKey, spacesKey, spacesSecret }

type ServerEnv =
  { cacheDir :: FilePath
  , logsDir :: FilePath
  , githubCacheRef :: CacheRef
  , legacyCacheRef :: CacheRef
  , registryCacheRef :: CacheRef
  , octokit :: Octokit
  , vars :: ServerEnvVars
  , debouncer :: Debouncer
  }

createServerEnv :: Aff ServerEnv
createServerEnv = do
  vars <- readServerEnvVars

  let cacheDir = Path.concat [ scratchDir, ".cache" ]
  let logsDir = Path.concat [ scratchDir, "logs" ]
  for_ [ cacheDir, logsDir ] FS.Extra.ensureDirectory

  githubCacheRef <- Cache.newCacheRef
  legacyCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  octokit <- Octokit.newOctokit vars.token
  debouncer <- Git.newDebouncer

  pure { debouncer, githubCacheRef, legacyCacheRef, registryCacheRef, cacheDir, logsDir, vars, octokit }

type ServerEffects = (PACCHETTIBOTTI_ENV + REGISTRY + GITHUB + GIT + STORAGE + PURSUIT + LEGACY_CACHE + NOTIFY + LOG + EXCEPT String + AFF + EFFECT ())

runServer :: ServerEnv -> (HTTPure.Request -> Run ServerEffects HTTPure.Response) -> HTTPure.Request -> Aff HTTPure.Response
runServer env router' request = do
  now <- nowUTC
  let logFile = String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> "-" <> String.joinWith "__" request.path <> ".log"
  let logPath = Path.concat [ env.logsDir, logFile ]

  result <- Aff.attempt do
    router' request
      # Env.runPacchettiBottiEnv { publicKey: env.vars.publicKey, privateKey: env.vars.privateKey }
      # Registry.interpret (Registry.handle env.registryCacheRef)
      # Git.interpret
          ( Git.handle
              { repos: Git.defaultRepos
              , pull: Git.ForceClean
              , write: Git.CommitAs (Git.pacchettibottiCommitter env.vars.token)
              , workdir: scratchDir
              , debouncer: env.debouncer
              }
          )
      # Pursuit.interpret (Pursuit.handleAff env.vars.token)
      # GitHub.interpret (GitHub.handle { octokit: env.octokit, cache: env.cacheDir, ref: env.githubCacheRef })
      # Storage.interpret (Storage.handleS3 { s3: { key: env.vars.spacesKey, secret: env.vars.spacesSecret }, cache: env.cacheDir })
      # Cache.interpret _legacyCache (Cache.handleMemoryFs { cache: env.cacheDir, ref: env.legacyCacheRef })
      # Notify.interpret Notify.handleLog
      # Except.catch (\msg -> Log.error msg *> Run.liftAff (Aff.throwError (Aff.error msg)))
      # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
      # Run.runBaseAff'

  case result of
    Left error -> HTTPure.badRequest (Aff.message error)
    Right response -> pure response

router :: HTTPure.Request -> Run ServerEffects HTTPure.Response
router match = case match.path of
  [ "api", "v1", "publish" ] | match.method == Post -> do
    body <- Run.liftAff $ HTTPure.toString match.body
    case parseJson Operation.publishCodec body of
      Left error -> HTTPure.badRequest $ CA.printJsonDecodeError error
      Right publish -> do
        -- TODO: This should really be a launchAff_ acknowledging receipt but
        -- not actualy processing, once we validate the operation is OK, and we
        -- can return the job ID for polling.
        API.publish Current publish
        HTTPure.ok "Completed publish operation."

  [ "api", "v1", "unpublish" ] | match.method == Post -> do
    body <- Run.liftAff $ HTTPure.toString match.body
    case parseJson Operation.authenticatedCodec body of
      Left error -> HTTPure.badRequest $ CA.printJsonDecodeError error
      Right auth -> case auth.payload of
        Operation.Unpublish _ -> do
          API.authenticated auth
          HTTPure.ok "Completed unpublish operation."
        _ ->
          HTTPure.badRequest "Expected unpublish operation."

  [ "api", "v1", "transfer" ] | match.method == Post -> do
    body <- Run.liftAff $ HTTPure.toString match.body
    case parseJson Operation.authenticatedCodec body of
      Left error -> HTTPure.badRequest $ CA.printJsonDecodeError error
      Right auth -> case auth.payload of
        Operation.Transfer _ -> do
          API.authenticated auth
          HTTPure.ok "Completed transfer operation."
        _ ->
          HTTPure.badRequest "Expected transfer operation."

  _ -> HTTPure.notFound
