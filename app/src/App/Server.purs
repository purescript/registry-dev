module Registry.App.Server where

import Registry.App.Prelude hiding ((/))

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (unwrap)
import Data.String as String
import Data.UUID.Random as UUID
import Effect.Aff as Aff
import Effect.Class.Console as Console
import HTTPurple (class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response, RouteDuplex', (/), (?))
import HTTPurple as HTTPurple
import Node.Path as Path
import Node.Process as Process
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.Effect.Cache (CacheRef)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Db (Db, JobId(..))
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Env (PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Git (Debouncer, GIT)
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
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
import Routing.Duplex as Routing
import Routing.Duplex.Generic as RoutingG
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

data Route
  = Publish
  | Unpublish
  | Transfer
  | Jobs JobId
      { logLevel :: Maybe LogLevel
      , since :: Maybe DateTime
      }

derive instance Generic Route _

jobIdS :: RouteDuplex' JobId
jobIdS = _Newtype Routing.segment

logLevelP :: RouteDuplex' String -> RouteDuplex' LogLevel
logLevelP = Routing.as printLogLevel parseLogLevel

timestampP :: RouteDuplex' String -> RouteDuplex' DateTime
timestampP = Routing.as printTimestamp parseTimestamp
  where
  printTimestamp t = DateTime.format Internal.Format.iso8601DateTime t
  parseTimestamp s = DateTime.unformat Internal.Format.iso8601DateTime s

routes :: RouteDuplex' Route
routes = Routing.root $ Routing.prefix "api" $ Routing.prefix "v1" $ RoutingG.sum
  { "Publish": "publish" / RoutingG.noArgs
  , "Unpublish": "unpublish" / RoutingG.noArgs
  , "Transfer": "transfer" / RoutingG.noArgs
  , "Jobs": "jobs" /
      ( jobIdS ?
          { logLevel: Routing.optional <<< logLevelP <<< Routing.string
          , since: Routing.optional <<< timestampP <<< Routing.string
          }
      )
  }

newJobId :: Effect JobId
newJobId = (JobId <<< UUID.toString) <$> UUID.make

type PublishResponse = { jobId :: JobId }

publishResponseCodec :: JsonCodec PublishResponse
publishResponseCodec = CA.Record.object "PublishResponse" { jobId: Db.jobIdCodec }

router :: ServerEnv -> Request Route -> Run ServerEffects Response
router env { route, method, body } = HTTPurple.usingCont case route, method of
  Publish, Post -> do
    publish <- HTTPurple.fromJson (jsonDecoder Operation.publishCodec) body
    jobId <- liftEffect $ newJobId
    lift $ Log.info $ "Received publish request, job id: " <> unwrap jobId
    let newEnv = env { jobId = Just jobId }
    _fiber <- liftAff $ Aff.forkAff $ runEffects newEnv (API.publish Current publish)
    jsonOk publishResponseCodec { jobId }

  Unpublish, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Unpublish _ -> do
        -- TODO: fork, get a jobid, etc
        -- TODO: we'll need a job type too!
        -- TODO: return json
        lift $ API.authenticated auth
        HTTPurple.ok "Completed unpublish operation."
      _ ->
        HTTPurple.badRequest "Expected unpublish operation."

  Transfer, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Transfer _ -> do
        -- TODO: fork, get a jobid, and so on and so on
        -- TODO: return json
        lift $ API.authenticated auth
        HTTPurple.ok "Completed transfer operation."
      _ ->
        HTTPurple.badRequest "Expected transfer operation."

  Jobs jobId { logLevel: maybeLogLevel, since }, Get -> do
    let logLevel = fromMaybe Notify maybeLogLevel
    logs <- liftEffect $ Db.selectLogsByJob env.db jobId logLevel since
    jsonOk (CA.array Db.logLineCodec) logs

  _, _ -> HTTPurple.notFound

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
  , db :: Db
  , jobId :: Maybe JobId
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

  db <- liftEffect $ Db.connect

  pure
    { debouncer
    , githubCacheRef
    , legacyCacheRef
    , registryCacheRef
    , cacheDir
    , logsDir
    , vars
    , octokit
    , db
    , jobId: Nothing
    }

type ServerEffects = (PACCHETTIBOTTI_ENV + REGISTRY + GITHUB + GIT + STORAGE + PURSUIT + LEGACY_CACHE + LOG + EXCEPT String + AFF + EFFECT ())

runServer :: ServerEnv -> (ServerEnv -> Request Route -> Run ServerEffects Response) -> Request Route -> Aff Response
runServer env router' request = do
  result <- runEffects env (router' env request)
  case result of
    Left error -> HTTPurple.badRequest (Aff.message error)
    Right response -> pure response

main :: Effect Unit
main = do
  createServerEnv # Aff.runAff_ case _ of
    Left error -> do
      Console.log $ "Failed to start server: " <> Aff.message error
      Process.exit 1
    Right env -> do
      _close <- HTTPurple.serve
        { hostname: "0.0.0.0"
        , port: 8080
        , onStarted
        }
        { route: routes
        , router: runServer env router
        }
      pure unit
  where
  onStarted :: Effect Unit
  onStarted = do
    Console.log $ String.joinWith "\n"
      [ " ┌───────────────────────────────────────────┐"
      , " │ Server now up on port 8080                │"
      , " │                                           │"
      , " │ To test, run:                             │"
      , " │  > curl -v localhost:8080/api/v1/publish  │"
      , " └───────────────────────────────────────────┘"
      ]

jsonDecoder :: forall a. JsonCodec a -> JsonDecoder JsonDecodeError a
jsonDecoder codec = JsonDecoder (parseJson codec)

jsonEncoder :: forall a. JsonCodec a -> JsonEncoder a
jsonEncoder codec = JsonEncoder (stringifyJson codec)

jsonOk :: forall m a. MonadAff m => JsonCodec a -> a -> m Response
jsonOk codec datum = HTTPurple.ok' HTTPurple.jsonHeaders $ HTTPurple.toJson (jsonEncoder codec) datum

runEffects :: forall a. ServerEnv -> Run ServerEffects a -> Aff (Either Aff.Error a)
runEffects env f = Aff.attempt do
  now <- nowUTC
  let logFile = String.take 10 (Formatter.DateTime.format Internal.Format.iso8601Date now) <> ".log"
  let logPath = Path.concat [ env.logsDir, logFile ]
  f # Env.runPacchettiBottiEnv { publicKey: env.vars.publicKey, privateKey: env.vars.privateKey }
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
    # Except.catch (\msg -> Log.error msg *> Run.liftAff (Aff.throwError (Aff.error msg)))
    # Log.interpret
        ( \log -> case env.jobId of
            Nothing -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log
            Just jobId ->
              Log.handleTerminal Normal log
                *> Log.handleFs Verbose logPath log
                *> Log.handleDb { db: env.db } jobId log

        )
    # Run.runBaseAff'

-- TODO: request path in logs: (String.joinWith "__" request.path)

