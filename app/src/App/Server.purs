module Registry.App.Server where

import Registry.App.Prelude hiding ((/))

import Control.Monad.Cont (ContT)
import Data.Codec.Argonaut as CA
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Newtype (unwrap)
import Data.String as String
import Data.UUID.Random as UUID
import Effect.Aff as Aff
import Effect.Class.Console as Console
import HTTPurple (JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response)
import HTTPurple as HTTPurple
import Node.Path as Path
import Node.Process as Process
import Record as Record
import Registry.API.V1 (JobId(..), JobType(..), LogLevel(..), Route(..))
import Registry.API.V1 as V1
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache (CacheRef)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment (COMMENT)
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Db (DB)
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Env (DatabaseUrl, PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Pursuit (PURSUIT)
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Source (SOURCE)
import Registry.App.Effect.Source as Source
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Manifest (LEGACY_CACHE, _legacyCache)
import Registry.App.SQLite (SQLite)
import Registry.App.SQLite as SQLite
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (GitHubToken, Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

newJobId :: forall m. MonadEffect m => m JobId
newJobId = liftEffect do
  id <- UUID.make
  pure $ JobId $ UUID.toString id

router :: ServerEnv -> Request Route -> Run ServerEffects Response
router env { route, method, body } = HTTPurple.usingCont case route, method of
  Publish, Post -> do
    publish <- HTTPurple.fromJson (jsonDecoder Operation.publishCodec) body
    lift $ Log.info $ "Received Publish request: " <> printJson Operation.publishCodec publish
    forkPipelineJob publish.name publish.ref PublishJob
      \jobId -> do
        Log.info $ "Received Publish request, job id: " <> unwrap jobId
        API.publish Current publish

  Unpublish, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Unpublish { name, version } -> do
        forkPipelineJob name (Version.print version) UnpublishJob
          \jobId -> do
            Log.info $ "Received Unpublish request, job id: " <> unwrap jobId
            API.authenticated auth
      _ ->
        HTTPurple.badRequest "Expected unpublish operation."

  Transfer, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Transfer { name } -> do
        forkPipelineJob name "" TransferJob
          \jobId -> do
            Log.info $ "Received Transfer request, job id: " <> unwrap jobId
            API.authenticated auth
      _ ->
        HTTPurple.badRequest "Expected transfer operation."

  Jobs, Get -> do
    jsonOk (CA.array V1.jobCodec) []

  Job jobId { level: maybeLogLevel, since }, Get -> do
    let logLevel = fromMaybe Error maybeLogLevel
    logs <- lift $ Db.selectLogsByJob jobId logLevel since
    lift (Db.selectJob jobId) >>= case _ of
      Left err -> do
        lift $ Log.error $ "Error while fetching job: " <> err
        HTTPurple.notFound
      Right job -> do
        jsonOk V1.jobCodec (Record.insert (Proxy :: _ "logs") logs job)

  _, _ -> HTTPurple.notFound
  where
  forkPipelineJob :: PackageName -> String -> JobType -> (JobId -> Run _ Unit) -> ContT Response (Run _) Response
  forkPipelineJob packageName ref jobType action = do
    -- First thing we check if the package already has a pipeline in progress
    lift (Db.runningJobForPackage packageName) >>= case _ of
      -- If yes, we error out if it's the wrong kind, return it if it's the same type
      Right { jobId, jobType: runningJobType } -> do
        lift $ Log.info $ "Found running job for package " <> PackageName.print packageName <> ", job id: " <> unwrap jobId
        case runningJobType == jobType of
          true -> jsonOk V1.jobCreatedResponseCodec { jobId }
          false -> HTTPurple.badRequest $ "There is already a " <> V1.printJobType runningJobType <> " job running for package " <> PackageName.print packageName
      -- otherwise spin up a new thread
      Left _err -> do
        lift $ Log.info $ "No running job for package " <> PackageName.print packageName <> ", creating a new one"
        jobId <- newJobId
        now <- nowUTC
        let newJob = { createdAt: now, jobId, jobType, packageName, ref }
        lift $ Db.createJob newJob
        let newEnv = env { jobId = Just jobId }

        _fiber <- liftAff $ Aff.forkAff $ Aff.attempt $ do
          void $ runEffects newEnv (action jobId)
          finishedAt <- nowUTC
          void $ runEffects newEnv (Db.finishJob { jobId, finishedAt, success: true })
        jsonOk V1.jobCreatedResponseCodec { jobId }

type ServerEnvVars =
  { token :: GitHubToken
  , publicKey :: String
  , privateKey :: String
  , spacesKey :: String
  , spacesSecret :: String
  , databaseUrl :: DatabaseUrl
  }

readServerEnvVars :: Aff ServerEnvVars
readServerEnvVars = do
  Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret
  databaseUrl <- Env.lookupRequired Env.databaseUrl
  pure { token, publicKey, privateKey, spacesKey, spacesSecret, databaseUrl }

type ServerEnv =
  { cacheDir :: FilePath
  , logsDir :: FilePath
  , githubCacheRef :: CacheRef
  , legacyCacheRef :: CacheRef
  , registryCacheRef :: CacheRef
  , octokit :: Octokit
  , vars :: ServerEnvVars
  , debouncer :: Registry.Debouncer
  , db :: SQLite
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
  debouncer <- Registry.newDebouncer

  db <- liftEffect $ SQLite.connect
    { database: vars.databaseUrl.path
    , logger: Run.runBaseEffect <<< Log.interpret (Log.handleTerminal Verbose) <<< Log.info
    }

  -- At server startup we clean out all the jobs that are not completed,
  -- because they are stale runs from previous startups of the server.
  -- We can just remove the jobs, and all the logs belonging to them will be
  -- removed automatically by the foreign key constraint.
  liftEffect $ SQLite.deleteIncompleteJobs db

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

type ServerEffects = (PACCHETTIBOTTI_ENV + REGISTRY + STORAGE + PURSUIT + SOURCE + DB + GITHUB + LEGACY_CACHE + COMMENT + LOG + EXCEPT String + AFF + EFFECT ())

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
        { route: V1.routes
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
      , " │  > curl -v localhost:8080/api/v1/jobs     │"
      , " └───────────────────────────────────────────┘"
      ]

jsonDecoder :: forall a. JsonCodec a -> JsonDecoder JsonDecodeError a
jsonDecoder codec = JsonDecoder (parseJson codec)

jsonEncoder :: forall a. JsonCodec a -> JsonEncoder a
jsonEncoder codec = JsonEncoder (stringifyJson codec)

jsonOk :: forall m a. MonadAff m => JsonCodec a -> a -> m Response
jsonOk codec datum = HTTPurple.ok' HTTPurple.jsonHeaders $ HTTPurple.toJson (jsonEncoder codec) datum

runEffects :: forall a. ServerEnv -> Run ServerEffects a -> Aff (Either Aff.Error a)
runEffects env operation = Aff.attempt do
  today <- nowUTC
  let logFile = String.take 10 (Formatter.DateTime.format Internal.Format.iso8601Date today) <> ".log"
  let logPath = Path.concat [ env.logsDir, logFile ]
  operation
    # Env.runPacchettiBottiEnv { publicKey: env.vars.publicKey, privateKey: env.vars.privateKey }
    # Registry.interpret
        ( Registry.handle
            { repos: Registry.defaultRepos
            , pull: Git.ForceClean
            , write: Registry.CommitAs (Git.pacchettibottiCommitter env.vars.token)
            , workdir: scratchDir
            , debouncer: env.debouncer
            , cacheRef: env.registryCacheRef
            }
        )
    # Pursuit.interpret (Pursuit.handleAff env.vars.token)
    # Storage.interpret (Storage.handleS3 { s3: { key: env.vars.spacesKey, secret: env.vars.spacesSecret }, cache: env.cacheDir })
    # Source.interpret Source.handle
    # GitHub.interpret (GitHub.handle { octokit: env.octokit, cache: env.cacheDir, ref: env.githubCacheRef })
    # Cache.interpret _legacyCache (Cache.handleMemoryFs { cache: env.cacheDir, ref: env.legacyCacheRef })
    # Except.catch
        ( \msg -> do
            finishedAt <- nowUTC
            case env.jobId of
              -- Important to make sure that we mark the job as completed
              Just jobId -> Db.finishJob { jobId, finishedAt, success: false }
              Nothing -> pure unit
            Log.error msg *> Run.liftAff (Aff.throwError (Aff.error msg))
        )
    # Db.interpret (Db.handleSQLite { db: env.db })
    # Comment.interpret Comment.handleLog
    # Log.interpret
        ( \log -> case env.jobId of
            Nothing -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log
            Just jobId ->
              Log.handleTerminal Normal log
                *> Log.handleFs Verbose logPath log
                *> Log.handleDb { db: env.db, job: jobId } log
        )
    # Run.runBaseAff'
