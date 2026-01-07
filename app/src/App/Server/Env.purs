module Registry.App.Server.Env where

import Registry.App.Prelude hiding ((/))

import Data.Codec.JSON as CJ
import Data.Formatter.DateTime as Formatter.DateTime
import Data.String as String
import Effect.Aff as Aff
import HTTPurple (JsonDecoder(..), JsonEncoder(..), Request, Response)
import HTTPurple as HTTPurple
import Node.Path as Path
import Registry.API.V1 (JobId, Route)
import Registry.App.API (COMPILER_CACHE, _compilerCache)
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache (CacheRef)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Db (DB)
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Archive (ARCHIVE)
import Registry.App.Effect.Archive as Archive
import Registry.App.Effect.Env (PACCHETTIBOTTI_ENV, RESOURCE_ENV, ResourceEnv)
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
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

type ServerEnvVars =
  { token :: GitHubToken
  , publicKey :: String
  , privateKey :: String
  , spacesKey :: String
  , spacesSecret :: String
  , resourceEnv :: ResourceEnv
  }

readServerEnvVars :: Aff ServerEnvVars
readServerEnvVars = do
  Env.loadEnvFile ".temp/local-server/.env.local"
  Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret
  resourceEnv <- Env.lookupResourceEnv
  pure { token, publicKey, privateKey, spacesKey, spacesSecret, resourceEnv }

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

  octokit <- Octokit.newOctokit vars.token vars.resourceEnv.githubApiUrl
  debouncer <- Registry.newDebouncer

  db <- liftEffect $ SQLite.connect
    { database: vars.resourceEnv.databaseUrl.path
    -- To see all database queries logged in the terminal, use this instead
    -- of 'mempty'. Turned off by default because this is so verbose.
    -- Run.runBaseEffect <<< Log.interpret (Log.handleTerminal Normal) <<< Log.info
    , logger: mempty
    }

  -- At server startup we clean out all the jobs that are not completed,
  -- because they are stale runs from previous startups of the server.
  -- We can just remove the jobs, and all the logs belonging to them will be
  -- removed automatically by the foreign key constraint.
  liftEffect $ SQLite.resetIncompleteJobs db

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

type ServerEffects = (RESOURCE_ENV + PACCHETTIBOTTI_ENV + ARCHIVE + REGISTRY + STORAGE + PURSUIT + SOURCE + DB + GITHUB + LEGACY_CACHE + COMPILER_CACHE + LOG + EXCEPT String + AFF + EFFECT ())

runServer
  :: ServerEnv
  -> (ServerEnv -> Request Route -> Run ServerEffects Response)
  -> Request Route
  -> Aff Response
runServer env router' request = do
  result <- runEffects env (router' env request)
  case result of
    Left error -> HTTPurple.badRequest (Aff.message error)
    Right response -> pure response

jsonDecoder :: forall a. CJ.Codec a -> JsonDecoder CJ.DecodeError a
jsonDecoder codec = JsonDecoder (parseJson codec)

jsonEncoder :: forall a. CJ.Codec a -> JsonEncoder a
jsonEncoder codec = JsonEncoder (stringifyJson codec)

jsonOk :: forall m a. MonadAff m => CJ.Codec a -> a -> m Response
jsonOk codec datum = HTTPurple.ok' HTTPurple.jsonHeaders $ HTTPurple.toJson (jsonEncoder codec) datum

runEffects :: forall a. ServerEnv -> Run ServerEffects a -> Aff (Either Aff.Error a)
runEffects env operation = Aff.attempt do
  today <- nowUTC
  let logFile = String.take 10 (Formatter.DateTime.format Internal.Format.iso8601Date today) <> ".log"
  let logPath = Path.concat [ env.logsDir, logFile ]
  operation
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
    # Archive.interpret Archive.handle
    # Pursuit.interpret (Pursuit.handleAff env.vars.token)
    # Storage.interpret (Storage.handleS3 { s3: { key: env.vars.spacesKey, secret: env.vars.spacesSecret }, cache: env.cacheDir })
    # Source.interpret (Source.handle Source.Recent)
    # GitHub.interpret (GitHub.handle { octokit: env.octokit, cache: env.cacheDir, ref: env.githubCacheRef })
    # Cache.interpret _legacyCache (Cache.handleMemoryFs { cache: env.cacheDir, ref: env.legacyCacheRef })
    # Cache.interpret _compilerCache (Cache.handleFs env.cacheDir)
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
    # Log.interpret
        ( \log -> case env.jobId of
            Nothing -> Log.handleTerminal Verbose log *> Log.handleFs Verbose logPath log
            Just jobId ->
              Log.handleTerminal Verbose log
                *> Log.handleFs Verbose logPath log
                *> Log.handleDb { db: env.db, job: jobId } log
        )
    # Env.runPacchettiBottiEnv { publicKey: env.vars.publicKey, privateKey: env.vars.privateKey }
    # Env.runResourceEnv env.vars.resourceEnv
    # Run.runBaseAff'
