module Registry.App.Server where

import Registry.App.Prelude hiding ((/))

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime, diff)
import Data.Exists as Exists
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Time.Duration (Milliseconds)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.String as String
import Type.Proxy (Proxy(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import HTTPurple (class Generic, JsonDecoder(..), Method(..), Request, Response, RouteDuplex', (/))
import HTTPurple as HTTPurple
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
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version
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
  | Jobs JobID

derive instance Generic Route _

newtype JobID = JobID String

instance Newtype JobID String

jobID :: RouteDuplex' JobID
jobID = _Newtype Routing.segment

-- | Generate a job ID from publish operation parameters to detect duplicates
generateJobId :: Operation.PublishData -> JobID
generateJobId payload = 
  let
    -- Create a deterministic hash based on package name, ref, and compiler
    hashInput = Array.fold
      [ PackageName.print payload.name
      , payload.ref
      , Version.print payload.compiler
      ]
    -- Use SHA256 to create a deterministic job ID, take first 8 chars for readability
    jobHash = Sha256.hash hashInput # Sha256.print # String.take 8
  in
    JobID jobHash

-- | Information about a job attempt
type JobAttempt = 
  { jobId :: JobID
  , timestamp :: DateTime
  , logFile :: String
  }

-- | Cache key for tracking job attempts
data JobCacheKey c a = JobCacheKey String (c JobAttempt a)

derive instance (Functor (c JobAttempt)) => Functor (JobCacheKey c)

instance Cache.MemoryEncodable JobCacheKey where
  encodeMemory (JobCacheKey id key) = Exists.mkExists (Cache.Key ("job-" <> id) key)

instance Cache.FsEncodable JobCacheKey where
  encodeFs (JobCacheKey id key) = Exists.mkExists (Cache.AsJson ("job-" <> id <> ".json") jobAttemptCodec key)

jobAttemptCodec :: JsonCodec JobAttempt
jobAttemptCodec = CA.Record.object "JobAttempt"
  { jobId: JobID.codec
  , timestamp: CA.datetime
  , logFile: CA.string
  }
  where
  JobID.codec = _Newtype CA.string

-- | Proxy for the job cache
_jobCache :: Proxy "jobCache"
_jobCache = Proxy

routes :: RouteDuplex' Route
routes = Routing.root $ Routing.prefix "api" $ Routing.prefix "v1" $ RoutingG.sum
  { "Publish": "publish" / RoutingG.noArgs
  , "Unpublish": "unpublish" / RoutingG.noArgs
  , "Transfer": "transfer" / RoutingG.noArgs
  , "Jobs": "jobs" / jobID
  }

router :: Request Route -> Run ServerEffects Response
router { route, method, body } = HTTPurple.usingCont case route, method of
  Publish, Post -> do
    publish <- HTTPurple.fromJson (jsonDecoder Operation.publishCodec) body
    
    -- Generate a deterministic job ID for duplicate detection
    let jobId@(JobID jobIdStr) = generateJobId publish
    
    -- Check if this exact job has been attempted recently (within last 24 hours)
    now <- nowUTC
    existingJob <- lift $ Cache.get _jobCache (JobCacheKey jobIdStr)
    
    case existingJob of
      Just attempt | timeDiffHours attempt.timestamp now < 24.0 -> do
        -- This is a duplicate job from the last 24 hours
        HTTPurple.badRequest $ String.joinWith "\n"
          [ "This is a duplicate job - the exact same publish operation has already been attempted recently."
          , ""
          , "Job ID: " <> jobIdStr
          , "Package: " <> PackageName.print publish.name <> "@" <> publish.ref
          , "Compiler: " <> Version.print publish.compiler 
          , "Previous attempt: " <> Internal.Format.printDateTime Internal.Format.iso8601DateTime attempt.timestamp
          , "Logs: " <> attempt.logFile
          , ""
          , "If you need to retry this operation, please wait 24 hours or contact the packaging team."
          ]
      _ -> do
        -- This is a new job or the previous attempt was more than 24 hours ago
        now' <- nowUTC
        let logFile = String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now') <> "-" <> jobIdStr <> ".log"
        
        -- Store this job attempt
        lift $ Cache.put _jobCache (JobCacheKey jobIdStr) { jobId, timestamp: now', logFile }
        
        -- Process the publish operation
        lift $ API.publish Current publish
        HTTPurple.ok "Completed publish operation."
  where
  timeDiffHours :: DateTime -> DateTime -> Number
  timeDiffHours earlier later = 
    let diffMs = unwrap (diff later earlier) 
    in diffMs / 1000.0 / 60.0 / 60.0

  Unpublish, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Unpublish _ -> do
        lift $ API.authenticated auth
        HTTPurple.ok "Completed unpublish operation."
      _ ->
        HTTPurple.badRequest "Expected unpublish operation."

  Transfer, Post -> do
    auth <- HTTPurple.fromJson (jsonDecoder Operation.authenticatedCodec) body
    case auth.payload of
      Operation.Transfer _ -> do
        lift $ API.authenticated auth
        HTTPurple.ok "Completed transfer operation."
      _ ->
        HTTPurple.badRequest "Expected transfer operation."

  Jobs _jobId, _ -> do
    HTTPurple.ok "TODO"

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
  , jobCacheRef :: CacheRef
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
  jobCacheRef <- Cache.newCacheRef

  octokit <- Octokit.newOctokit vars.token
  debouncer <- Git.newDebouncer

  pure { debouncer, githubCacheRef, legacyCacheRef, registryCacheRef, jobCacheRef, cacheDir, logsDir, vars, octokit }

type ServerEffects = (PACCHETTIBOTTI_ENV + REGISTRY + GITHUB + GIT + STORAGE + PURSUIT + LEGACY_CACHE + Cache.Cache JobCacheKey + NOTIFY + LOG + EXCEPT String + AFF + EFFECT ())

runServer :: ServerEnv -> (Request Route -> Run ServerEffects Response) -> Request Route -> Aff Response
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
      # Cache.interpret _jobCache (Cache.handleMemoryFs { cache: env.cacheDir, ref: env.jobCacheRef })
      # Notify.interpret Notify.handleLog
      # Except.catch (\msg -> Log.error msg *> Run.liftAff (Aff.throwError (Aff.error msg)))
      # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
      # Run.runBaseAff'

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
