module Registry.App.Main where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk, ReaderT, asks)
import Control.Monad.Reader as Reader
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Foldable (traverse_)
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Foreign.Object as Object
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.Auth as Auth
import Registry.App.Env (GITHUB_EVENT, GitHubEventEnv, PACCHETTIBOTTI, PacchettiBottiEnv)
import Registry.App.Env as Env
import Registry.App.Legacy.Manifest (LegacyCache)
import Registry.App.Monad.Cache (class MonadCache, FsCacheEnv, MemoryEnv, MemoryFsEnv)
import Registry.App.Monad.Cache as Cache
import Registry.App.Monad.Git (class MonadGit, GitEnv, PullMode(..), WriteMode(..))
import Registry.App.Monad.Git as Git
import Registry.App.Monad.GitHub (class MonadGitHub, GitHubCache, OctokitEnv)
import Registry.App.Monad.GitHub as GitHub
import Registry.App.Monad.Log (class MonadLog, LogTerminalEnv, LogVerbosity(..))
import Registry.App.Monad.Log as Log
import Registry.App.Monad.Notify (class MonadNotify, NotifyGitHubEnv)
import Registry.App.Monad.Notify as Notify
import Registry.App.Monad.PackageSets (class MonadPackageSets, PackageSetsEnv)
import Registry.App.Monad.PackageSets as PackageSets
import Registry.App.Monad.Pursuit (class MonadPursuit, PursuitEnv)
import Registry.App.Monad.Pursuit as Pursuit
import Registry.App.Monad.Registry (class MonadRegistry, RegistryCache)
import Registry.App.Monad.Registry as Registry
import Registry.App.Monad.Storage (class MonadStorage, S3Env, StorageCache)
import Registry.App.Monad.Storage as Storage
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (IssueNumber(..))
import Registry.Foreign.Octokit as Octokit
import Registry.Operation (AuthenticatedData, PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation

type Env =
  { git :: GitEnv
  , github :: OctokitEnv
  , githubEvent :: GitHubEventEnv
  , log :: LogTerminalEnv
  , notify :: NotifyGitHubEnv
  , pursuit :: PursuitEnv
  , storage :: S3Env
  , pacchettiBotti :: PacchettiBottiEnv
  , packageSets :: PackageSetsEnv
  , cache ::
      { registry :: MemoryEnv
      , github :: MemoryFsEnv
      , storage :: FsCacheEnv
      , legacy :: MemoryFsEnv
      }
  }

newtype GitHubEventM a = GitHubEventM (ReaderT Env Aff a)

derive newtype instance Functor GitHubEventM
derive newtype instance Apply GitHubEventM
derive newtype instance Applicative GitHubEventM
derive newtype instance Bind GitHubEventM
derive newtype instance Monad GitHubEventM
derive newtype instance MonadEffect GitHubEventM
derive newtype instance MonadAff GitHubEventM
derive newtype instance MonadAsk Env GitHubEventM

instance MonadPursuit GitHubEventM where
  publish = Pursuit.handlePursuitHttp

instance MonadRegistry GitHubEventM where
  readManifest = Registry.handleReadManifest
  writeManifest = Registry.handleWriteManifest
  deleteManifest = Registry.handleDeleteManifest
  readAllManifests = Registry.handleReadAllManifests
  readMetadata = Registry.handleReadMetadata
  writeMetadata = Registry.handleWriteMetadata
  readAllMetadata = Registry.handleReadAllMetadata
  readLatestPackageSet = Registry.handleReadLatestPackageSet
  writePackageSet = Registry.handleWritePackageSet
  readAllPackageSets = Registry.handleReadAllPackageSets
  -- legacy operations
  readLegacyRegistry = Registry.handleReadLegacyRegistry
  mirrorPackageSet = Registry.handleMirrorPackageSet
  mirrorLegacyRegistry = Registry.handleMirrorLegacyRegistry

instance MonadGitHub GitHubEventM where
  listTags = GitHub.handleListTags
  listTeamMembers = GitHub.handleListTeamMembers
  getContent = GitHub.handleGetContent
  getRefCommit = GitHub.handleGetRefCommit
  getCommitDate = GitHub.handleGetCommitDate

instance MonadGit GitHubEventM where
  getAddress = Git.handleGetAddress
  getPath = Git.handleGetPath
  pull = Git.handlePull
  commit = Git.handleCommit
  push = Git.handlePush
  tag = Git.handleTag
  pushTags = Git.handlePushTags

instance MonadPackageSets GitHubEventM where
  upgradeAtomic = PackageSets.handleUpgradeAtomic
  upgradeSequential = PackageSets.handleUpgradeSequential

instance MonadStorage GitHubEventM where
  upload = Storage.handleUploadS3
  download = Storage.handleDownloadS3
  delete = Storage.handleDeleteS3

instance MonadLog GitHubEventM where
  log = Log.handleLogTerminal

instance MonadNotify GitHubEventM where
  send = Notify.handleNotifyGitHub

instance MonadCache RegistryCache GitHubEventM where
  getCache key = asks _.cache >>= \env -> Cache.handleGetMemory env.registry key
  putCache key = asks _.cache >>= \env -> Cache.handlePutMemory env.registry key
  deleteCache key = asks _.cache >>= \env -> Cache.handleDeleteMemory env.registry key

instance MonadCache GitHubCache GitHubEventM where
  getCache key = asks _.cache >>= \env -> Cache.handleGetMemoryFs env.github key
  putCache key = asks _.cache >>= \env -> Cache.handlePutMemoryFs env.github key
  deleteCache key = asks _.cache >>= \env -> Cache.handleDeleteMemoryFs env.github key

instance MonadCache StorageCache GitHubEventM where
  getCache key = asks _.cache >>= \env -> Cache.handleGetFs env.storage key
  putCache key = asks _.cache >>= \env -> Cache.handlePutFs env.storage key
  deleteCache key = asks _.cache >>= \env -> Cache.handleDeleteFs env.storage key

instance MonadCache LegacyCache GitHubEventM where
  getCache key = asks _.cache >>= \env -> Cache.handleGetMemoryFs env.legacy key
  putCache key = asks _.cache >>= \env -> Cache.handlePutMemoryFs env.legacy key
  deleteCache key = asks _.cache >>= \env -> Cache.handleDeleteMemoryFs env.legacy key

runGitHubEventM :: forall a. Env -> GitHubEventM a -> Aff a
runGitHubEventM env (GitHubEventM m) = Reader.runReaderT m env

main :: Effect Unit
main = launchAff_ $ do
  -- For now we only support GitHub events, and no formal API, so we'll jump
  -- straight into the GitHub event workflow.
  initializeGitHub >>= traverse_ \{ env, operation } -> do
    let
      program :: GitHubEventM (Either String Unit)
      program = case operation of
        Left packageSetOperation -> case packageSetOperation of
          PackageSetUpdate payload ->
            API.packageSetUpdate payload

        Right packageOperation -> case packageOperation of
          Publish payload ->
            API.publish Current payload
          Authenticated payload -> Except.runExceptT do
            -- If we receive an authenticated operation via GitHub, then we
            -- re-sign it with pacchettibotti credentials if and only if the
            -- operation was opened by a trustee.
            signed <- signPacchettiBottiIfTrustee payload
            Except.ExceptT $ API.authenticated signed

    -- We'll run the program, reporting failures as issue comments.
    runGitHubEventM env program >>= case _ of
      Left error ->
        Octokit.request env.github.octokit (Octokit.createCommentRequest { address: Constants.registry, issue: env.githubEvent.issue, body: error }) >>= case _ of
          Left githubError -> Aff.throwError $ Aff.error $ Octokit.printGitHubError githubError
          Right _ -> pure unit
      Right _ ->
        Octokit.request env.github.octokit (Octokit.closeIssueRequest { address: Constants.registry, issue: env.githubEvent.issue }) >>= case _ of
          Left error -> Aff.throwError $ Aff.error $ Octokit.printGitHubError error
          Right _ -> pure unit

initializeGitHub :: Aff (Maybe { env :: Env, operation :: Either PackageSetOperation PackageOperation })
initializeGitHub = do
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret
  eventPath <- Env.lookupRequired Env.githubEventPath

  octokit <- Octokit.newOctokit token

  readOperation eventPath >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure Nothing

    MalformedJson issue err -> do
      let
        comment = String.joinWith "\n"
          [ "The JSON input for this package update is malformed:"
          , "```"
          , err
          , "```"
          , "You can try again by commenting on this issue with a corrected payload."
          ]

      Octokit.request octokit (Octokit.createCommentRequest { address: Constants.registry, issue, body: comment }) >>= case _ of
        Left githubError -> Aff.throwError $ Aff.error $ Octokit.printGitHubError githubError
        Right _ -> pure Nothing

    DecodedOperation issue username operation -> do
      -- Caching
      let cacheDir = Path.concat [ scratchDir, ".cache" ]
      FS.Extra.ensureDirectory cacheDir
      githubCacheRef <- Cache.newCacheRef
      legacyCacheRef <- Cache.newCacheRef
      registryCacheRef <- Cache.newCacheRef

      --  Package sets
      let workdir = Path.concat [ scratchDir, "package-sets-work" ]
      FS.Extra.ensureDirectory workdir

      debouncer <- Git.newDebouncer

      let
        git :: GitEnv
        git =
          { repos: Git.defaultRepos
          , pull: ForceClean
          , write: CommitAs (Git.pacchettibottiCommitter token)
          , workdir: scratchDir
          , debouncer
          }

        github :: OctokitEnv
        github = { octokit }

        githubEvent :: GitHubEventEnv
        githubEvent = { username, issue }

        log :: LogTerminalEnv
        log = { verbosity: Verbose }

        notify :: NotifyGitHubEnv
        notify = { octokit, issue, registry: Constants.registry }

        pursuit :: PursuitEnv
        pursuit = { token }

        storage :: S3Env
        storage = { key: spacesKey, secret: spacesSecret }

        pacchettiBotti :: PacchettiBottiEnv
        pacchettiBotti = { publicKey, privateKey }

        packageSets :: PackageSetsEnv
        packageSets = { workdir }

        cache :: { registry :: MemoryEnv, github :: MemoryFsEnv, storage :: FsCacheEnv, legacy :: MemoryFsEnv }
        cache =
          { registry: { ref: registryCacheRef }
          , github: { ref: githubCacheRef, cacheDir }
          , storage: { cacheDir }
          , legacy: { ref: legacyCacheRef, cacheDir }
          }

        env :: Env
        env = { git, github, githubEvent, log, notify, pursuit, storage, pacchettiBotti, packageSets, cache }

      pure $ Just { env, operation }

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber String (Either PackageSetOperation PackageOperation)

derive instance Eq OperationDecoding

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.Aff.readTextFile UTF8 eventPath

  IssueEvent { issueNumber, body, username } <- case Argonaut.Parser.jsonParser fileContents >>= decodeIssueEvent of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  let
    -- TODO: Right now we parse all operations from GitHub issues, but we should
    -- in the future only parse out package set operations. The others should be
    -- handled via a HTTP API.
    decodeOperation :: Json -> Either JsonDecodeError (Either PackageSetOperation PackageOperation)
    decodeOperation json = do
      object <- CA.decode CA.jobject json
      let keys = Object.keys object
      let hasKeys = all (flip Array.elem keys)
      if hasKeys [ "packages" ] then
        map (Left <<< PackageSetUpdate) (CA.decode Operation.packageSetUpdateCodec json)
      else if hasKeys [ "name", "ref", "compiler" ] then
        map (Right <<< Publish) (CA.decode Operation.publishCodec json)
      else if hasKeys [ "payload", "signature", "email" ] then
        map (Right <<< Authenticated) (CA.decode Operation.authenticatedCodec json)
      else
        Left $ CA.TypeMismatch "Operation: Expected a valid registry operation, but provided object did not match any operation decoder."

  case Argonaut.Parser.jsonParser (JsonRepair.tryRepair (firstObject body)) of
    Left err -> do
      Console.log "Not JSON."
      Console.logShow { err, body }
      pure NotJson
    Right json -> case decodeOperation json of
      Left jsonError -> do
        let printedError = CA.printJsonDecodeError jsonError
        Console.log $ "Malformed JSON:\n" <> printedError
        Console.log $ "Received body:\n" <> body
        pure $ MalformedJson issueNumber printedError
      Right operation ->
        pure $ DecodedOperation issueNumber username operation

-- | Users may submit issues with contents wrapped in code fences, perhaps with
-- | a language specifier, trailing lines, and other issues. This rudimentary
-- | cleanup pass retrieves all contents within an opening { and closing }
-- | delimiter.
firstObject :: String -> String
firstObject input = fromMaybe input do
  before <- String.indexOf (String.Pattern "{") input
  let start = String.drop before input
  after <- String.lastIndexOf (String.Pattern "}") start
  pure (String.take (after + 1) start)

-- | An event triggered by a GitHub workflow, specifically via an issue comment
-- | or issue creation.
-- | https://docs.github.com/developers/webhooks-and-events/webhooks/webhook-events-and-payloads#issue_comment
newtype IssueEvent = IssueEvent
  { issueNumber :: IssueNumber
  , body :: String
  , username :: String
  }

derive instance Newtype IssueEvent _

decodeIssueEvent :: Json -> Either String IssueEvent
decodeIssueEvent json = lmap CA.printJsonDecodeError do
  object <- CA.decode CA.jobject json
  username <- atKey "login" CA.string =<< atKey "sender" CA.jobject object

  issueObject <- atKey "issue" CA.jobject object
  issueNumber <- atKey "number" CA.int issueObject

  -- We accept issue creation and issue comment events, but both contain an
  -- 'issue' field. However, only comments contain a 'comment' field. For that
  -- reason we first try to parse the comment and fall back to the issue if
  -- that fails.
  body <- atKey "body" CA.string =<< atKey "comment" CA.jobject object <|> pure issueObject
  pure $ IssueEvent { body, username, issueNumber: IssueNumber issueNumber }
  where
  atKey :: forall a. String -> JsonCodec a -> Object Json -> Either JsonDecodeError a
  atKey key codec object =
    maybe
      (Left $ CA.AtKey key CA.MissingValue)
      (lmap (CA.AtKey key) <<< CA.decode codec)
      (Object.lookup key object)

-- | Re-sign a payload as pacchettibotti if the authenticated operation was
-- | submitted by a registry trustee.
--
-- @pacchettibotti is considered an 'owner' of all packages for authenticated
-- operations. Registry trustees can ask pacchettibotti to perform an action on
-- behalf of a package by submitting a payload with the @pacchettibotti email
-- address. If the payload was submitted by a trustee (ie. a member of the
-- packaging team) then pacchettibotti will re-sign it and add itself as an
-- owner before continuing with the authenticated operation.
signPacchettiBottiIfTrustee
  :: forall m r
   . MonadAsk { | PACCHETTIBOTTI + GITHUB_EVENT + r } m
  => MonadGitHub m
  => MonadAff m
  => AuthenticatedData
  -> ExceptT String m AuthenticatedData
signPacchettiBottiIfTrustee auth = do
  if auth.email /= pacchettibottiEmail then
    pure auth
  else do
    GitHub.listTeamMembers API.packagingTeam >>= case _ of
      Left githubError -> Except.throwError $ Array.fold
        [ "This authenticated operation was opened using the pacchettibotti "
        , "email address, but we were unable to authenticate that you are a "
        , "member of the @purescript/packaging team:\n\n"
        , Octokit.printGitHubError githubError
        ]
      Right members -> do
        { username } <- asks _.githubEvent
        unless (Array.elem username members) do
          Except.throwError $ Array.fold
            [ "This authenticated operation was opened using the pacchettibotti "
            , "email address, but your username is not a member of the "
            , "@purescript/packaging team."
            ]

        { publicKey, privateKey } <- asks _.pacchettiBotti
        signature <- liftAff (Auth.signPayload { publicKey, privateKey, rawPayload: auth.rawPayload }) >>= case _ of
          Left _ -> Except.throwError "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature

        pure $ auth { signature = signature }
