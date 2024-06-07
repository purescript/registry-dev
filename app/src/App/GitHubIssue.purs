module Registry.App.GitHubIssue where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Foldable (traverse_)
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import JSON as JSON
import JSON.Object as CJ.Object
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Auth as Auth
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Source as Source
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (GitHubToken, IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.S3 (SpaceKey)
import Registry.Operation (AuthenticatedData, PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Run (Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

main :: Effect Unit
main = launchAff_ $ do
  -- For now we only support GitHub events, and no formal API, so we'll jump
  -- straight into the GitHub event workflow.
  initializeGitHub >>= traverse_ \env -> do
    let
      run = case env.operation of
        Left packageSetOperation -> case packageSetOperation of
          PackageSetUpdate payload ->
            API.packageSetUpdate payload

        Right packageOperation -> case packageOperation of
          Publish payload ->
            API.publish CurrentPackage payload
          Authenticated payload -> do
            -- If we receive an authenticated operation via GitHub, then we
            -- re-sign it with pacchettibotti credentials if and only if the
            -- operation was opened by a trustee.
            signed <- signPacchettiBottiIfTrustee payload
            API.authenticated signed

    -- Caching
    let cache = Path.concat [ scratchDir, ".cache" ]
    FS.Extra.ensureDirectory cache
    githubCacheRef <- Cache.newCacheRef
    legacyCacheRef <- Cache.newCacheRef
    registryCacheRef <- Cache.newCacheRef

    -- Registry env
    debouncer <- Registry.newDebouncer
    let
      registryEnv :: Registry.RegistryEnv
      registryEnv =
        { repos: Registry.defaultRepos
        , pull: Git.ForceClean
        , write: Registry.CommitAs (Git.pacchettibottiCommitter env.token)
        , workdir: scratchDir
        , debouncer
        , cacheRef: registryCacheRef
        }

    --  Package sets
    let workdir = Path.concat [ scratchDir, "package-sets-work" ]
    FS.Extra.ensureDirectory workdir

    thrownRef <- liftEffect $ Ref.new false

    run
      -- App effects
      # PackageSets.interpret (PackageSets.handle { workdir })
      # Registry.interpret (Registry.handle registryEnv)
      # Storage.interpret (Storage.handleS3 { s3: env.spacesConfig, cache })
      # Pursuit.interpret (Pursuit.handleAff env.token)
      # Source.interpret Source.handle
      # GitHub.interpret (GitHub.handle { octokit: env.octokit, cache, ref: githubCacheRef })
      -- Caching & logging
      # Cache.interpret Legacy.Manifest._legacyCache (Cache.handleMemoryFs { cache, ref: legacyCacheRef })
      # Except.catch (\msg -> Log.error msg *> Comment.comment msg *> Run.liftEffect (Ref.write true thrownRef))
      # Comment.interpret (Comment.handleGitHub { octokit: env.octokit, issue: env.issue, registry: Registry.defaultRepos.registry })
      # Log.interpret (Log.handleTerminal Verbose)
      -- Environments
      # Env.runResourceEnv env.resourceEnv
      # Env.runGitHubEventEnv { username: env.username, issue: env.issue }
      # Env.runPacchettiBottiEnv { publicKey: env.publicKey, privateKey: env.privateKey }
      -- Base effects
      # Run.runBaseAff'

    liftEffect (Ref.read thrownRef) >>= case _ of
      true ->
        liftEffect $ Process.exit' 1
      _ -> do
        -- After the run, close the issue. If an exception was thrown then the issue will remain open.
        _ <- Octokit.request env.octokit (Octokit.closeIssueRequest { address: Constants.registry, issue: env.issue })
        pure unit

type GitHubEventEnv =
  { octokit :: Octokit
  , token :: GitHubToken
  , issue :: IssueNumber
  , username :: String
  , operation :: Either PackageSetOperation PackageOperation
  , spacesConfig :: SpaceKey
  , publicKey :: String
  , privateKey :: String
  , resourceEnv :: Env.ResourceEnv
  }

initializeGitHub :: Aff (Maybe GitHubEventEnv)
initializeGitHub = do
  token <- Env.lookupRequired Env.pacchettibottiToken
  publicKey <- Env.lookupRequired Env.pacchettibottiED25519Pub
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret
  resourceEnv <- Env.lookupResourceEnv
  eventPath <- Env.lookupRequired Env.githubEventPath

  octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl

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
      pure $ Just
        { octokit
        , token
        , issue
        , username
        , operation
        , spacesConfig: { key: spacesKey, secret: spacesSecret }
        , publicKey
        , privateKey
        , resourceEnv
        }

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber String (Either PackageSetOperation PackageOperation)

derive instance Eq OperationDecoding

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.Aff.readTextFile UTF8 eventPath

  IssueEvent { issueNumber, body, username } <- case JSON.parse fileContents >>= decodeIssueEvent of
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
    decodeOperation :: JSON -> Either CJ.DecodeError (Either PackageSetOperation PackageOperation)
    decodeOperation json = do
      object <- CJ.decode CJ.jobject json
      let keys = CJ.Object.keys object
      let hasKeys = all (flip Array.elem keys)
      if hasKeys [ "packages" ] then
        map (Left <<< PackageSetUpdate) (CJ.decode Operation.packageSetUpdateCodec json)
      else if hasKeys [ "name", "ref", "compiler" ] then
        map (Right <<< Publish) (CJ.decode Operation.publishCodec json)
      else if hasKeys [ "payload", "signature" ] then
        map (Right <<< Authenticated) (CJ.decode Operation.authenticatedCodec json)
      else
        Left $ CJ.DecodeError.basic "Operation: Expected a valid registry operation, but provided object did not match any operation decoder."

  case JSON.parse (JsonRepair.tryRepair (firstObject body)) of
    Left err -> do
      Console.log "Not JSON."
      Console.logShow { err, body }
      pure NotJson
    Right json -> case decodeOperation json of
      Left jsonError -> do
        let printedError = CJ.DecodeError.print jsonError
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

decodeIssueEvent :: JSON -> Either String IssueEvent
decodeIssueEvent json = lmap CJ.DecodeError.print do
  object <- CJ.decode CJ.jobject json
  username <- Octokit.atKey "login" CJ.string =<< Octokit.atKey "sender" CJ.jobject object

  issueObject <- Octokit.atKey "issue" CJ.jobject object
  issueNumber <- Octokit.atKey "number" CJ.int issueObject

  -- We accept issue creation and issue comment events, but both contain an
  -- 'issue' field. However, only comments contain a 'comment' field. For that
  -- reason we first try to parse the comment and fall back to the issue if
  -- that fails.
  body <- Octokit.atKey "body" CJ.string =<< Octokit.atKey "comment" CJ.jobject object <|> pure issueObject
  pure $ IssueEvent { body, username, issueNumber: IssueNumber issueNumber }

-- | Re-sign a payload as pacchettibotti if the authenticated operation was
-- | submitted by a registry trustee.
--
-- @pacchettibotti is considered an 'owner' of all packages for authenticated
-- operations. Registry trustees can ask pacchettibotti to perform an action on
-- behalf of a package by submitting a payload with an empty signature. If the
-- payload was submitted by a trustee (ie. a member of the packaging team) then
-- pacchettibotti will re-sign it and add itself as an owner before continuing
-- with the authenticated operation.
signPacchettiBottiIfTrustee
  :: forall r
   . AuthenticatedData
  -> Run (GITHUB + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + LOG + EXCEPT String + r) AuthenticatedData
signPacchettiBottiIfTrustee auth = do
  GitHub.listTeamMembers API.packagingTeam >>= case _ of
    Left githubError -> do
      Log.warn $ Array.fold
        [ "Unable to fetch members of packaging team, not verifying whether requestor is a member of @purescript/packaging: "
        , Octokit.printGitHubError githubError
        ]
      pure auth
    Right members -> do
      { username } <- Env.askGitHubEvent
      if Array.elem username members then do
        Log.info "Authenticated payload submitted by a registry trustee, re-signing with pacchettibotti keys."
        { privateKey } <- Env.askPacchettiBotti
        signature <- case Auth.signPayload { privateKey, rawPayload: auth.rawPayload } of
          Left _ -> Except.throw "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature
        pure $ auth { signature = signature }
      else do
        Log.info "Authenticated payload not submitted by a registry trustee, continuing with original signature."
        pure auth
