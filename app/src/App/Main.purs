module Registry.App.Main where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Foldable (traverse_)
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Foreign.Object as Object
import Node.FS.Aff as FS.Aff
import Node.Process as Process
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.Auth as Auth
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.Constants as Constants
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (GitHubToken, IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.S3 (SpaceKey)
import Registry.Operation (AuthenticatedData, PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Run (AFF, Run)
import Run as Run

main :: Effect Unit
main = launchAff_ $ do
  -- For now we only support GitHub events, and no formal API, so we'll jump
  -- straight into the GitHub event workflow.
  initializeGitHub >>= traverse_ \env -> do
    let
      _app = case env.operation of
        Left packageSetOperation -> case packageSetOperation of
          PackageSetUpdate updateData ->
            API.packageSetUpdate updateData
        Right packageOperation -> case packageOperation of
          Publish publishData ->
            API.publish Current publishData
          Authenticated authData -> do
            -- If we receive an authenticated operation via GitHub, then we
            -- re-sign it with pacchettibotti credentials if and only if the
            -- operation was opened by a trustee.
            signed <- signPacchettiBottiIfTrustee authData
            API.authenticated signed

    -- After the run, close the issue. If an exception was thrown then the issue
    -- will remain open.
    _ <- Octokit.request env.octokit (Octokit.closeIssueRequest { address: Constants.registry, issue: env.issue })
    pure unit

type GitHubEventEnv =
  { octokit :: Octokit
  , token :: GitHubToken
  , issue :: IssueNumber
  , username :: String
  , operation :: Either PackageSetOperation PackageOperation
  , spacesConfig :: SpaceKey
  , pacchettibottiKeys :: { publicKey :: String, privateKey :: String }
  }

initializeGitHub :: Aff (Maybe GitHubEventEnv)
initializeGitHub = do
  envVars <- liftEffect Env.readEnvVars

  token <- liftEffect $ case envVars.pacchettibottiToken of
    Nothing -> Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment."
    Just token -> pure token

  publicKey <- liftEffect $ case envVars.pacchettibottiED25519Pub of
    Nothing -> Exception.throw "PACCHETTIBOTTI_ED25519_PUB not defined in the environment."
    Just key -> pure key

  privateKey <- liftEffect $ case envVars.pacchettibottiED25519 of
    Nothing -> Exception.throw "PACCHETTIBOTTI_ED25519 not defined in the environment."
    Just key -> pure key

  spacesKey <- liftEffect $ case envVars.spacesKey of
    Nothing -> Exception.throw "SPACES_KEY not defined in the environment."
    Just key -> pure key

  spacesSecret <- liftEffect $ case envVars.spacesSecret of
    Nothing -> Exception.throw "SPACES_SECRET not defined in the environment."
    Just key -> pure key

  eventPath <- liftEffect $ Process.lookupEnv "GITHUB_EVENT_PATH" >>= case _ of
    Nothing -> Exception.throw "GITHUB_EVENT_PATH not defined in the environment."
    Just path -> pure path

  octokit <- liftEffect $ Octokit.newOctokit token

  readOperation eventPath >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure Nothing

    MalformedJson issue err -> do
      let
        comment = Array.fold
          [ "The JSON input for this package update is malformed:"
          , "\n"
          , "```" <> err <> "```"
          , "\n"
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
        , pacchettibottiKeys: { publicKey, privateKey }
        }

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
  :: forall r
   . AuthenticatedData
  -> Run (GITHUB + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + LOG_EXCEPT + AFF + r) AuthenticatedData
signPacchettiBottiIfTrustee auth = do
  if auth.email /= pacchettiBottiEmail then
    pure auth
  else do
    GitHub.listTeamMembers API.packagingTeam >>= case _ of
      Left githubError -> Log.exit $ Array.fold
        [ "This authenticated operation was opened using the pacchettibotti "
        , "email address, but we were unable to authenticate that you are a "
        , "member of the @purescript/packaging team:\n\n"
        , Octokit.printGitHubError githubError
        ]
      Right members -> do
        { username } <- Env.askGitHubEvent
        unless (Array.elem username members) do
          Log.exit $ Array.fold
            [ "This authenticated operation was opened using the pacchettibotti "
            , "email address, but your username is not a member of the "
            , "@purescript/packaging team."
            ]

        { publicKey, privateKey } <- Env.askPacchettiBotti
        signature <- Run.liftAff (Auth.signPayload { publicKey, privateKey, rawPayload: auth.rawPayload }) >>= case _ of
          Left _ -> Log.exit "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature

        pure $ auth { signature = signature }

