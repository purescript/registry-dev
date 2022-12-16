module Registry.App.Main where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.String as String
import Data.String.Base64 as Base64
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Foreign.Object as Object
import Node.FS.Aff as FS.Aff
import Node.Process as Process
import Registry.App.API as API
import Registry.Constants as Constants
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (GitHubToken(..), IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Operation (PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation

main :: Effect Unit
main = launchAff_ $ do
  -- For now we only support GitHub events, and no formal API, so we'll jump
  -- straight into the GitHub event workflow.
  maybeEnv <- initializeGitHub
  for_ maybeEnv \env -> do
    -- TODO: Set up run.
    -- runOperation API operation

    -- TODO: After the run, close the issue, assuming that it will stay open if
    -- an exception was thrown.
    _ <- Octokit.request env.octokit (Octokit.closeIssueRequest { address: Constants.registry, issue: env.issue })
    pure unit

-- | Loads the `.env` file into the environment.
loadEnv :: Aff Dotenv.Settings
loadEnv = do
  contents <- Aff.try $ FS.Aff.readTextFile UTF8 envFilePath
  case contents of
    Left _ -> Console.log ("Not loading .env file because none was found at path: " <> envFilePath) $> []
    Right string -> Dotenv.loadContents (String.trim string)

envFilePath :: FilePath
envFilePath = ".env"

type GitHubEventEnv =
  { octokit :: Octokit
  , token :: GitHubToken
  , issue :: IssueNumber
  , username :: String
  , operation :: Either PackageSetOperation PackageOperation
  , pacchettibottiKeys :: { publicKey :: String, privateKey :: String }
  }

initializeGitHub :: Aff (Maybe GitHubEventEnv)
initializeGitHub = do
  eventPath <- liftEffect do
    Process.lookupEnv "GITHUB_EVENT_PATH"
      >>= maybe (Exception.throw "GITHUB_EVENT_PATH not defined in the environment") pure

  pacchettibottiKeys <- readPacchettiBottiKeys

  token <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (Exception.throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

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
      pure $ Just { octokit, token, issue, username, operation, pacchettibottiKeys }

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

-- | Read the PacchettiBotti SSH keypair from the environment.
--
-- PacchettiBotti's keys are stored in base64-encoded strings in the
-- environment. To regenerate SSH keys for pacchettibotti:
--
-- 1. Generate the keypair
-- $ ssh-keygen -t ed25519 -C "pacchettibotti@purescript.org"
--
-- 2. Encode the keypair (run this for both public and private):
-- $ cat id_ed25519 | base64 | tr -d \\n
-- $ cat id_ed25519.pub | base64 | tr -d \\n
--
-- 3. Store the results in 1Password and in GitHub secrets storage.
readPacchettiBottiKeys :: Aff { publicKey :: String, privateKey :: String }
readPacchettiBottiKeys = do
  publicKey <- liftEffect (Process.lookupEnv "PACCHETTIBOTTI_ED25519_PUB") >>= case _ of
    Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_ED25519_PUB not defined in the environment."
    Just b64Key -> case Base64.decode b64Key of
      Left b64Error -> Aff.throwError $ Aff.error $ "Failed to decode base64-encoded public key: " <> Aff.message b64Error
      Right decoded -> case verifyPublicKey (String.trim decoded) of
        Left error -> Aff.throwError $ Aff.error $ "Public key is malformed: " <> error
        Right key -> pure key

  privateKey <- liftEffect (Process.lookupEnv "PACCHETTIBOTTI_ED25519") >>= case _ of
    Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_ED25519 not defined in the environment."
    Just b64Key -> case Base64.decode b64Key of
      Left _ -> Aff.throwError $ Aff.error $ "Failed to decode base64-encoded private key."
      Right key -> pure (String.trim key)

  pure { publicKey, privateKey }
  where
  verifyPublicKey :: String -> Either String String
  verifyPublicKey decodedKey = do
    let split = String.split (String.Pattern " ") decodedKey

    keyFields <- note "Key must be of the form 'keytype key email'" do
      keyType <- Array.index split 0
      key <- Array.index split 1
      email <- Array.index split 2
      pure { keyType, key, email }

    if keyFields.keyType /= API.pacchettiBottiKeyType then
      Left $ Array.fold [ "Key type must be ", API.pacchettiBottiKeyType, " but received ", keyFields.keyType, " instead." ]
    else if keyFields.email /= API.pacchettiBottiEmail then
      Left $ Array.fold [ "Email must be ", API.pacchettiBottiEmail, " but received: ", keyFields.email, " instead." ]
    else
      pure keyFields.key
