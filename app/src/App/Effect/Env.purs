-- | An effect for reading common data from an environment.
module Registry.App.Effect.Env where

import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Data.String.Base64 as Base64
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception as Exception
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.Constants as Constants
import Registry.Foreign.Octokit (GitHubToken(..), IssueNumber)
import Run (Run)
import Run.Reader (Reader)
import Run.Reader as Run.Reader

type ResourceEnv =
  { dhallTypes :: FilePath
  , databaseUrl :: DatabaseUrl
  , s3ApiUrl :: URL
  , s3BucketUrl :: URL
  , githubApiUrl :: URL
  , pursuitApiUrl :: URL
  , healthchecksUrl :: URL
  }

-- | An effect for various external resources (files, databases, API endpoints,
-- | and so on) that the registry needs to connect to.
type RESOURCE_ENV r = (resourceEnv :: Reader ResourceEnv | r)

_resourceEnv :: Proxy "resourceEnv"
_resourceEnv = Proxy

askResourceEnv :: forall r. Run (RESOURCE_ENV + r) ResourceEnv
askResourceEnv = Run.Reader.askAt _resourceEnv

runResourceEnv :: forall r a. ResourceEnv -> Run (RESOURCE_ENV + r) a -> Run r a
runResourceEnv = Run.Reader.runReaderAt _resourceEnv

lookupResourceEnv :: forall m. MonadEffect m => m ResourceEnv
lookupResourceEnv = do
  dhallTypesEnv <- liftEffect <<< Path.resolve [] =<< lookupWithDefault dhallTypes defaultDhallTypes
  databaseUrlEnv <- lookupWithDefault databaseUrl defaultDatabaseUrl
  s3ApiUrlEnv <- lookupWithDefault s3ApiUrl defaultS3ApiUrl
  s3BucketUrlEnv <- lookupWithDefault s3BucketUrl defaultS3BucketUrl
  githubApiUrlEnv <- lookupWithDefault githubApiUrl defaultGitHubApiUrl
  pursuitApiUrlEnv <- lookupWithDefault pursuitApiUrl defaultPursuitApiUrl
  healthchecksUrlEnv <- lookupRequired healthchecksUrl
  pure
    { dhallTypes: dhallTypesEnv
    , databaseUrl: databaseUrlEnv
    , s3ApiUrl: s3ApiUrlEnv
    , s3BucketUrl: s3BucketUrlEnv
    , githubApiUrl: githubApiUrlEnv
    , pursuitApiUrl: pursuitApiUrlEnv
    , healthchecksUrl: healthchecksUrlEnv
    }

-- | Environment fields available in the GitHub Event environment, namely
-- | pointers to the user who created the event and the issue associated with it.
type GitHubEventEnv =
  { username :: String
  , issue :: IssueNumber
  }

type GITHUB_EVENT_ENV r = (githubEventEnv :: Reader GitHubEventEnv | r)

_githubEventEnv :: Proxy "githubEventEnv"
_githubEventEnv = Proxy

askGitHubEvent :: forall r. Run (GITHUB_EVENT_ENV + r) GitHubEventEnv
askGitHubEvent = Run.Reader.askAt _githubEventEnv

runGitHubEventEnv :: forall r a. GitHubEventEnv -> Run (GITHUB_EVENT_ENV + r) a -> Run r a
runGitHubEventEnv = Run.Reader.runReaderAt _githubEventEnv

-- | Environment fields available when the process provides @pacchettibotti
-- | credentials for sensitive authorized actions.
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
type PacchettiBottiEnv =
  { publicKey :: String
  , privateKey :: String
  }

type PACCHETTIBOTTI_ENV r = (pacchettiBottiEnv :: Reader PacchettiBottiEnv | r)

_pacchettiBottiEnv :: Proxy "pacchettiBottiEnv"
_pacchettiBottiEnv = Proxy

askPacchettiBotti :: forall r. Run (PACCHETTIBOTTI_ENV + r) PacchettiBottiEnv
askPacchettiBotti = Run.Reader.askAt _pacchettiBottiEnv

runPacchettiBottiEnv :: forall r a. PacchettiBottiEnv -> Run (PACCHETTIBOTTI_ENV + r) a -> Run r a
runPacchettiBottiEnv = Run.Reader.runReaderAt _pacchettiBottiEnv

-- ENV VARS

-- | Loads the environment from a .env file, if one exists.
loadEnvFile :: FilePath -> Aff Unit
loadEnvFile dotenv = do
  contents <- Aff.attempt $ FS.Aff.readTextFile UTF8 dotenv
  case contents of
    Left _ -> pure unit
    Right string -> void $ Dotenv.loadContents (String.trim string)

-- | An environment key the registry is aware of.
newtype EnvKey a = EnvKey { key :: String, decode :: String -> Either String a }

printEnvKey :: forall a. EnvKey a -> String
printEnvKey (EnvKey { key }) = key

-- | Look up an optional environment variable, throwing an exception if it is
-- | present but cannot be decoded. Empty strings are considered missing values.
lookupOptional :: forall m a. MonadEffect m => EnvKey a -> m (Maybe a)
lookupOptional (EnvKey { key, decode }) = liftEffect $ Process.lookupEnv key >>= case _ of
  Nothing -> pure Nothing
  Just "" -> pure Nothing
  Just value -> case decode value of
    Left error -> Exception.throw $ "Found " <> key <> " in the environment with value " <> value <> ", but it could not be decoded: " <> error
    Right decoded -> pure $ Just decoded

-- | Look up an optional environment variable, throwing an exception if it is
-- | present but cannot be decoded, and returning a default value if missing or
-- | empty.
lookupWithDefault :: forall m a. MonadEffect m => EnvKey a -> a -> m a
lookupWithDefault (EnvKey { key, decode }) default = liftEffect $ Process.lookupEnv key >>= case _ of
  Nothing -> pure default
  Just "" -> pure default
  Just value -> case decode value of
    Left error -> Exception.throw $ "Found " <> key <> " in the environment with value " <> value <> ", but it could not be decoded: " <> error
    Right decoded -> pure decoded

-- | Look up a required environment variable, throwing an exception if it is
-- | missing, an empty string, or present but cannot be decoded.
lookupRequired :: forall m a. MonadEffect m => EnvKey a -> m a
lookupRequired (EnvKey { key, decode }) = liftEffect $ Process.lookupEnv key >>= case _ of
  Nothing -> Exception.throw $ key <> " is not present in the environment."
  Just "" -> Exception.throw $ "Found " <> key <> " in the environment, but its value was an empty string."
  Just value -> case decode value of
    Left error -> Exception.throw $ "Found " <> key <> " in the environment with value " <> value <> ", but it could not be decoded: " <> error
    Right decoded -> pure decoded

-- | A user GitHub token at the GITHUB_TOKEN key.
githubToken :: EnvKey GitHubToken
githubToken = EnvKey { key: "GITHUB_TOKEN", decode: decodeGitHubToken }

-- | A public key for the S3 account at the SPACES_KEY key.
spacesKey :: EnvKey String
spacesKey = EnvKey { key: "SPACES_KEY", decode: pure }

-- | A secret key for the S3 account at the SPACES_SECRET key.
spacesSecret :: EnvKey String
spacesSecret = EnvKey { key: "SPACES_SECRET", decode: pure }

type DatabaseUrl = { prefix :: String, path :: FilePath }

-- | The location of the sqlite database
databaseUrl :: EnvKey DatabaseUrl
databaseUrl = EnvKey { key: "DATABASE_URL", decode: decodeDatabaseUrl }

defaultDatabaseUrl :: DatabaseUrl
defaultDatabaseUrl = { prefix: "sqlite:", path: "db/registry.sqlite3" }

-- | The location of the Dhall specifications directory
dhallTypes :: EnvKey FilePath
dhallTypes = EnvKey { key: "DHALL_TYPES", decode: pure }

defaultDhallTypes :: FilePath
defaultDhallTypes = "./types"

-- | The base URL of the S3 API
s3ApiUrl :: EnvKey URL
s3ApiUrl = EnvKey { key: "S3_API_URL", decode: pure }

defaultS3ApiUrl :: URL
defaultS3ApiUrl = Constants.storageUrl

-- | The base URL of the S3 API
s3BucketUrl :: EnvKey URL
s3BucketUrl = EnvKey { key: "S3_BUCKET_URL", decode: pure }

defaultS3BucketUrl :: URL
defaultS3BucketUrl = "https://ams3.digitaloceanspaces.com"

-- | The base URL of the GitHub API
githubApiUrl :: EnvKey URL
githubApiUrl = EnvKey { key: "GITHUB_API_URL", decode: pure }

defaultGitHubApiUrl :: URL
defaultGitHubApiUrl = "https://api.github.com"

-- | The base URL of the Pursuit API
pursuitApiUrl :: EnvKey URL
pursuitApiUrl = EnvKey { key: "PURSUIT_API_URL", decode: pure }

defaultPursuitApiUrl :: URL
defaultPursuitApiUrl = "https://pursuit.purescript.org"

-- | The URL of the health checks endpoint
healthchecksUrl :: EnvKey URL
healthchecksUrl = EnvKey { key: "HEALTHCHECKS_URL", decode: pure }

-- | A GitHub token for the @pacchettibotti user at the PACCHETTIBOTTI_TOKEN key.
pacchettibottiToken :: EnvKey GitHubToken
pacchettibottiToken = EnvKey { key: "PACCHETTIBOTTI_TOKEN", decode: decodeGitHubToken }

-- | A base64-encoded ED25519 SSH private key for the @pacchettibotti user at the
-- | PACCHETTIBOTTI_ED25519 key.
pacchettibottiED25519 :: EnvKey String
pacchettibottiED25519 = EnvKey { key: "PACCHETTIBOTTI_ED25519", decode: decodeBase64Key }

-- | A base64-encoded ED25519 SSH public key for the @pacchettibotti user at the
-- | PACCHETTIBOTTI_ED25519_PUB key.
pacchettibottiED25519Pub :: EnvKey String
pacchettibottiED25519Pub = EnvKey
  { key: "PACCHETTIBOTTI_ED25519_PUB"
  , decode: \input -> do
      decoded <- decodeBase64Key input
      let split = String.split (String.Pattern " ") decoded

      keyFields <- note "Key must be of the form 'keytype key comment'" do
        keyType <- Array.index split 0
        key <- Array.index split 1
        email <- Array.index split 2
        pure { keyType, key, email }

      if keyFields.keyType /= pacchettibottiKeyType then
        Left $ Array.fold [ "Key type must be ", pacchettibottiKeyType, " but received ", keyFields.keyType, " instead." ]
      else if keyFields.email /= pacchettibottiEmail then
        Left $ Array.fold [ "Email must be ", pacchettibottiEmail, " but received: ", keyFields.email, " instead." ]
      else
        pure keyFields.key
  }

-- | A file path to the JSON payload describing the triggered GitHub event.
githubEventPath :: EnvKey FilePath
githubEventPath = EnvKey { key: "GITHUB_EVENT_PATH", decode: pure }

decodeDatabaseUrl :: String -> Either String DatabaseUrl
decodeDatabaseUrl input = do
  let prefix = "sqlite:"
  case String.stripPrefix (String.Pattern prefix) input of
    Nothing -> Left $ "Database URL must begin with 'sqlite:' but the input does not: " <> input
    Just path -> pure { prefix, path }

decodeGitHubToken :: String -> Either String GitHubToken
decodeGitHubToken input = case String.stripPrefix (String.Pattern "ghp_") input of
  Nothing -> Left $ "GitHub tokens begin with ghp_, but the input does not: " <> input
  Just _ -> pure $ GitHubToken input

decodeBase64Key :: String -> Either String String
decodeBase64Key b64Key = case Base64.decode b64Key of
  Left b64Error -> Left $ "Failed to decode base64-encoded key " <> b64Key <> " with error: " <> Aff.message b64Error
  Right decoded -> Right $ String.trim decoded
