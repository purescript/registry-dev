-- | An effect for reading common data from an environment.
module Registry.App.Effect.Env where

import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Data.String.Base64 as Base64
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception as Exn
import Node.FS.Aff as FS.Aff
import Node.Process as Process
import Registry.Foreign.Octokit (GitHubToken(..), IssueNumber)
import Run (Run)
import Run.Reader (Reader)
import Run.Reader as Run.Reader

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

-- | Environment fields available when the process provides @pacchettibotti
-- | credentials for sensitive authorized actions.
type PacchettiBottiEnv =
  { publicKey :: String
  , privateKey :: String
  , token :: GitHubToken
  }

type PACCHETTIBOTTI_ENV r = (pacchettiBottiEnv :: Reader PacchettiBottiEnv | r)

_pacchettiBottiEnv :: Proxy "pacchettiBottiEnv"
_pacchettiBottiEnv = Proxy

askPacchettiBotti :: forall r. Run (PACCHETTIBOTTI_ENV + r) PacchettiBottiEnv
askPacchettiBotti = Run.Reader.askAt _pacchettiBottiEnv

-- | Variables that can be set in the environment.
type EnvVars =
  { githubToken :: Maybe GitHubToken
  , spacesKey :: Maybe String
  , spacesSecret :: Maybe String
  , pacchettibottiToken :: Maybe GitHubToken
  , pacchettibottiED25519Pub :: Maybe String
  , pacchettibottiED25519 :: Maybe String
  }

-- | Loads the environment, using a `.env` file when suitable.
loadEnvFile :: FilePath -> Aff Unit
loadEnvFile dotenv = do
  contents <- Aff.attempt $ FS.Aff.readTextFile UTF8 dotenv
  case contents of
    Left _ -> Aff.throwError $ Aff.error $ "No .env file found at path " <> dotenv
    Right string -> void $ Dotenv.loadContents (String.trim string)

readEnvVars :: Effect EnvVars
readEnvVars = do
  spacesKey <- Process.lookupEnv "SPACES_KEY"
  spacesSecret <- Process.lookupEnv "SPACES_SECRET"

  let
    lookupToken key = do
      mbToken <- Process.lookupEnv key
      for mbToken \token -> case String.stripPrefix (String.Pattern "ghp_") token of
        Nothing -> Exn.throw $ "GitHub tokens begin with ghp_, but value found for env var " <> key <> " does not: " <> token
        Just _ -> pure $ GitHubToken token

  githubToken <- lookupToken "GITHUB_TOKEN"
  pacchettibottiToken <- lookupToken "PACCHETTIBOTTI_TOKEN"

  let
    lookupBase64Key key = do
      mbB64Key <- Process.lookupEnv key
      for mbB64Key \b64Key -> case Base64.decode b64Key of
        Left b64Error -> Exn.throw $ "Failed to decode base64-encoded key: " <> Aff.message b64Error
        Right decoded -> pure (String.trim decoded)

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
  pacchettibottiED25519 <- lookupBase64Key "PACCHETTIBOTTI_ED25519"
  pacchettibottiED25519Pub <- do
    mbKey <- lookupBase64Key "PACCHETTIBOTTI_ED25519_PUB"
    for mbKey \key -> case verifyPacchettiBottiPublicKey key of
      Left error -> Exn.throw error
      Right verified -> pure verified

  pure
    { githubToken
    , spacesKey
    , spacesSecret
    , pacchettibottiToken
    , pacchettibottiED25519Pub
    , pacchettibottiED25519
    }
  where
  verifyPacchettiBottiPublicKey :: String -> Either String String
  verifyPacchettiBottiPublicKey decodedKey = do
    let split = String.split (String.Pattern " ") decodedKey

    keyFields <- note "Key must be of the form 'keytype key email'" do
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

pacchettibottiKeyType :: String
pacchettibottiKeyType = "ssh-ed25519"

pacchettibottiEmail :: String
pacchettibottiEmail = "pacchettibotti@purescript.org"
