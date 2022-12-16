-- | An effect for reading common data from an environment.
module Registry.App.Effect.Env where

import Registry.Foreign.Octokit (GitHubToken, IssueNumber)
import Run (Run)
import Run.Reader (Reader)
import Run.Reader as Run.Reader
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

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
