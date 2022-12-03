module Registry.App.RegistryM where

import Registry.App.Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Foreign.GitHub (IssueNumber, Octokit)
import Registry.App.Cache (Cache)
import Registry.Effect.Class (class MonadRegistry, HandlersEnv, MetadataEnv, RepoEnv)
import Registry.Effect.Class as Registry
import Registry.Effect.Log (class MonadLog, runLogFile, runLogGitHub)

type GitHubEnv r =
  { closeIssue :: Aff Unit
  , octokit :: Octokit
  , username :: String
  , issue :: IssueNumber
  , cache :: Cache
  | HandlersEnv + MetadataEnv + RepoEnv r
  }

-- | Close the issue for the current pipeline
closeIssue :: forall m r. MonadAsk { closeIssue :: Aff Unit | r } m => MonadRegistry m => m Unit
closeIssue = asks _.closeIssue >>= liftAff

-- | A monad for running `MonadRegistry` in the GitHub environment.
newtype GitHubM a = GitHubM (ReaderT (GitHubEnv ()) Aff a)

derive instance Newtype (GitHubM a) _

derive newtype instance Functor GitHubM
derive newtype instance Apply GitHubM
derive newtype instance Applicative GitHubM
derive newtype instance Bind GitHubM
derive newtype instance Monad GitHubM
derive newtype instance MonadEffect GitHubM
derive newtype instance MonadAff GitHubM
derive newtype instance MonadAsk (GitHubEnv ()) GitHubM

instance MonadLog GitHubM where
  log level message = do
    { octokit, issue } <- ask
    runLogGitHub octokit issue level message

instance MonadRegistry GitHubM where
  commitMetadataFile = Registry.handleCommitMetadataFile
  commitIndexFile = Registry.handleCommitIndexFile
  commitPackageSetFile = Registry.handleCommitPackageSetFile
  uploadPackage = Registry.handleUploadPackage
  deletePackage = Registry.handleDeletePackage
  readPackagesMetadata = Registry.handleReadPackagesMetadata
  updatePackagesMetadata = Registry.handleUpdatePackagesMetadata

runGitHubM :: forall a. GitHubEnv () -> GitHubM a -> Aff a
runGitHubM env (GitHubM m) = runReaderT m env

-- Until we can separate out the GitHub effects, we have to include them even
-- though they should never be used in the local run.
type LocalEnv = GitHubEnv (logfile :: FilePath)

-- | A monad for running `MonadRegistry` locally, without access to a GitHub
-- | issue or event.
newtype LocalM a = LocalM (ReaderT LocalEnv Aff a)

derive instance Newtype (LocalM a) _

derive newtype instance Functor LocalM
derive newtype instance Apply LocalM
derive newtype instance Applicative LocalM
derive newtype instance Bind LocalM
derive newtype instance Monad LocalM
derive newtype instance MonadEffect LocalM
derive newtype instance MonadAff LocalM
derive newtype instance MonadAsk LocalEnv LocalM

instance MonadLog LocalM where
  log level message = ask >>= \{ logfile } -> runLogFile logfile level message

instance MonadRegistry LocalM where
  commitMetadataFile = Registry.handleCommitMetadataFile
  commitIndexFile = Registry.handleCommitIndexFile
  commitPackageSetFile = Registry.handleCommitPackageSetFile
  uploadPackage = Registry.handleUploadPackage
  deletePackage = Registry.handleDeletePackage
  readPackagesMetadata = Registry.handleReadPackagesMetadata
  updatePackagesMetadata = Registry.handleUpdatePackagesMetadata

runLocalM :: forall a. LocalEnv -> LocalM a -> Aff a
runLocalM env (LocalM m) = runReaderT m env
