module Registry.App.Monad where

import Registry.App.Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Map as Map
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.GitHub (IssueNumber, Octokit)
import Registry.App.Cache (Cache)
import Registry.App.PackageStorage (PackageInfo)
import Registry.Effect.Log (class MonadLog, LogVerbosity(..))
import Registry.Effect.Log as Log
import Registry.Effect.Notify (class MonadNotify)
import Registry.Effect.Notify as Notify

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
  log level message = Log.runLogTerminal { verbosity: Verbose } level message

instance MonadNotify GitHubM where
  notify message = do
    { octokit, issue } <- ask
    Notify.runNotifyGitHub { octokit, issue } message

  exit message = do
    { octokit, issue } <- ask
    Notify.runExitGitHub { octokit, issue } message

instance MonadRegistry GitHubM where
  commitMetadataFile = handleCommitMetadataFile
  commitIndexFile = handleCommitIndexFile
  commitPackageSetFile = handleCommitPackageSetFile
  uploadPackage = handleUploadPackage
  deletePackage = handleDeletePackage
  readPackagesMetadata = handleReadPackagesMetadata
  updatePackagesMetadata = handleUpdatePackagesMetadata

runGitHubM :: forall a. GitHubEnv () -> GitHubM a -> Aff a
runGitHubM env (GitHubM m) = runReaderT m env

-- Until we can separate out the GitHub effects, we have to include them even
-- though they should never be used in the local run.
type LocalEnv = GitHubEnv (logfile :: FilePath, verbosity :: LogVerbosity)

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

instance MonadThrow String LocalM where
  throwError = liftEffect <<< Exception.throw

instance MonadLog LocalM where
  log level message = do
    { logfile, verbosity } <- ask
    Log.runLogFs { logfile, verbosity } level message

instance MonadNotify LocalM where
  notify _ = pure unit
  exit message = throwError (?a message)

instance MonadRegistry LocalM where
  commitMetadataFile = handleCommitMetadataFile
  commitIndexFile = handleCommitIndexFile
  commitPackageSetFile = handleCommitPackageSetFile
  uploadPackage = handleUploadPackage
  deletePackage = handleDeletePackage
  readPackagesMetadata = handleReadPackagesMetadata
  updatePackagesMetadata = handleUpdatePackagesMetadata

runLocalM :: forall a. LocalEnv -> LocalM a -> Aff a
runLocalM env (LocalM m) = runReaderT m env

-- This is a temporary measure: the members of this class should be implemented
-- as their own effects later and there's no need for this class other than the
-- convenience of not having to write out the specific effects you're using.
class (MonadNotify m, MonadLog m, MonadAff m) <= MonadRegistry m where
  commitMetadataFile :: PackageName -> m (Either String Unit)
  commitIndexFile :: PackageName -> m (Either String Unit)
  commitPackageSetFile :: Version -> String -> m (Either String Unit)
  uploadPackage :: PackageInfo -> FilePath -> m Unit
  deletePackage :: PackageInfo -> m Unit
  readPackagesMetadata :: m (Map PackageName Metadata)
  updatePackagesMetadata :: PackageName -> Metadata -> m Unit

type HandlersEnv r =
  ( commitMetadataFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitIndexFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitPackageSetFile :: Version -> String -> FilePath -> Aff (Either String Unit)
  , uploadPackage :: PackageInfo -> FilePath -> Aff Unit
  , deletePackage :: PackageInfo -> Aff Unit
  | r
  )

type MetadataEnv r =
  ( packagesMetadata :: Ref (Map PackageName Metadata)
  | r
  )

type RepoEnv r =
  ( registry :: FilePath
  , registryIndex :: FilePath
  | r
  )

handleCommitMetadataFile :: forall m r. MonadAsk { | HandlersEnv + RepoEnv r } m => MonadAff m => PackageName -> m (Either String Unit)
handleCommitMetadataFile packageName = do
  env <- ask
  liftAff $ env.commitMetadataFile packageName env.registry

handleCommitIndexFile :: forall m r. MonadAsk { | HandlersEnv + RepoEnv r } m => MonadAff m => PackageName -> m (Either String Unit)
handleCommitIndexFile packageName = do
  env <- ask
  liftAff $ env.commitIndexFile packageName env.registryIndex

handleCommitPackageSetFile :: forall m r. MonadAsk { | HandlersEnv + RepoEnv r } m => MonadAff m => Version -> String -> m (Either String Unit)
handleCommitPackageSetFile version commitMessage = do
  env <- ask
  liftAff $ env.commitPackageSetFile version commitMessage env.registry

-- | Upload a package to the backend storage provider
handleUploadPackage :: forall m r. MonadAsk { | HandlersEnv r } m => MonadAff m => PackageInfo -> FilePath -> m Unit
handleUploadPackage pkg path = do
  f <- asks _.uploadPackage
  liftAff $ f pkg path

-- | Delete a package from the backend storage provider
handleDeletePackage :: forall r m. MonadAsk { | HandlersEnv r } m => MonadAff m => PackageInfo -> m Unit
handleDeletePackage pkg = do
  f <- asks _.deletePackage
  liftAff $ f pkg

-- TODO: right now we write this to file separately, but maybe it'd be better
-- to do everything here so we don't risk to forget this?
handleUpdatePackagesMetadata :: forall r m. MonadAsk { | MetadataEnv r } m => MonadEffect m => PackageName -> Metadata -> m Unit
handleUpdatePackagesMetadata pkg metadata = do
  packagesMetadata <- asks _.packagesMetadata
  liftEffect $ Ref.modify_ (Map.insert pkg metadata) packagesMetadata

handleReadPackagesMetadata :: forall r m. MonadAsk { | MetadataEnv r } m => MonadEffect m => m (Map PackageName Metadata)
handleReadPackagesMetadata = liftEffect <<< Ref.read =<< asks _.packagesMetadata
