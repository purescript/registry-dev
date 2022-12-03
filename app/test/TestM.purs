module Test.TestM where

import Registry.App.Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Map as Map
import Effect.Exception (Error)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..), IssueNumber(..), mkOctokit)
import Registry.App.Monad (class MonadRegistry, GitHubEnv)
import Registry.App.Monad as App
import Registry.Effect.Log (class MonadLog, runLogConsole)

-- TODO: Once we have actually separated out the effects, this doesn't need any
-- of the GitHub stuff.
type TestEnv = GitHubEnv ()

newtype TestM a = TestM (ReaderT TestEnv Aff a)

derive instance Newtype (TestM a) _

derive newtype instance Functor TestM
derive newtype instance Apply TestM
derive newtype instance Applicative TestM
derive newtype instance Bind TestM
derive newtype instance Monad TestM
derive newtype instance MonadEffect TestM
derive newtype instance MonadAff TestM
derive newtype instance MonadAsk TestEnv TestM
derive newtype instance MonadThrow Error TestM

instance MonadLog TestM where
  log = runLogConsole

instance MonadRegistry TestM where
  commitMetadataFile = App.handleCommitMetadataFile
  commitIndexFile = App.handleCommitIndexFile
  commitPackageSetFile = App.handleCommitPackageSetFile
  uploadPackage = App.handleUploadPackage
  deletePackage = App.handleDeletePackage
  updatePackagesMetadata = App.handleUpdatePackagesMetadata
  readPackagesMetadata = App.handleReadPackagesMetadata

runTestM :: TestEnv -> TestM ~> Aff
runTestM env (TestM m) = runReaderT m env

defaultTestEnv :: TestEnv
defaultTestEnv =
  { registry: ""
  , registryIndex: ""
  , commitMetadataFile: \_ _ -> pure (Right unit)
  , commitIndexFile: \_ _ -> pure (Right unit)
  , commitPackageSetFile: \_ _ _ -> pure (Right unit)
  , deletePackage: mempty
  , uploadPackage: mempty
  , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
  , cache: { read: \_ -> pure (Left ""), write: mempty, remove: mempty }

  , closeIssue: mempty
  , octokit: unsafePerformEffect (mkOctokit (GitHubToken ""))
  , username: ""
  , issue: IssueNumber 0
  }
