module Registry.Effect.Class where

import Registry.App.Prelude

import Control.Monad.Reader (class MonadAsk, ask, asks)
import Data.Map as Map
import Effect.Ref as Ref
import Foreign.GitHub (IssueNumber, Octokit)
import Registry.App.PackageStorage as PackageStorage
import Registry.Effect.Log (class MonadLog)

-- This is a temporary measure: the members of this class should be implemented
-- as their own effects later and there's no need for this class other than the
-- convenience of not having to write out the specific effects you're using.
class (MonadLog m, MonadAff m) <= MonadRegistry m where
  commitMetadataFile :: PackageName -> m (Either String Unit)
  commitIndexFile :: PackageName -> m (Either String Unit)
  commitPackageSetFile :: Version -> String -> m (Either String Unit)
  uploadPackage :: PackageStorage.PackageInfo -> FilePath -> m Unit
  deletePackage :: PackageStorage.PackageInfo -> m Unit
  readPackagesMetadata :: m (Map PackageName Metadata)
  updatePackagesMetadata :: PackageName -> Metadata -> m Unit

type GitHubEnv r =
  ( closeIssue :: Aff Unit
  , octokit :: Octokit
  , username :: String
  , issue :: IssueNumber
  | r
  )

type HandlersEnv r =
  ( commitMetadataFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitIndexFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitPackageSetFile :: Version -> String -> FilePath -> Aff (Either String Unit)
  , uploadPackage :: PackageStorage.PackageInfo -> FilePath -> Aff Unit
  , deletePackage :: PackageStorage.PackageInfo -> Aff Unit
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
handleUploadPackage :: forall m r. MonadAsk { | HandlersEnv r } m => MonadAff m => PackageStorage.PackageInfo -> FilePath -> m Unit
handleUploadPackage pkg path = do
  f <- asks _.uploadPackage
  liftAff $ f pkg path

-- | Delete a package from the backend storage provider
handleDeletePackage :: forall r m. MonadAsk { | HandlersEnv r } m => MonadAff m => PackageStorage.PackageInfo -> m Unit
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
