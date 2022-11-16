module Registry.RegistryM where

import Registry.App.Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Map as Map
import Effect.Aff (Error)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Foreign.GitHub (Octokit)
import Registry.App.Cache (Cache)
import Registry.App.PackageStorage as Upload
import Registry.Metadata (Metadata)
import Registry.PackageName (PackageName)
import Registry.Version (Version)

type Env =
  { comment :: String -> Aff Unit
  , closeIssue :: Aff Unit
  , commitMetadataFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitIndexFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitPackageSetFile :: Version -> String -> FilePath -> Aff (Either String Unit)
  , uploadPackage :: Upload.PackageInfo -> FilePath -> Aff Unit
  , deletePackage :: Upload.PackageInfo -> Aff Unit
  , octokit :: Octokit
  , username :: String
  , packagesMetadata :: Ref (Map PackageName Metadata)
  , cache :: Cache
  , registry :: FilePath
  , registryIndex :: FilePath
  }

newtype RegistryM a = RegistryM (ReaderT Env Aff a)

derive instance Newtype (RegistryM a) _

derive newtype instance Functor RegistryM
derive newtype instance Apply RegistryM
derive newtype instance Applicative RegistryM
derive newtype instance Bind RegistryM
derive newtype instance Monad RegistryM
derive newtype instance MonadEffect RegistryM
derive newtype instance MonadAff RegistryM
derive newtype instance MonadThrow Error RegistryM
derive newtype instance MonadAsk Env RegistryM

runRegistryM :: forall a. Env -> RegistryM a -> Aff a
runRegistryM env (RegistryM m) = runReaderT m env

-- | Post a comment to the user's issue
comment :: String -> RegistryM Unit
comment body = do
  f <- asks _.comment
  liftAff $ f body

-- | Close the issue for the current pipeline
closeIssue :: RegistryM Unit
closeIssue = asks _.closeIssue >>= liftAff

-- | Post an error to the user's issue and then throw an exception
throwWithComment :: forall a. String -> RegistryM a
throwWithComment body = comment body *> Aff.throwError (Aff.error body)

commitMetadataFile :: PackageName -> RegistryM (Either String Unit)
commitMetadataFile packageName = do
  env <- ask
  liftAff $ env.commitMetadataFile packageName env.registry

commitIndexFile :: PackageName -> RegistryM (Either String Unit)
commitIndexFile packageName = do
  env <- ask
  liftAff $ env.commitIndexFile packageName env.registryIndex

commitPackageSetFile :: Version -> String -> RegistryM (Either String Unit)
commitPackageSetFile version commitMessage = do
  env <- ask
  liftAff $ env.commitPackageSetFile version commitMessage env.registry

-- | Upload a package to the backend storage provider
uploadPackage :: Upload.PackageInfo -> FilePath -> RegistryM Unit
uploadPackage info path = do
  f <- asks _.uploadPackage
  liftAff $ f info path

-- | Delete a package from the backend storage provider
deletePackage :: Upload.PackageInfo -> RegistryM Unit
deletePackage info = do
  f <- asks _.deletePackage
  liftAff $ f info

-- TODO: right now we write this to file separately, but maybe it'd be better
-- to do everything here so we don't risk to forget this?
updatePackagesMetadata :: PackageName -> Metadata -> RegistryM Unit
updatePackagesMetadata pkg metadata = do
  packagesMetadata <- asks _.packagesMetadata
  liftEffect $ Ref.modify_ (Map.insert pkg metadata) packagesMetadata

readPackagesMetadata :: RegistryM (Map PackageName Metadata)
readPackagesMetadata = liftEffect <<< Ref.read =<< asks _.packagesMetadata
