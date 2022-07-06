module Registry.RegistryM where

import Registry.Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Map as Map
import Effect.Aff (Error)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Foreign.GitHub (Octokit)
import Registry.Cache as Registry
import Registry.PackageName (PackageName)
import Registry.PackageUpload as Upload
import Registry.Schema (Metadata)

type Env =
  { comment :: String -> Aff Unit
  , closeIssue :: Aff Unit
  , commitToTrunk :: PackageName -> FilePath -> Aff (Either String Unit)
  , uploadPackage :: Upload.PackageInfo -> FilePath -> Aff Unit
  , deletePackage :: Upload.PackageInfo -> Aff Unit
  , octokit :: Octokit
  , packagesMetadata :: Ref (Map PackageName Metadata)
  , cache :: Registry.Cache
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

-- | Commit a change to the default branch of the registry repository
commitToTrunk :: PackageName -> FilePath -> RegistryM (Either String Unit)
commitToTrunk packageName path = do
  f <- asks _.commitToTrunk
  liftAff $ f packageName path

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
