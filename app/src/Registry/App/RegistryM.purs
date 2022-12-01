module Registry.App.RegistryM where

import Registry.App.Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Map as Map
import Effect.Exception (Error)
import Effect.Ref as Ref
import Foreign.GitHub (Octokit)
import Registry.App.Cache (Cache)
import Registry.App.PackageStorage as PackageStorage
import Registry.Effect.Log (LOG, LOG_EXCEPT)
import Registry.Effect.Log as Log
import Run (AFF, EFFECT, Run)

type Env =
  { closeIssue :: Aff Unit
  , commitMetadataFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitIndexFile :: PackageName -> FilePath -> Aff (Either String Unit)
  , commitPackageSetFile :: Version -> String -> FilePath -> Aff (Either String Unit)
  , uploadPackage :: PackageStorage.PackageInfo -> FilePath -> Aff Unit
  , deletePackage :: PackageStorage.PackageInfo -> Aff Unit
  , octokit :: Octokit
  , username :: String
  , packagesMetadata :: Ref (Map PackageName Metadata)
  , cache :: Cache
  , registry :: FilePath
  , registryIndex :: FilePath
  }

newtype RegistryM a = RegistryM (ReaderT Env (Run RegistryEffects) a)

derive instance Newtype (RegistryM a) _

derive newtype instance Functor RegistryM
derive newtype instance Apply RegistryM
derive newtype instance Applicative RegistryM
derive newtype instance Bind RegistryM
derive newtype instance Monad RegistryM
derive newtype instance MonadEffect RegistryM
derive newtype instance MonadAff RegistryM
derive newtype instance MonadAsk Env RegistryM

instance MonadThrow Error RegistryM where
  throwError = liftAff <<< throwError

runRegistryM :: forall a. Env -> (Run RegistryEffects ~> Aff) -> RegistryM a -> Aff a
runRegistryM env handler (RegistryM m) = handler (runReaderT m env)

-- | Close the issue for the current pipeline
closeIssue :: RegistryM Unit
closeIssue = asks _.closeIssue >>= liftAff

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
uploadPackage :: PackageStorage.PackageInfo -> FilePath -> RegistryM Unit
uploadPackage pkg path = do
  f <- asks _.uploadPackage
  liftAff $ f pkg path

-- | Delete a package from the backend storage provider
deletePackage :: PackageStorage.PackageInfo -> RegistryM Unit
deletePackage pkg = do
  f <- asks _.deletePackage
  liftAff $ f pkg

-- TODO: right now we write this to file separately, but maybe it'd be better
-- to do everything here so we don't risk to forget this?
updatePackagesMetadata :: PackageName -> Metadata -> RegistryM Unit
updatePackagesMetadata pkg metadata = do
  packagesMetadata <- asks _.packagesMetadata
  liftEffect $ Ref.modify_ (Map.insert pkg metadata) packagesMetadata

readPackagesMetadata :: RegistryM (Map PackageName Metadata)
readPackagesMetadata = liftEffect <<< Ref.read =<< asks _.packagesMetadata

-- EFFECTS

type RegistryEffects = (LOG + LOG_EXCEPT + AFF + EFFECT ())

liftRun :: forall a. Run RegistryEffects a -> RegistryM a
liftRun act = RegistryM (lift act)

-- LOG effect

debug :: String -> RegistryM Unit
debug = liftRun <<< Log.debug

info :: String -> RegistryM Unit
info = liftRun <<< Log.info

warn :: String -> RegistryM Unit
warn = liftRun <<< Log.warn

error :: String -> RegistryM Unit
error = liftRun <<< Log.error

die :: forall a. String -> RegistryM a
die = liftRun <<< Log.die
