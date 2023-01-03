-- | An effect for reading and writing to the registry storage backend.
module Registry.App.Monad.Storage where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.Exists as Exists
import Data.HTTP.Method (Method(..))
import Data.String as String
import Effect.Aff as Aff
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Registry.App.Monad.Cache (class FsEncodable, class MonadCache, FsEncoding(..))
import Registry.App.Monad.Cache as Cache
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
import Registry.Constants as Constants
import Registry.Foreign.S3 as S3
import Registry.PackageName as PackageName
import Registry.Version as Version

-- | A key type for the storage cache. Only supports packages identified by
-- | their name and version.
data StorageCache (c :: Type -> Type -> Type) a = Package PackageName Version (c Buffer a)

instance FsEncodable StorageCache where
  encodeFs = case _ of
    Package name version k ->
      Exists.mkExists $ AsBuffer (PackageName.print name <> "-" <> Version.print version) k

-- | The Storage effect, which describes uploading, downloading, and deleting
-- | tarballs from the registry storage backend.
class Monad m <= MonadStorage m where
  upload :: PackageName -> Version -> FilePath -> m (Either String Unit)
  download :: PackageName -> Version -> FilePath -> m (Either String Unit)
  delete :: PackageName -> Version -> m (Either String Unit)

instance MonadStorage m => MonadStorage (ExceptT e m) where
  upload name version = lift <<< upload name version
  download name version = lift <<< download name version
  delete name = lift <<< delete name

formatPackagePath :: PackageName -> Version -> String
formatPackagePath name version = Array.fold
  [ PackageName.print name
  , "/"
  , Version.print version
  , ".tar.gz"
  ]

formatPackageUrl :: PackageName -> Version -> Affjax.Node.URL
formatPackageUrl name version = Constants.storageUrl <> "/" <> formatPackagePath name version

connectS3 :: forall m. MonadLog m => MonadAff m => S3.SpaceKey -> m (Either String S3.Space)
connectS3 key = do
  let bucket = "purescript-registry"
  let space = "ams3.digitaloceanspaces.com"
  Log.debug $ "Connecting to the bucket " <> bucket <> " at space " <> space <> " with public key " <> key.key
  liftAff (withBackoff' (Aff.attempt (S3.connect key "ams3.digitaloceanspaces.com" bucket))) >>= case _ of
    Nothing -> do
      Log.error "Timed out when attempting to connect to S3"
      pure $ Left "Could not connect to storage backend."
    Just (Left err) -> do
      Log.error $ "Failed to connect to S3 due to an exception: " <> Aff.message err
      pure $ Left "Could not connect to storage backend."
    Just (Right connection) -> do
      Log.debug "Connected to S3!"
      pure $ Right connection

type S3Env = S3.SpaceKey

type STORAGE r = (storage :: S3Env | r)

handleDownloadS3
  :: forall m
   . MonadCache StorageCache m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> FilePath
  -> m (Either String Unit)
handleDownloadS3 name version path = do
  let package = formatPackageVersion name version
  let packageUrl = formatPackageUrl name version
  buffer <- Cache.get (Package name version) >>= case _ of
    Nothing -> Except.runExceptT do
      Log.debug $ "Downloading " <> package <> " from " <> packageUrl
      response <- liftAff $ withBackoff' $ Affjax.Node.request $ Affjax.Node.defaultRequest
        { method = Left GET
        , responseFormat = ResponseFormat.arrayBuffer
        , url = packageUrl
        }
      -- TODO: Rely on the metadata to check the size and hash? Or do we not care
      -- for registry-internal operations?
      case response of
        Nothing -> do
          Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of a connection timeout."
          Except.throwError $ "Failed to download " <> package <> " from the storage backend."
        Just (Left error) -> do
          Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of an HTTP error: " <> Affjax.Node.printError error
          Except.throwError $ "Could not download " <> package <> " from the storage backend."
        Just (Right { status, body }) | status /= StatusCode 200 -> do
          buffer <- liftEffect $ Buffer.fromArrayBuffer body
          bodyString <- liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
          Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of a non-200 status code (" <> show status <> ") with body " <> bodyString
          Except.throwError $ "Could not download " <> package <> " from the storage backend."
        Just (Right { body }) -> do
          Log.debug $ "Successfully downloaded " <> package <> " into a buffer."
          buffer :: Buffer <- liftEffect $ Buffer.fromArrayBuffer body
          Cache.put (Package name version) buffer
          pure buffer
    Just cached ->
      pure $ Right cached

  case buffer of
    Left error -> pure $ Left error
    Right buf -> liftAff (Aff.attempt (FS.Aff.writeFile path buf)) >>= case _ of
      Left error -> do
        Log.error $ "Downloaded " <> package <> " but failed to write it to the file at path " <> path <> ":\n" <> Aff.message error
        pure $ Left $ "Could not save downloaded package " <> package <> " due to an internal error."
      Right _ -> pure $ Right unit

handleUploadS3
  :: forall m r
   . MonadAsk { | STORAGE + r } m
  => MonadCache StorageCache m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> FilePath
  -> m (Either String Unit)
handleUploadS3 name version path = Except.runExceptT do
  env <- asks _.storage
  let
    package = formatPackageVersion name version
    packagePath = formatPackagePath name version

  buffer <- liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
    Left error -> do
      Log.error $ "Failed to read contents of " <> package <> " at path " <> path <> ": " <> Aff.message error
      Except.throwError $ "Could not upload package " <> package <> " due to a file system error."
    Right buf ->
      pure buf

  Log.debug $ "Read file for " <> package <> ", now uploading to " <> packagePath <> "..."
  s3 <- Except.ExceptT $ connectS3 env
  published <- liftAff (withBackoff' (S3.listObjects s3 { prefix: PackageName.print name <> "/" })) >>= case _ of
    Nothing -> do
      Log.error $ "Failed to list S3 objects for " <> PackageName.print name <> " because the process timed out."
      Except.throwError $ "Could not upload package " <> package <> " due to an error connecting to the storage backend."
    Just objects ->
      pure $ map _.key objects

  if Array.elem packagePath published then do
    Log.error $ packagePath <> " already exists on S3."
    Except.throwError $ "Could not upload " <> package <> " because a package at " <> formatPackageUrl name version <> " already exists."
  else do
    Log.debug $ "Uploading release to the bucket at path " <> packagePath
    let putParams = { key: packagePath, body: buffer, acl: S3.PublicRead }
    liftAff (withBackoff' (S3.putObject s3 putParams)) >>= case _ of
      Nothing -> do
        Log.error "Failed to put object to S3 because the process timed out."
        Except.throwError $ "Could not upload package " <> package <> " due to an error connecting to the storage backend."
      Just _ -> do
        Log.info $ "Uploaded " <> package <> " to the bucket at path " <> packagePath
        pure unit

handleDeleteS3
  :: forall m r
   . MonadAsk { | STORAGE + r } m
  => MonadCache StorageCache m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> m (Either String Unit)
handleDeleteS3 name version = Except.runExceptT do
  env <- asks _.storage
  let
    package = formatPackageVersion name version
    packagePath = formatPackagePath name version

  Log.debug $ "Deleting " <> package
  s3 <- Except.ExceptT $ connectS3 env
  published <- liftAff (withBackoff' (S3.listObjects s3 { prefix: PackageName.print name <> "/" })) >>= case _ of
    Nothing -> do
      Log.error $ "Failed to delete " <> package <> " because the process timed out when attempting to list objects at " <> packagePath <> " from S3."
      Except.throwError $ "Could not delete " <> package <> " from the storage backend."
    Just objects ->
      pure $ map _.key objects

  if Array.elem packagePath published then do
    Log.debug $ "Deleting release from the bucket at path " <> packagePath
    let deleteParams = { key: packagePath }
    liftAff (withBackoff' (S3.deleteObject s3 deleteParams)) >>= case _ of
      Nothing -> do
        Log.error $ "Timed out when attempting to delete the release of " <> package <> " from S3 at the path " <> packagePath
        Except.throwError $ "Could not delete " <> package <> " from the storage backend."
      Just _ -> do
        Log.debug $ "Deleted release of " <> package <> " from S3 at the path " <> packagePath
  else do
    Log.error $ packagePath <> " does not exist on S3 (available: " <> String.joinWith ", " published <> ")"
    Except.throwError $ "Could not delete " <> package <> " because it does not exist in the storage backend."

  Cache.delete (Package name version)
