-- | An effect for reading and writing to the registry storage backend.
module Registry.App.Effect.Storage
  ( STORAGE
  , STORAGE_CACHE
  , Storage
  , _storage
  , deleteTarball
  , downloadTarball
  , handleStorageReadOnly
  , handleStorageS3
  , runStorage
  , runStorageCacheFs
  , uploadTarball
  ) where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.HTTP.Method (Method(..))
import Data.String as String
import Effect.Aff as Aff
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Registry.App.Effect.Cache (Cache, CacheId(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Log (LOG, LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.Constants as Constants
import Registry.Foreign.S3 as S3
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Type.Proxy (Proxy(..))

type STORAGE_CACHE r = (storageCache :: Cache Buffer | r)

_storageCache :: Proxy "storageCache"
_storageCache = Proxy

runStorageCacheFs :: forall r a. FilePath -> Run (STORAGE_CACHE + LOG + AFF + EFFECT + r) a -> Run (LOG + AFF + EFFECT + r) a
runStorageCacheFs cacheDir = Cache.runCacheAt _storageCache (Cache.handleCacheFs { cacheDir, encoding: Cache.buffer })

data Storage a
  = Upload PackageName Version FilePath a
  | Download PackageName Version FilePath a
  | Delete PackageName Version a

derive instance Functor Storage

type STORAGE r = (storage :: Storage | r)

_storage :: Proxy "storage"
_storage = Proxy

-- | Upload a package tarball to the storage backend from the given path.
uploadTarball :: forall r. PackageName -> Version -> FilePath -> Run (STORAGE + r) Unit
uploadTarball name version file = Run.lift _storage (Upload name version file unit)

-- | Download a package tarball from the storage backend to the given path.
downloadTarball :: forall r. PackageName -> Version -> FilePath -> Run (STORAGE + r) Unit
downloadTarball name version file = Run.lift _storage (Download name version file unit)

-- | Delete a package tarball from the storage backend.
deleteTarball :: forall r. PackageName -> Version -> Run (STORAGE + r) Unit
deleteTarball name version = Run.lift _storage (Delete name version unit)

formatPackagePath :: PackageName -> Version -> String
formatPackagePath name version = Array.fold
  [ PackageName.print name
  , "/"
  , Version.print version
  , ".tar.gz"
  ]

formatPackageUrl :: PackageName -> Version -> Affjax.Node.URL
formatPackageUrl name version = Constants.storageUrl <> "/" <> formatPackagePath name version

connectS3 :: forall r. S3.SpaceKey -> Run (LOG + LOG_EXCEPT + AFF + r) S3.Space
connectS3 key = do
  let bucket = "purescript-registry"
  let space = "ams3.digitaloceanspaces.com"
  Log.debug $ "Connecting to the bucket " <> bucket <> " at space " <> space <> " with public key " <> key.key
  Run.liftAff (withBackoff' (Aff.attempt (S3.connect key "ams3.digitaloceanspaces.com" bucket))) >>= case _ of
    Nothing -> do
      Log.error "Timed out when attempting to connect to S3"
      Log.exit "Could not connect to storage backend."
    Just (Left err) -> do
      Log.error $ "Failed to connect to S3 due to an exception: " <> Aff.message err
      Log.exit "Could not connect to storage backend."
    Just (Right connection) -> do
      Log.debug "Connected to S3!"
      pure connection

-- | Interpret the STORAGE effect, given a handler.
runStorage :: forall r a. (Storage ~> Run r) -> Run (STORAGE + r) a -> Run r a
runStorage handler = Run.interpret (Run.on _storage handler Run.send)

-- | Handle package storage using a remote S3 bucket.
handleStorageS3 :: forall r a. S3.SpaceKey -> Storage a -> Run (STORAGE_CACHE + LOG + LOG_EXCEPT + AFF + EFFECT + r) a
handleStorageS3 key = case _ of
  Download name version path next -> do
    let package = formatPackageVersion name version
    let cacheId = CacheId package
    buffer <- Cache.get _storageCache cacheId >>= case _ of
      Nothing -> do
        buffer <- downloadS3 name version
        Cache.put _storageCache cacheId buffer
        pure buffer
      Just cached ->
        pure cached.value
    Run.liftAff (Aff.attempt (FS.Aff.writeFile path buffer)) >>= case _ of
      Left error -> do
        Log.error $ "Downloaded " <> package <> " but failed to write it to the file at path " <> path <> ":\n" <> Aff.message error
        Log.exit $ "Could not save downloaded package " <> package <> " due to an internal error."
      Right _ -> pure next

  Upload name version path next -> do
    let
      package = formatPackageVersion name version
      packagePath = formatPackagePath name version

    buffer <- Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left error -> do
        Log.error $ "Failed to read contents of " <> package <> " at path " <> path <> ": " <> Aff.message error
        Log.exit $ "Could not upload package " <> package <> " due to a file system error."
      Right buf ->
        pure buf

    Log.debug $ "Read file for " <> package <> ", now uploading to " <> packagePath <> "..."
    s3 <- connectS3 key
    published <- Run.liftAff (withBackoff' (S3.listObjects s3 { prefix: PackageName.print name <> "/" })) >>= case _ of
      Nothing -> do
        Log.error $ "Failed to list S3 objects for " <> PackageName.print name <> " because the process timed out."
        Log.exit $ "Could not upload package " <> package <> " due to an error connecting to the storage backend."
      Just objects ->
        pure $ map _.key objects

    if Array.elem packagePath published then do
      Log.error $ packagePath <> " already exists on S3."
      Log.exit $ "Could not upload " <> package <> " because a package at " <> formatPackageUrl name version <> " already exists."
    else do
      Log.debug $ "Uploading release to the bucket at path " <> packagePath
      let putParams = { key: packagePath, body: buffer, acl: S3.PublicRead }
      Run.liftAff (withBackoff' (S3.putObject s3 putParams)) >>= case _ of
        Nothing -> do
          Log.error "Failed to put object to S3 because the process timed out."
          Log.exit $ "Could not upload package " <> package <> " due to an error connecting to the storage backend."
        Just _ -> do
          Log.info $ "Uploaded " <> package <> " to the bucket at path " <> packagePath
          pure next

  Delete name version next -> do
    let
      package = formatPackageVersion name version
      packagePath = formatPackagePath name version

    Log.debug $ "Deleting " <> package
    s3 <- connectS3 key
    published <- Run.liftAff (withBackoff' (S3.listObjects s3 { prefix: PackageName.print name <> "/" })) >>= case _ of
      Nothing -> do
        Log.error $ "Failed to delete " <> package <> " because the process timed out when attempting to list objects at " <> packagePath <> " from S3."
        Log.exit $ "Could not delete " <> package <> " from the storage backend."
      Just objects ->
        pure $ map _.key objects

    if Array.elem packagePath published then do
      Log.debug $ "Deleting release from the bucket at path " <> packagePath
      let deleteParams = { key: packagePath }
      Run.liftAff (withBackoff' (S3.deleteObject s3 deleteParams)) >>= case _ of
        Nothing -> do
          Log.error $ "Timed out when attempting to delete the release of " <> package <> " from S3 at the path " <> packagePath
          Log.exit $ "Could not delete " <> package <> " from the storage backend."
        Just _ -> do
          Log.debug $ "Deleted release of " <> package <> " from S3 at the path " <> packagePath
          pure next
    else do
      Log.error $ packagePath <> " does not exist on S3 (available: " <> String.joinWith ", " published <> ")"
      Log.exit $ "Could not delete " <> package <> " because it does not exist in the storage backend."

-- | A storage effect that reads from the registry but does not write to it.
handleStorageReadOnly :: forall r a. Storage a -> Run (STORAGE_CACHE + LOG + LOG_EXCEPT + AFF + EFFECT + r) a
handleStorageReadOnly = case _ of
  Upload name version _ next -> do
    Log.warn $ "Requested upload of " <> formatPackageVersion name version <> " to url " <> formatPackageUrl name version <> " but this interpreter is read-only."
    pure next

  Delete name version next -> do
    Log.warn $ "Requested deletion of " <> formatPackageVersion name version <> " from url " <> formatPackageUrl name version <> " but this interpreter is read-only."
    pure next

  Download name version path next -> do
    let package = formatPackageVersion name version
    let cacheId = CacheId package
    buffer <- Cache.get _storageCache cacheId >>= case _ of
      Nothing -> do
        buffer <- downloadS3 name version
        Cache.put _storageCache cacheId buffer
        pure buffer
      Just cached ->
        pure cached.value
    Run.liftAff (Aff.attempt (FS.Aff.writeFile path buffer)) >>= case _ of
      Left error -> do
        Log.error $ "Downloaded " <> package <> " but failed to write it to the file at path " <> path <> ":\n" <> Aff.message error
        Log.exit $ "Could not save downloaded package " <> package <> " due to an internal error."
      Right _ -> pure next

-- | An implementation for downloading packages from the registry using `Aff` requests.
downloadS3 :: forall r. PackageName -> Version -> Run (LOG + LOG_EXCEPT + AFF + EFFECT + r) Buffer
downloadS3 name version = do
  let
    package = formatPackageVersion name version
    packageUrl = formatPackageUrl name version

  Log.debug $ "Downloading " <> package <> " from " <> packageUrl
  response <- Run.liftAff $ withBackoff' $ Affjax.Node.request $ Affjax.Node.defaultRequest
    { method = Left GET
    , responseFormat = ResponseFormat.arrayBuffer
    , url = packageUrl
    }

  -- TODO: Rely on the metadata to check the size and hash? Or do we not care
  -- for registry-internal operations?
  case response of
    Nothing -> do
      Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of a connection timeout."
      Log.exit $ "Failed to download " <> package <> " from the storage backend."
    Just (Left error) -> do
      Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of an HTTP error: " <> Affjax.Node.printError error
      Log.exit $ "Could not download " <> package <> " from the storage backend."
    Just (Right { status, body }) | status /= StatusCode 200 -> do
      buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
      bodyString <- Run.liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
      Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of a non-200 status code (" <> show status <> ") with body " <> bodyString
      Log.exit $ "Could not download " <> package <> " from the storage backend."
    Just (Right { body }) -> do
      Log.debug $ "Successfully downloaded " <> package <> " into a buffer."
      buffer :: Buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
      pure buffer
