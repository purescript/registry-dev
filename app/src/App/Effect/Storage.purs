-- | An effect for reading and writing to the registry storage backend.
module Registry.App.Effect.Storage
  ( S3Env
  , STORAGE
  , STORAGE_CACHE
  , Storage(..)
  , StorageCache
  , _storage
  , _storageCache
  , delete
  , download
  , handleReadOnly
  , handleS3
  , interpret
  , upload
  , query
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Exists as Exists
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
import Effect.Exception as Exception
import Fetch.Retry as Fetch
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Registry.App.Effect.Cache (class FsEncodable, Cache, FsEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Foreign.S3 as S3
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- | The Storage effect, which describes uploading, downloading, and deleting
-- | tarballs from the registry storage backend.
data Storage a
  = Upload PackageName Version FilePath (Either String Unit -> a)
  | Download PackageName Version FilePath (Either String Unit -> a)
  | Delete PackageName Version (Either String Unit -> a)
  | Query PackageName (Either String (Set Version) -> a)

derive instance Functor Storage

type STORAGE r = (storage :: Storage | r)

_storage :: Proxy "storage"
_storage = Proxy

-- | Upload a package tarball to the storage backend from the given path.
upload :: forall r. PackageName -> Version -> FilePath -> Run (STORAGE + EXCEPT String + r) Unit
upload name version file = Except.rethrow =<< Run.lift _storage (Upload name version file identity)

-- | Download a package tarball from the storage backend to the given path.
download :: forall r. PackageName -> Version -> FilePath -> Run (STORAGE + EXCEPT String + r) Unit
download name version file = Except.rethrow =<< Run.lift _storage (Download name version file identity)

-- | Delete a package tarball from the storage backend.
delete :: forall r. PackageName -> Version -> Run (STORAGE + EXCEPT String + r) Unit
delete name version = Except.rethrow =<< Run.lift _storage (Delete name version identity)

-- | Interpret the STORAGE effect, given a handler.
interpret :: forall r a. (Storage ~> Run r) -> Run (STORAGE + r) a -> Run r a
interpret handler = Run.interpret (Run.on _storage handler Run.send)

-- | Query what tarballs exist for a package in the storage backend
query :: forall r. PackageName -> Run (STORAGE + EXCEPT String + r) (Set Version)
query name = Except.rethrow =<< Run.lift _storage (Query name identity)

formatPackagePath :: PackageName -> Version -> String
formatPackagePath name version = Array.fold
  [ PackageName.print name
  , "/"
  , Version.print version
  , ".tar.gz"
  ]

parsePackagePath :: String -> Either String { name :: PackageName, version :: Version }
parsePackagePath input = do
  filePath <- String.stripSuffix (String.Pattern ".tar.gz") input
    # note ("Missing .tar.gz suffix: " <> input)
  case String.split (String.Pattern "/") filePath of
    [ namePart, versionPart ] ->
      { name: _, version: _ }
        <$> PackageName.parse namePart
        <*> Version.parse versionPart
    parts -> Left
      if Array.length parts > 2 then "Too many parts in path: " <> input
      else "Too few parts in path: " <> input

formatPackageUrl :: forall r. PackageName -> Version -> Run (RESOURCE_ENV + r) URL
formatPackageUrl name version = do
  { s3ApiUrl } <- Env.askResourceEnv
  pure $ Array.fold [ s3ApiUrl, "/", formatPackagePath name version ]

connectS3 :: forall r. S3.SpaceKey -> Run (RESOURCE_ENV + LOG + EXCEPT String + AFF + r) S3.Space
connectS3 key = do
  let bucket = "purescript-registry"
  { s3BucketUrl: space } <- Env.askResourceEnv
  Log.debug $ "Connecting to the bucket " <> bucket <> " at space " <> space <> " with public key " <> key.key
  Run.liftAff (withRetryOnTimeout (Aff.attempt (S3.connect key space bucket))) >>= case _ of
    Cancelled ->
      Except.throw "Timed out when attempting to connect to S3 storage backend."
    Failed err -> do
      Log.error $ "Failed to connect to S3 due to an exception: " <> Aff.message err
      Except.throw "Could not connect to storage backend."
    Succeeded connection -> do
      Log.debug "Connected to S3!"
      pure connection

type S3Env =
  { cache :: FilePath
  , s3 :: S3.SpaceKey
  }

-- | Handle package storage using a remote S3 bucket.
handleS3 :: forall r a. S3Env -> Storage a -> Run (RESOURCE_ENV + LOG + AFF + EFFECT + r) a
handleS3 env = Cache.interpret _storageCache (Cache.handleFs env.cache) <<< case _ of
  Query name reply -> map (map reply) Except.runExcept do
    s3 <- connectS3 env.s3
    resources <- Except.rethrow =<< Run.liftAff (withRetryListObjects s3 name)
    pure $ Set.fromFoldable $ resources >>= \resource -> do
      { name: parsedName, version } <- Array.fromFoldable $ parsePackagePath resource
      version <$ guard (name == parsedName)

  Download name version path reply -> map (map reply) Except.runExcept do
    let package = formatPackageVersion name version
    buffer <- Cache.get _storageCache (Package name version) >>= case _ of
      Nothing -> do
        buffer <- downloadS3 name version
        Cache.put _storageCache (Package name version) buffer
        pure buffer
      Just cached ->
        pure cached
    Run.liftAff (Aff.attempt (FS.Aff.writeFile path buffer)) >>= case _ of
      Left error -> do
        Log.error $ "Downloaded " <> package <> " but failed to write it to the file at path " <> path <> ":\n" <> Aff.message error
        Except.throw $ "Could not save downloaded package " <> package <> " due to an internal error."
      Right _ -> pure unit

  Upload name version path reply -> map (map reply) Except.runExcept do
    let
      package = formatPackageVersion name version
      packagePath = formatPackagePath name version

    buffer <- Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left error -> do
        Log.error $ "Failed to read contents of " <> package <> " at path " <> path <> ": " <> Aff.message error
        Except.throw $ "Could not upload package " <> package <> " due to a file system error."
      Right buf ->
        pure buf

    Log.debug $ "Read file for " <> package <> ", now uploading to " <> packagePath <> "..."
    s3 <- connectS3 env.s3
    published <- Except.rethrow =<< Run.liftAff (withRetryListObjects s3 name)
    if Array.elem packagePath published then do
      Log.error $ packagePath <> " already exists on S3."
      packageUrl <- formatPackageUrl name version
      Except.throw $ "Could not upload " <> package <> " because a package at " <> packageUrl <> " already exists."
    else do
      Log.debug $ "Uploading release to the bucket at path " <> packagePath
      let putParams = { key: packagePath, body: buffer, acl: S3.PublicRead }
      Run.liftAff (withRetryOnTimeout (Aff.attempt (S3.putObject s3 putParams))) >>= case _ of
        Cancelled -> do
          Log.error "Failed to upload object to S3 because the process timed out."
          Except.throw $ "Could not upload package " <> package <> " due to an error connecting to the storage backend."
        Failed error -> do
          Log.error $ "Failed to upload object to S3 because of an exception: " <> Aff.message error
          Except.throw $ "Could not upload package " <> package <> " due to an error connecting to the storage backend."
        Succeeded _ ->
          Log.info $ "Uploaded " <> package <> " to the bucket at path " <> packagePath

  Delete name version reply -> map (map reply) Except.runExcept do
    let
      package = formatPackageVersion name version
      packagePath = formatPackagePath name version

    Log.debug $ "Deleting " <> package
    s3 <- connectS3 env.s3
    published <- Except.rethrow =<< Run.liftAff (withRetryListObjects s3 name)
    if Array.elem packagePath published then do
      Log.debug $ "Deleting release from the bucket at path " <> packagePath
      let deleteParams = { key: packagePath }
      Run.liftAff (withRetryOnTimeout (Aff.attempt (S3.deleteObject s3 deleteParams))) >>= case _ of
        Cancelled -> do
          Log.error $ "Timed out when attempting to delete the release of " <> package <> " from S3 at the path " <> packagePath
          Except.throw $ "Could not delete " <> package <> " from the storage backend."
        Failed error -> do
          Log.error $ "Failed to delete object from S3 because of an exception: " <> Aff.message error
          Except.throw $ "Could not delete package " <> package <> " due to an error connecting to the storage backend."
        Succeeded _ -> do
          Log.debug $ "Deleted release of " <> package <> " from S3 at the path " <> packagePath
          Cache.delete _storageCache (Package name version)
          pure unit
    else do
      Log.error $ packagePath <> " does not exist on S3 (available: " <> String.joinWith ", " published <> ")"
      Except.throw $ "Could not delete " <> package <> " because it does not exist in the storage backend."

-- | A storage effect that reads from the registry but does not write to it.
handleReadOnly :: forall r a. FilePath -> Storage a -> Run (RESOURCE_ENV + LOG + AFF + EFFECT + r) a
handleReadOnly cache = Cache.interpret _storageCache (Cache.handleFs cache) <<< case _ of
  -- TODO: is there a way to do this without S3 credentials?
  Query _ reply -> do
    pure $ reply $ Left "Cannot query in read-only mode."

  Upload name version path reply -> map (map reply) Except.runExcept do
    let package = formatPackageVersion name version
    packageUrl <- formatPackageUrl name version
    Log.warn $ "Requested upload of " <> package <> " to url " <> packageUrl <> " but this interpreter is read-only. Caching tarball locally."
    buffer <- Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left error -> do
        Log.error $ "Failed to read tarball for " <> package <> " at path " <> path <> ": " <> Aff.message error
        Except.throw $ "Could not cache package " <> package <> " due to a file system error."
      Right buf ->
        pure buf
    Cache.put _storageCache (Package name version) buffer

  Delete name version reply -> do
    packageUrl <- formatPackageUrl name version
    Log.warn $ "Requested deletion of " <> formatPackageVersion name version <> " from url " <> packageUrl <> " but this interpreter is read-only."
    pure $ reply $ Right unit

  Download name version path reply -> map (map reply) Except.runExcept do
    let package = formatPackageVersion name version
    buffer <- Cache.get _storageCache (Package name version) >>= case _ of
      Nothing -> do
        buffer <- downloadS3 name version
        Cache.put _storageCache (Package name version) buffer
        pure buffer
      Just cached ->
        pure cached
    Run.liftAff (Aff.attempt (FS.Aff.writeFile path buffer)) >>= case _ of
      Left error -> do
        Log.error $ "Downloaded " <> package <> " but failed to write it to the file at path " <> path <> ":\n" <> Aff.message error
        Except.throw $ "Could not save downloaded package " <> package <> " due to an internal error."
      Right _ -> pure unit

-- | An implementation for downloading packages from the registry using `Aff` requests.
downloadS3 :: forall r. PackageName -> Version -> Run (RESOURCE_ENV + LOG + EXCEPT String + AFF + EFFECT + r) Buffer
downloadS3 name version = do
  let package = formatPackageVersion name version

  packageUrl <- formatPackageUrl name version

  Log.debug $ "Downloading " <> package <> " from " <> packageUrl
  response <- Run.liftAff $ Fetch.withRetryRequest packageUrl {}

  -- TODO: Rely on the metadata to check the size and hash? Or do we not care
  -- for registry-internal operations?
  case response of
    Cancelled -> do
      Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of a connection timeout."
      Except.throw $ "Failed to download " <> package <> " from the storage backend."
    Failed (Fetch.FetchError error) -> do
      Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of an HTTP error: " <> Exception.message error
      Except.throw $ "Could not download " <> package <> " from the storage backend."
    Failed (Fetch.StatusError { status, arrayBuffer: arrayBufferAff }) -> do
      arrayBuffer <- Run.liftAff arrayBufferAff
      buffer <- Run.liftEffect $ Buffer.fromArrayBuffer arrayBuffer
      bodyString <- Run.liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
      Log.error $ "Failed to download " <> package <> " from " <> packageUrl <> " because of a bad status code (" <> show status <> ") with body " <> bodyString
      Except.throw $ "Could not download " <> package <> " from the storage backend."
    Succeeded { arrayBuffer: arrayBufferAff } -> do
      arrayBuffer <- Run.liftAff arrayBufferAff

      Log.debug $ "Successfully downloaded " <> package <> " into a buffer."
      buffer :: Buffer <- Run.liftEffect $ Buffer.fromArrayBuffer arrayBuffer
      pure buffer

withRetryListObjects :: S3.Space -> PackageName -> Aff (Either String (Array String))
withRetryListObjects s3 name = do
  let package = PackageName.print name
  result <- withRetry (defaultRetry { retryOnFailure = \attempt _ -> attempt < 3 }) do
    Aff.attempt (S3.listObjects s3 { prefix: package <> "/" })
  pure $ case result of
    Cancelled -> do
      Left $ "Failed to list S3 objects for " <> package <> " because the process timed out."
    Failed error -> do
      Left $ "Failed to list S3 objects for " <> package <> " because of an exception: " <> Aff.message error
    Succeeded objects ->
      pure $ map _.key objects

-- | A key type for the storage cache. Only supports packages identified by
-- | their name and version.
data StorageCache (c :: Type -> Type -> Type) a = Package PackageName Version (c Buffer a)

instance Functor2 c => Functor (StorageCache c) where
  map k (Package name version a) = Package name version (map2 k a)

instance FsEncodable StorageCache where
  encodeFs = case _ of
    Package name version next ->
      Exists.mkExists $ AsBuffer (PackageName.print name <> "-" <> Version.print version) next

type STORAGE_CACHE r = (storageCache :: Cache StorageCache | r)

_storageCache :: Proxy "storageCache"
_storageCache = Proxy
