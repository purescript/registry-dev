module Registry.App.Effect.Storage
  ( STORAGE
  , Storage
  , _storage
  , deleteTarball
  , downloadTarball
  , formatPackageUrl
  , handleStorageS3
  , uploadTarball
  ) where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.HTTP.Method (Method(..))
import Effect.Aff as Aff
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify (NOTIFY)
import Registry.App.Effect.Notify as Notify
import Registry.Constants as Constants
import Registry.Foreign.S3 as S3
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Except (FAIL)
import Type.Proxy (Proxy(..))

data Storage a
  = Upload PackageName Version FilePath a
  | Download PackageName Version FilePath a
  | Delete PackageName Version a

derive instance Functor Storage

type STORAGE r = (storage :: Storage | r)

_storage :: Proxy "storage"
_storage = Proxy

-- | Upload a package to the storage backend.
uploadTarball :: forall r. PackageName -> Version -> FilePath -> Run (STORAGE + r) Unit
uploadTarball name version file = Run.lift _storage (Upload name version file unit)

-- | Download a package from the storage backend.
downloadTarball :: forall r. PackageName -> Version -> FilePath -> Run (STORAGE + r) Unit
downloadTarball name version file = Run.lift _storage (Download name version file unit)

-- | Delete a package from the storage backend.
deleteTarball :: forall r. PackageName -> Version -> Run (STORAGE + r) Unit
deleteTarball name version = Run.lift _storage (Delete name version unit)

formatPackageUrl :: PackageName -> Version -> Affjax.Node.URL
formatPackageUrl name version = Array.fold
  [ Constants.packageStorageUrl
  , "/"
  , PackageName.print name
  , "/"
  , Version.print version
  , ".tar.gz"
  ]

connectS3 :: forall r. S3.SpaceKey -> Run (NOTIFY + LOG + FAIL + AFF + r) S3.Space
connectS3 key = do
  let bucket = "purescript-registry"
  Log.debug $ "Connecting to the bucket " <> bucket
  Run.liftAff (withBackoff' (S3.connect key "ams3.digitaloceanspaces.com" bucket)) >>= case _ of
    Nothing -> Notify.exit "Timed out when attempting to connect to S3."
    Just connection -> pure connection

-- | Handle package storage using a remote S3 bucket.
-- TODO: We could add a storage cache that keeps downloads on the file system
-- instead of always hitting S3.
handleStorageS3 :: forall r a. S3.SpaceKey -> Storage a -> Run (NOTIFY + LOG + FAIL + AFF + r) a
handleStorageS3 key = case _ of
  Upload name version path next -> do
    let
      package = formatPackageVersion name version
      packageUrl = formatPackageUrl name version

    buffer <- Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left error -> Notify.exit $ Array.foldMap (append "\n")
        [ "Could not read contents of package at path " <> path
        , "while attempting to upload " <> package <> "."
        , ""
        , Aff.message error
        ]

      Right buf -> pure buf

    Log.debug $ "Read file for " <> package <> ", now uploading to " <> packageUrl <> "..."
    s3 <- connectS3 key
    published <- Run.liftAff (withBackoff' (S3.listObjects s3 { prefix: PackageName.print name <> "/" })) >>= case _ of
      Nothing -> Notify.exit $ Array.foldMap (append "\n")
        [ "Could not publish package " <> package
        , "because the upload timed out when attempting to list existing objects for " <> PackageName.print name <> " from S3."
        ]
      Just objects -> pure $ map _.key objects

    if Array.elem packageUrl published then
      Notify.exit $ "Could not upload " <> package <> " because an upload at " <> packageUrl <> " already exists."
    else do
      Log.debug $ "Uploading release to the bucket at path " <> packageUrl
      let putParams = { key: packageUrl, body: buffer, acl: S3.PublicRead }
      Run.liftAff (withBackoff' (S3.putObject s3 putParams)) >>= case _ of
        Nothing ->
          Notify.exit $ "Could not upload " <> package <> " because the process timed out when attempting to write the release to S3."
        Just _ -> do
          Log.debug $ "Uploaded " <> package <> " to the bucket at path " <> packageUrl
          pure next

  Download name version path next -> do
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
      Nothing ->
        Notify.exit $ "Could not download " <> package <> " from " <> packageUrl <> " because the connection timed out."
      Just (Left error) ->
        Notify.exit $ "Could not download " <> package <> " from " <> packageUrl <> " due to a HTTP error:\n" <> Affjax.Node.printError error
      Just (Right { status, body }) | status /= StatusCode 200 -> do
        buffer <- Run.liftAff $ liftEffect $ Buffer.fromArrayBuffer body
        bodyString <- Run.liftAff $ liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
        Notify.exit $ Array.fold
          [ "Could not download "
          , package
          , " because the status code was not OK "
          , show status
          , ". Received the body:\n"
          , bodyString
          ]
      Just (Right { body }) -> do
        Log.debug $ "Successfully downloaded " <> package <> " into a buffer."
        buffer <- Run.liftAff $ liftEffect $ Buffer.fromArrayBuffer body
        Run.liftAff (Aff.attempt (FS.Aff.writeFile path buffer)) >>= case _ of
          Left error -> Notify.exit $ "Downloaded " <> package <> " but failed to write it to the file at path " <> path <> ":\n" <> Aff.message error
          Right _ -> pure unit
        pure next

  Delete name version next -> do
    let
      package = formatPackageVersion name version
      packageUrl = formatPackageUrl name version

    Log.debug $ "Deleting " <> package
    s3 <- connectS3 key
    published <- Run.liftAff (withBackoff' (S3.listObjects s3 { prefix: PackageName.print name <> "/" })) >>= case _ of
      Nothing -> Notify.exit $ "Could not delete " <> package <> " because the process timed out when attempting to list objects at " <> packageUrl <> " from S3."
      Just objects -> pure $ map _.key objects

    if Array.elem packageUrl published then do
      Log.debug $ "Deleting release from the bucket at path " <> packageUrl
      let deleteParams = { key: packageUrl }
      Run.liftAff (withBackoff' (S3.deleteObject s3 deleteParams)) >>= case _ of
        Nothing -> Notify.exit $ "Timed out when attempting to delete the release of " <> package <> " from S3 at the path " <> packageUrl
        Just _ -> do
          Log.debug $ "Deleted release of " <> package <> " from S3 at the path " <> packageUrl
          pure next
    else
      Notify.exit $ "Could not delete " <> package <> " from path " <> packageUrl <> " because that path does not exist on S3"
