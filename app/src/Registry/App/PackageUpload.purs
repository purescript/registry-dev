module Registry.App.PackageStorage where

import Registry.App.Prelude

import Data.Array as Array
import Effect.Aff as Aff
import Foreign.S3 as S3
import Node.FS.Aff as FS
import Registry.PackageName as PackageName
import Registry.Version as Version

connect :: Aff S3.Space
connect = do
  let bucket = "purescript-registry"
  log $ "Connecting to the bucket " <> show bucket
  withBackoff' (S3.connect "ams3.digitaloceanspaces.com" bucket) >>= case _ of
    Nothing -> Aff.throwError $ Aff.error "Timed out when attempting to connect to S3"
    Just connection -> pure connection

type PackageInfo =
  { name :: PackageName
  , version :: Version
  }

formatPackagePath :: String -> Version -> String
formatPackagePath name version = Array.fold
  [ name
  , "/"
  , Version.print version
  , ".tar.gz"
  ]

upload :: PackageInfo -> FilePath -> Aff Unit
upload { name, version } path = do
  fileContent <- FS.readFile path
  s3 <- connect

  let
    packageName = PackageName.print name
    packagePath = formatPackagePath packageName version

  -- check that the file for that version is there
  publishedPackages <- withBackoff' (S3.listObjects s3 { prefix: packageName <> "/" }) >>= case _ of
    Nothing -> Aff.throwError $ Aff.error "Timed out when attempting to list objects for package name from S3"
    Just objects -> pure $ map _.key objects

  if Array.elem packagePath publishedPackages then
    -- if the release is already there we crash
    Aff.throwError $ Aff.error $ "The package " <> show packagePath <> " already exists"
  else do
    -- if it's not, we upload it with public read permission
    log $ "Uploading release to the bucket: " <> show packagePath
    let putParams = { key: packagePath, body: fileContent, acl: S3.PublicRead }
    withBackoff' (S3.putObject s3 putParams) >>= case _ of
      Nothing -> throwError $ Aff.error "Timed out when attempting to write the release to S3"
      Just _ -> log "Done."

delete :: PackageInfo -> Aff Unit
delete { name, version } = do
  s3 <- connect

  -- check that the file for that version is there
  let
    packageName = PackageName.print name
    packagePath = formatPackagePath packageName version

  publishedPackages <- withBackoff' (S3.listObjects s3 { prefix: packageName <> "/" }) >>= case _ of
    Nothing -> Aff.throwError $ Aff.error "Timed out when attempting to list objects for package name from S3."
    Just objects -> pure $ map _.key objects

  if Array.elem packagePath publishedPackages then do
    -- if the release is already there we delete it
    log $ "Deleting release from the bucket: " <> show packagePath
    let deleteParams = { key: packagePath }
    withBackoff' (S3.deleteObject s3 deleteParams) >>= case _ of
      Nothing -> throwError $ Aff.error "Timed out when attempting to delete the release from S3."
      Just _ -> log "Done."
  else do
    -- if it's not, we crash
    Aff.throwError $ Aff.error $ "The package " <> show packagePath <> " doesn't exist."
