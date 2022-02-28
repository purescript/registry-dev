module Registry.PackageUpload where

import Registry.Prelude

import Data.Array as Array
import Effect.Aff as Aff
import Foreign.S3 as S3
import Node.FS.Aff as FS
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version

connect :: Aff S3.Space
connect = do
  let bucket = "purescript-registry"
  log $ "Connecting to the bucket " <> show bucket
  S3.connect "ams3.digitaloceanspaces.com" bucket

type PackageInfo =
  { name :: PackageName
  , version :: Version
  }

formatPackagePath :: String -> Version -> String
formatPackagePath name version = Array.fold
  [ name
  , "/"
  , Version.printVersion version
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
  publishedPackages <- map _.key <$> S3.listObjects s3 { prefix: packageName <> "/" }

  if Array.elem packagePath publishedPackages then
    -- if the release is already there we crash
    Aff.throwError $ Aff.error $ "The package " <> show packagePath <> " already exists"
  else do
    -- if it's not, we upload it with public read permission
    log $ "Uploading release to the bucket: " <> show packagePath
    let putParams = { key: packagePath, body: fileContent, acl: S3.PublicRead }
    void $ S3.putObject s3 putParams
    log "Done."

delete :: PackageInfo -> Aff Unit
delete { name, version } = do
  s3 <- connect

  -- check that the file for that version is there
  let
    packageName = PackageName.print name
    packagePath = formatPackagePath packageName version

  publishedPackages <- map _.key <$> S3.listObjects s3 { prefix: packageName <> "/" }

  if Array.elem packagePath publishedPackages then do
    -- if the release is already there we delete it
    log $ "Deleting release from the bucket: " <> show packagePath
    let deleteParams = { key: packagePath }
    void $ S3.deleteObject s3 deleteParams
    log "Done."
  else do
    -- if it's not, we crash
    Aff.throwError $ Aff.error $ "The package " <> show packagePath <> " doesn't exist."
