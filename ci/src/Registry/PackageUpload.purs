module Registry.PackageUpload where

import Registry.Prelude

import Data.Array as Array
import Effect.Aff as Aff
import Foreign.S3 as S3
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version

type PackageInfo =
  { name :: PackageName
  , version :: Version
  }

upload :: PackageInfo -> FilePath -> Aff Unit
upload { name, version } path = do
  fileContent <- FS.readFile path

  -- connect to the bucket
  let bucket = "purescript-registry"
  log $ "Connecting to the bucket " <> show bucket
  s3 <- S3.connect "ams3.digitaloceanspaces.com" bucket

  -- check that the file for that version is there
  let
    packageName = PackageName.print name
    filename = Path.concat [ packageName, Version.printVersion version <> ".tar.gz" ]

  publishedPackages <- map _.key <$> S3.listObjects s3 { prefix: packageName <> "/" }

  if Array.elem filename publishedPackages then
    -- if the release is already there we crash
    Aff.throwError $ Aff.error $ "The package " <> show filename <> " already exists"
  else do
    -- if it's not, we upload it with public read permission
    log $ "Uploading release to the bucket: " <> show filename
    let putParams = { key: filename, body: fileContent, acl: S3.PublicRead }
    void $ S3.putObject s3 putParams
    log "Done."
