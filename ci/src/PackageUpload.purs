module PackageUpload where

import Prelude

import Data.Array (fold)
import Data.Array as Array
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Node.FS.Aff as FS
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import S3 as S3

type PackageInfo =
  { name :: PackageName
  , version :: String
  , revision :: Int
  }

type Path = String

upload :: PackageInfo -> Path -> Effect Unit
upload { name, version, revision } path = Aff.launchAff_ $ do
  -- first check that the file path exists
  fileContent <-
    FS.exists path >>= if _ then
      FS.readFile path
    else
      Aff.throwError $ Aff.error $ "File doesn't exist: " <> show path

  -- connect to the bucket
  let bucket = "purescript-registry"
  log $ "Connecting to the bucket " <> show bucket
  s3 <- S3.connect "ams3.digitaloceanspaces.com" bucket

  -- check that the file for that version and revision is there
  let
    packageName = PackageName.print name
    filename = fold
      [ packageName
      , "/"
      , version
      , if revision == 0 then "" else "_r" <> show revision
      , ".tar.gz"
      ]

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

{-

main :: Effect Unit
main = do
  upload { name: "aff", version: "5.1.2", revision: 0 } "../examples/aff/v5.1.2.json"

-}
