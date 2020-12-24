module PackageUpload where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Node.FS.Aff as FS
import S3 as S3

type PackageInfo =
  { name :: String
  , version :: String
  , revision :: Int
  }

type Path = String

upload :: PackageInfo -> Path -> Effect Unit
upload { name, version, revision } path = Aff.launchAff_ $ do
  -- first check that the file path exists
  fileContent <- do
    fileExists <- FS.exists path
    if fileExists
    then FS.readFile path
    else Aff.throwError $ Aff.error $ "File doesn't exist: " <> show path
  -- connect to the bucket
  let bucket = "purescript-registry"
  log $ "Connecting to the bucket " <> show bucket
  s3 <- S3.connect "ams3.digitaloceanspaces.com" bucket
  -- check that the file for that version and revision is there
  let filename
         = name <> "/"
        <> version
        <> (if revision == 0 then "" else "_r" <> show revision)
        <> ".tar.gz"
  publishedPackages <- map _.key <$> S3.listObjects s3 { prefix: name <> "/" }
  if Array.elem filename publishedPackages
  -- if the release is already there we crash
  then Aff.throwError $ Aff.error $ "The package " <> show filename <> " already exists"
  -- if it's not, we upload it with public read permission
  else do
    log $ "Uploading release to the bucket: " <> show filename
    let putParams = { key: filename, body: fileContent, acl: S3.PublicRead }
    void $ S3.putObject s3 putParams
    log "Done."

{-

main :: Effect Unit
main = do
  upload { name: "aff", version: "5.1.2", revision: 0 } "../examples/aff/v5.1.2.json"

-}
