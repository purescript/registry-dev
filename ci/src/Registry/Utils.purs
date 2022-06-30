module Registry.Utils where

import Registry.Prelude

import Node.ChildProcess as NodeProcess
import Registry.PackageName (PackageName)
import Registry.PackageUpload as Upload
import Registry.RegistryM (RegistryM, Env, throwWithComment)
import Registry.Schema (Metadata)
import Sunde as Process

wget :: String -> FilePath -> RegistryM Unit
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = [ "-O", path, url ]
  result <- liftAff $ Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> pure unit
    _ -> throwWithComment $ "Error while fetching tarball: " <> result.stderr

mkLocalEnv :: Ref (Map PackageName Metadata) -> Env
mkLocalEnv packagesMetadata =
  { comment: \err -> error err
  , closeIssue: log "Skipping GitHub issue closing, we're running locally.."
  , commitToTrunk: \_ _ -> do
      log "Skipping committing to trunk.."
      pure (Right unit)
  , uploadPackage: Upload.upload
  , deletePackage: Upload.delete
  , packagesMetadata
  }
