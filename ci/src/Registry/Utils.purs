module Registry.Utils where

import Registry.Prelude

import Node.ChildProcess as NodeProcess
import Registry.RegistryM (RegistryM, throwWithComment)
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
