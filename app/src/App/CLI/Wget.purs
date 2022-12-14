module Registry.App.CLI.Wget where

import Registry.App.Prelude

import Data.String as String
import Node.ChildProcess as NodeProcess
import Sunde as Process

wget :: String -> FilePath -> Aff (Either String Unit)
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = [ "-O", path, url ]
  maybeResult <- withBackoff' $ Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case maybeResult of
    Nothing ->
      Left $ "Timed out attempting to use wget to fetch " <> url
    Just { exit, stderr } -> case exit of
      NodeProcess.Normally 0 -> Right unit
      _ -> Left $ String.trim stderr
