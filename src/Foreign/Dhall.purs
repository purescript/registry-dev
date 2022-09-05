module Foreign.Dhall where

import Registry.Prelude

import Node.ChildProcess as NodeProcess
import Registry.Json as Json
import Sunde as Process

-- | Convert a string representing a Dhall expression into JSON using the
-- | `dhall-to-json` CLI.
dhallToJson :: { dhall :: String, cwd :: Maybe FilePath } -> Aff (Either String Json.Json)
dhallToJson { dhall, cwd } = do
  let cmd = "dhall-to-json"
  let stdin = Just dhall
  let args = []
  result <- Process.spawn { cmd, stdin, args } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Json.parseJson result.stdout
    _ -> Left result.stderr
