module Foreign.Dhall where

import Registry.Prelude

import Node.ChildProcess as NodeProcess
import Registry.Json as Json
import Sunde as Process

-- | Attempt to convert a JSON file representing a PureScript manifest into
-- | the corresponding `Manifest.dhall` type using the `json-to-dhall` CLI.
jsonToDhallManifest :: String -> Aff (Either String String)
jsonToDhallManifest jsonStr = do
  let cmd = "json-to-dhall"
  let stdin = Just jsonStr
  let args = [ "--records-loose", "--unions-strict", "./v1/Manifest.dhall" ]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right jsonStr
    _ -> Left result.stderr

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
