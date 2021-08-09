module Foreign.Dhall where

import Registry.Prelude

import Node.ChildProcess as NodeProcess
import Sunde as Process

jsonToDhallManifest :: String -> Aff (Either String String)
jsonToDhallManifest jsonStr = do
  let cmd = "json-to-dhall"
  let stdin = Just jsonStr
  let args = [ "--records-loose", "--unions-strict", "../v1/Manifest.dhall" ]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right jsonStr
    _ -> Left result.stderr
