module Dhall where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Sunde as Process
import Node.ChildProcess as NodeProcess

jsonToDhall :: String -> Aff (Either String String)
jsonToDhall jsonStr = do
  let cmd = "json-to-dhall"
  let stdin = Just jsonStr
  let args = ["--records-loose", "--unions-strict", "../v1/Manifest.dhall"]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right jsonStr
    _ -> Left result.stderr