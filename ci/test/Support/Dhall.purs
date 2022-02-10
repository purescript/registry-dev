-- | These tests verify that:
-- |   - all the dhall we have in the repo actually compiles
-- |   - all the example manifests actually typecheck as Manifests
module Test.Support.Dhall (spec) where

import Registry.Prelude

import Data.Monoid (guard)
import Data.Set as Set
import Data.String as String
import Debug (spy)
import Foreign.Dhall as Dhall
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS
import Node.Glob.Basic as Glob
import Node.Path as Path
import Sunde as Process
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec = do
  Spec.it "Dhall files typecheck" do
    matches <- Glob.expandGlobs (Path.concat [ "..", "v1" ]) [ "**/*.dhall" ]
    -- We include this check so that if we ever change the directory containing
    -- the test files this test doesn't trivially succeed.
    matches `Assert.shouldNotSatisfy` Set.isEmpty
    let _ = spy "matches" (Set.toUnfoldable matches :: Array FilePath)
    for_ matches \match -> do
      case match of
        "../v1/Address.dhall" -> mempty
        _ -> checkDhall match >>= case _ of
          Left err -> Assert.fail err
          Right _ -> mempty
  Spec.it "Manifest files conform to Manifest type" do
    matches <- Glob.expandGlobs (Path.concat [ "..", "examples" ]) [ "**/*.json" ]
    -- We include this check so that if we ever change the directory containing
    -- the test files this test doesn't trivially succeed.
    matches `Assert.shouldNotSatisfy` Set.isEmpty
    for_ matches \match -> do
      FS.readTextFile UTF8 match >>= Dhall.jsonToDhallManifest >>= case _ of
        Left err -> Assert.fail err
        Right _ -> mempty

-- A helper function to use the `dhall` CLI tool to typecheck a file provided
-- as input.
checkDhall :: FilePath -> Aff (Either String String)
checkDhall file = do
  let cmd = "dhall"
  let stdin = Nothing
  let args = [ "--file", String.drop 6 file ]
  result <- Process.spawn (spy "spawn: " { cmd, stdin, args }) (NodeProcess.defaultSpawnOptions { cwd = Just "../v1" })
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right result.stdout
    _ -> Left result.stderr
