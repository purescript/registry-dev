module Test.Support.Format where

import Registry.Prelude

import Node.ChildProcess as NodeProcess
import Sunde as Process
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec =
  Spec.it "Source code is formatted correctly" do
    result <- checkFormat
    case result of
      Left err -> Assert.fail err
      Right _ -> mempty

-- A helper function to verify that all source code formats properly.
checkFormat :: Aff (Either String String)
checkFormat = do
  let cmd = "purs-tidy"
  let stdin = Nothing
  let args = [ "check", "src", "test" ]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right result.stdout
    _ -> Left result.stderr
