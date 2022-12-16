module Test.Registry.App.CLI.Purs (spec) where

import Registry.App.Prelude

import Data.Foldable (traverse_)
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  traverse_ testVersion [ "0.13.0", "0.14.0", "0.14.7", "0.15.4" ]
  traverse_ testMissingVersion [ "0.13.1", "0.14.8" ]
  where
  testVersion version =
    Spec.it ("Calls compiler version " <> version) do
      Purs.callCompiler { command: Purs.Version, cwd: Nothing, version } >>= case _ of
        Left err -> case err of
          MissingCompiler ->
            Assert.fail "MissingCompiler"
          CompilationError errs ->
            Assert.fail ("CompilationError:\n" <> Purs.printCompilerErrors errs)
          UnknownError err' ->
            Assert.fail ("UnknownError: " <> err')
        Right stdout ->
          version `Assert.shouldEqual` stdout

  testMissingVersion version =
    Spec.it ("Handles failure when compiler is missing " <> version) do
      result <- Purs.callCompiler { command: Purs.Version, cwd: Nothing, version }
      case result of
        Left MissingCompiler -> pure unit
        _ -> Assert.fail "Should have failed with MissingCompiler"
