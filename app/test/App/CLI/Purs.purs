module Test.Registry.App.CLI.Purs (spec) where

import Registry.App.Prelude

import Data.Foldable (traverse_)
import Node.Path as Path
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  traverse_ testVersion [ "0.13.0", "0.14.0", "0.14.7", "0.15.4" ]
  traverse_ testMissingVersion [ "0.13.1", "0.13.7", "0.15.1", "0.12.0", "0.14.12345" ]
  testCompilationError
  where
  testVersion version =
    Spec.it ("Calls compiler version " <> version) do
      Purs.callCompiler { command: Purs.Version, cwd: Nothing, version: Just version } >>= case _ of
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
      result <- Purs.callCompiler { command: Purs.Version, cwd: Nothing, version: Just version }
      case result of
        Left MissingCompiler -> pure unit
        _ -> Assert.fail "Should have failed with MissingCompiler"

  testCompilationError =
    Spec.it "Handles compilation error for bad input file" do
      let fixture = Path.concat [ "test", "_fixtures", "ShouldFailToCompile" ]
      result <- Purs.callCompiler { command: Purs.Compile { globs: [ fixture ] }, cwd: Nothing, version: Just "0.15.9" }
      case result of
        Left (CompilationError [ { position: { startLine: 1, startColumn: 1 } } ]) -> pure unit
        _ -> Assert.fail "Should have failed with CompilationError"
