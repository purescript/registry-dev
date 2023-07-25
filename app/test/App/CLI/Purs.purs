module Test.Registry.App.CLI.Purs (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.Foreign.Tmp as Tmp
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  traverse_ (testVersion <<< Utils.unsafeVersion) [ "0.13.0", "0.14.0", "0.14.7", "0.15.4" ]
  traverse_ (testMissingVersion <<< Utils.unsafeVersion) [ "0.13.1", "0.13.7", "0.15.1", "0.12.0", "0.14.12345" ]
  traverse_ testCompilationError [ Just (Utils.unsafeVersion "0.13.0"), Just (Utils.unsafeVersion "0.13.8"), Just (Utils.unsafeVersion "0.14.0"), Just (Utils.unsafeVersion "0.15.0"), Nothing ]
  where
  testVersion version =
    Spec.it ("Calls compiler version " <> Version.print version) do
      Purs.callCompiler { command: Purs.Version, cwd: Nothing, version: Just version } >>= case _ of
        Left err -> case err of
          MissingCompiler ->
            Assert.fail "MissingCompiler"
          CompilationError errs ->
            Assert.fail ("CompilationError:\n" <> Purs.printCompilerErrors errs)
          UnknownError err' ->
            Assert.fail ("UnknownError: " <> err')
        Right stdout ->
          Version.print version `Assert.shouldEqual` stdout

  testMissingVersion version =
    Spec.it ("Handles failure when compiler is missing " <> Version.print version) do
      result <- Purs.callCompiler { command: Purs.Version, cwd: Nothing, version: Just version }
      case result of
        Left MissingCompiler -> pure unit
        _ -> Assert.fail "Should have failed with MissingCompiler"

  testCompilationError version =
    Spec.it ("Handles compilation error for bad input file for version " <> maybe "latest" Version.print version) do
      tmp <- Tmp.mkTmpDir
      let file = Path.concat [ tmp, "ShouldFailToCompile.purs" ]
      FS.Aff.writeTextFile UTF8 file "<contents>"
      result <- Purs.callCompiler { command: Purs.Compile { globs: [ file ] }, cwd: Nothing, version }
      case result of
        Left (CompilationError [ { position: { startLine: 1, startColumn: 1 } } ]) -> pure unit
        _ -> Assert.fail "Should have failed with CompilationError"
