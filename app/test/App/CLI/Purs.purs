module Test.Registry.App.CLI.Purs (spec) where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Foldable (traverse_)
import Data.Map as Map
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.Foreign.Tmp as Tmp
import Registry.PursGraph (ModuleName(..))
import Registry.PursGraph as PursGraph
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  traverse_ (testVersion <<< Utils.unsafeVersion) [ "0.13.0", "0.14.0", "0.14.7", "0.15.4" ]
  traverse_ (testMissingVersion <<< Utils.unsafeVersion) [ "0.13.1", "0.13.7", "0.15.1", "0.12.0", "0.14.12345" ]
  traverse_ (testCompilationError <<< map Utils.unsafeVersion) [ Just "0.13.0", Just "0.13.8", Just "0.14.0", Just "0.15.0", Nothing ]
  traverse_ (testGraph <<< map Utils.unsafeVersion) [ Just "0.14.0", Just "0.15.0", Nothing ]
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
        Left (CompilationError [ { position: { startLine: 1, startColumn: 1 } } ]) ->
          pure unit
        _ -> Assert.fail "Should have failed with CompilationError"

  testGraph version =
    Spec.it ("Produces a graph for " <> maybe "latest" Version.print version) do
      tmp <- Tmp.mkTmpDir
      let moduleA = Path.concat [ tmp, "ModuleA.purs" ]
      let moduleB = Path.concat [ tmp, "ModuleB.purs" ]
      FS.Aff.writeTextFile UTF8 moduleA "module ModuleA where\n\nimport ModuleB\n"
      FS.Aff.writeTextFile UTF8 moduleB "module ModuleB where\n"
      result <- Purs.callCompiler { command: Purs.Graph { globs: [ moduleA, moduleB ] }, cwd: Nothing, version }
      case result of
        Left runErr -> Assert.fail $ case runErr of
          CompilationError errs -> Purs.printCompilerErrors errs
          UnknownError str -> str
          MissingCompiler -> "MissingCompiler"
        Right str -> case Argonaut.Parser.jsonParser str of
          Left parseErr -> Assert.fail $ "Failed to parse output as JSON: " <> parseErr
          Right json -> case CA.decode PursGraph.pursGraphCodec json of
            Left decodeErr -> Assert.fail $ "Failed to decode JSON: " <> CA.printJsonDecodeError decodeErr
            Right graph -> do
              let
                expected = Map.fromFoldable
                  [ Tuple (ModuleName "ModuleA") { path: moduleA, depends: [ ModuleName "ModuleB" ] }
                  , Tuple (ModuleName "ModuleB") { path: moduleB, depends: [] }
                  ]

              graph `Assert.shouldEqual` expected
