module Test.Registry.App.CLI.Purs (spec) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.String as String
import JSON as JSON
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.Foreign.Tmp as Tmp
import Registry.PursGraph (ModuleName(..))
import Registry.PursGraph as PursGraph
import Registry.Test.Assert as Assert
import Registry.Test.Utils (filterAvailableCompilers)
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assertions

spec :: Spec.Spec Unit
spec = do
  -- Filter test versions to only those available on the current platform.
  -- On aarch64-darwin, compilers before 0.15.9 don't have native binaries.
  let
    filterVersions = filterAvailableCompilers <<< map Utils.unsafeVersion
    filterMaybeVersions = map (map Utils.unsafeVersion) >>> case _ of
      vs | Utils.isAarch64Darwin -> Array.filter (maybe true (\v -> v >= Utils.minAarch64DarwinCompiler)) vs
      vs -> vs

  traverse_ testVersion (filterVersions [ "0.13.0", "0.14.0", "0.14.7", "0.15.4" ])
  traverse_ (testMissingVersion <<< Utils.unsafeVersion) [ "0.13.1", "0.13.7", "0.15.1", "0.12.0", "0.14.12345" ]
  traverse_ testCompilationError (filterMaybeVersions [ Just "0.13.0", Just "0.13.8", Just "0.14.0", Just "0.15.0", Nothing ])
  traverse_ testGraph (filterMaybeVersions [ Just "0.14.0", Just "0.15.0", Nothing ])

  Spec.describe "Compiler Race Condition Detection" do
    Spec.it "Detects race condition error in UnknownError" do
      let
        raceConditionError = UnknownError $ String.joinWith "\n"
          [ "[ 1 of 226] Compiling Unsafe.Coerce"
          , "[ 2 of 226] Compiling Type.Row"
          , "JSON: Unexpected end of JSON input"
          ]
      Purs.isCompilerRaceCondition raceConditionError `Assertions.shouldEqual` true

    Spec.it "Detects race condition error with surrounding text" do
      let err = UnknownError "Some prefix text\nJSON: Unexpected end of JSON input\nSome suffix"
      Purs.isCompilerRaceCondition err `Assertions.shouldEqual` true

    Spec.it "Does not detect race condition for normal UnknownError" do
      let err = UnknownError "Some other compiler error message"
      Purs.isCompilerRaceCondition err `Assertions.shouldEqual` false

    Spec.it "Does not detect race condition for CompilationError" do
      let
        err = CompilationError
          [ { position: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 10 }
            , message: "Unknown module ModuleB"
            , errorCode: "UnknownModule"
            , errorLink: "https://example.com"
            , filename: "test.purs"
            , moduleName: Just "ModuleA"
            }
          ]
      Purs.isCompilerRaceCondition err `Assertions.shouldEqual` false

    Spec.it "Does not detect race condition for unrelated JSON error" do
      let err = UnknownError "JSON parse error: invalid syntax at line 5"
      Purs.isCompilerRaceCondition err `Assertions.shouldEqual` false
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
        Right str -> case JSON.parse str of
          Left parseErr -> Assert.fail $ "Failed to parse output as JSON: " <> parseErr
          Right json -> case CJ.decode PursGraph.pursGraphCodec json of
            Left decodeErr -> Assert.fail $ "Failed to decode JSON: " <> CJ.DecodeError.print decodeErr
            Right graph -> do
              let
                expected = Map.fromFoldable
                  [ Tuple (ModuleName "ModuleA") { path: moduleA, depends: [ ModuleName "ModuleB" ] }
                  , Tuple (ModuleName "ModuleB") { path: moduleB, depends: [] }
                  ]

              graph `Assert.shouldEqual` expected
