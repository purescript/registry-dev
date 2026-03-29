module Test.Registry.App.Server.MatrixBuilder (spec) where

import Registry.App.Prelude

import Data.Map as Map
import Data.Set as Set
import Effect.Ref as Ref
import Registry.App.Server.MatrixBuilder as MatrixBuilder
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Solver as Solver
import Registry.Test.Assert.Run (runRegistryMock)
import Registry.Test.Utils as Utils
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec = do
  Spec.describe "solveDependantsForCompiler" do
    Spec.it "cascades to dependant for a new compiler" do
      let { solverData, index, metadata } = setup [ "0.15.10" ]
      result <- runSolver solverData index metadata
      let names = Set.map _.name result
      unless (Set.member effectName names) do
        Assert.fail $ "Expected cascade to effect, but got: "
          <> show (Set.map PackageName.print names)

    Spec.it "skips dependant already tested with this compiler" do
      let { solverData, index, metadata } = setup [ "0.15.10", "0.15.11" ]
      result <- runSolver solverData index metadata
      unless (Set.isEmpty result) do
        Assert.fail "Expected empty result set when compiler already tested"

  where
  preludeName = Utils.unsafePackageName "prelude"
  effectName = Utils.unsafePackageName "effect"

  compiler_0_15_10 = Utils.unsafeVersion "0.15.10"
  compiler_0_15_11 = Utils.unsafeVersion "0.15.11"
  allCompilers = Utils.unsafeNonEmptyArray [ compiler_0_15_10, compiler_0_15_11 ]

  preludeVersion = Utils.unsafeVersion "6.0.0"

  preludeManifest = Utils.unsafeManifest "prelude" "6.0.0" []
  effectManifest = Utils.unsafeManifest "effect" "4.0.0"
    [ Tuple "prelude" ">=6.0.0 <7.0.0" ]

  dummySha = Utils.unsafeSha256 "sha256-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa="
  dummyTime = Utils.unsafeDateTime "2022-08-18T20:04:00.000Z"

  mkPublishedMeta version compilers =
    Map.singleton version
      { bytes: 1000.0
      , compilers: Utils.unsafeNonEmptyArray (map Utils.unsafeVersion compilers)
      , hash: dummySha
      , publishedTime: dummyTime
      , ref: Nothing
      }

  -- | Set up a test scenario where prelude@6.0.0 (no deps) has completed its
  -- | matrix job for 0.15.11, and effect@4.0.0 (depends on prelude) has the
  -- | given compilers in its metadata.
  setup effectCompilers = do
    let
      index = Utils.fromRight "Failed to build ManifestIndex" do
        ManifestIndex.insert ManifestIndex.ConsiderRanges preludeManifest ManifestIndex.empty
          >>= ManifestIndex.insert ManifestIndex.ConsiderRanges effectManifest

      -- prelude must include 0.15.11 in its compilers because this test
      -- simulates the state AFTER prelude's own matrix job has completed.
      -- In the real flow: runMatrixJob writes the new compiler into the
      -- parent's metadata (MatrixBuilder.purs:82-91), then the executor
      -- rebuilds the CompilerIndex from current metadata before calling
      -- solveDependantsForCompiler. Without 0.15.11 here, the solver
      -- would compute purs >=0.15.10 <0.15.11 for prelude, excluding
      -- the target compiler from effect's build plan.
      preludeMetadata = Metadata
        { location: Git { url: "https://github.com/purescript/purescript-prelude.git", subdir: Nothing }
        , owners: Nothing
        , published: mkPublishedMeta preludeVersion [ "0.15.10", "0.15.11" ]
        , unpublished: Map.empty
        }

      effectMetadata = Metadata
        { location: Git { url: "https://github.com/purescript/purescript-effect.git", subdir: Nothing }
        , owners: Nothing
        , published: mkPublishedMeta (Utils.unsafeVersion "4.0.0") effectCompilers
        , unpublished: Map.empty
        }

      metadata = Map.fromFoldable [ Tuple preludeName preludeMetadata, Tuple effectName effectMetadata ]

      compilerIndex = Solver.buildCompilerIndex allCompilers index metadata

      solverData =
        { compilerIndex
        , compiler: compiler_0_15_11
        , name: preludeName
        , version: preludeVersion
        , dependencies: Map.empty
        }

    { solverData, index, metadata }

  runSolver solverData index metadata = liftAff do
    indexRef <- liftEffect $ Ref.new index
    metadataRef <- liftEffect $ Ref.new metadata
    runRegistryMock metadataRef indexRef
      $ MatrixBuilder.solveDependantsForCompiler solverData
