module Test.Registry.App.Server.MatrixBuilder (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
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

    Spec.it "propagates through already-compiled packages to reach stranded dependants" do
      -- This is a regression test for a specific scenario when cascading matrix
      -- jobs for a new compiler. Setup:
      --
      --   prelude@5.0.0  (no deps)
      --   prelude@6.0.0  (no deps)
      --   leaf-dep@4.0.0 (prelude >=4.0.0 <6.0.0)  -- old, wants old prelude
      --   leaf-dep@6.0.0 (prelude >=6.0.0 <7.0.0)  -- new, wants new prelude
      --   mid-pkg@1.0.0  (leaf-dep >=4.0.0 <7.0.0) -- wide range on leaf-dep
      --   top-pkg@1.0.0  (mid-pkg >=1.0.0 <2.0.0, prelude >=6.0.0 <7.0.0)
      --
      -- top-pkg's prelude constraint forces the solver to pick leaf-dep@6.0.0.
      -- When the old dep path completes first (leaf-dep@4.0.0 -> mid-pkg),
      -- top-pkg fails because leaf-dep@6.0.0 isn't ready. Later when
      -- leaf-dep@6.0.0 completes, mid-pkg already has the compiler. Without
      -- special care (solveDependantsForCompiler recursing into transitive
      -- dependants), the cascade stops at mid-pkg and top-pkg is never
      -- retriggered.
      let
        cascadeIndex = Utils.fromRight "Failed to build ManifestIndex" do
          ManifestIndex.insert ManifestIndex.ConsiderRanges prelude5Manifest ManifestIndex.empty
            >>= ManifestIndex.insert ManifestIndex.ConsiderRanges preludeManifest
            >>= ManifestIndex.insert ManifestIndex.ConsiderRanges leafDep4Manifest
            >>= ManifestIndex.insert ManifestIndex.ConsiderRanges leafDep6Manifest
            >>= ManifestIndex.insert ManifestIndex.ConsiderRanges midPkgManifest
            >>= ManifestIndex.insert ManifestIndex.ConsiderRanges topPkgManifest

        -- Initial state: all packages only have 0.15.10
        initMetadata = Map.fromFoldable
          [ Utils.unsafeMetadata "prelude" [ Tuple "5.0.0" [ "0.15.10" ], Tuple "6.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "leaf-dep" [ Tuple "4.0.0" [ "0.15.10" ], Tuple "6.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "mid-pkg" [ Tuple "1.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "top-pkg" [ Tuple "1.0.0" [ "0.15.10" ] ]
          ]

      metadataRef <- liftEffect $ Ref.new initMetadata
      indexRef <- liftEffect $ Ref.new cascadeIndex

      let
        step name version dependencies = do
          -- Simulate runMatrixJob completing: add 0.15.11 to metadata
          liftEffect $ addCompilerToMetadata metadataRef name version compiler_0_15_11
          -- Rebuild CompilerIndex from current metadata (as JobExecutor does)
          currentMetadata <- liftEffect $ Ref.read metadataRef
          let compilerIndex = Solver.buildCompilerIndex allCompilers cascadeIndex currentMetadata
          let solverData = { compilerIndex, compiler: compiler_0_15_11, name, version, dependencies }
          runRegistryMock metadataRef indexRef
            $ MatrixBuilder.solveDependantsForCompiler solverData

      -- Wave 0: leaves complete
      -- prelude@5.0.0 -> should find leaf-dep@4.0.0 (prelude >=4.0.0 <6.0.0 includes 5.0.0)
      r1 <- step preludeName prelude5Version Map.empty
      let r1Names = Set.map _.name r1
      unless (Set.member leafDepName r1Names) do
        Assert.fail $ "Step 1: Expected leaf-dep in cascade from prelude@5.0.0, got: "
          <> show (Set.map PackageName.print r1Names)

      -- prelude@6.0.0 -> should try leaf-dep@6.0.0 and top-pkg, but they can't solve yet
      _r2 <- step preludeName preludeVersion Map.empty

      -- Old dep path resolves first
      -- leaf-dep@4.0.0 -> should find mid-pkg@1.0.0 (leaf-dep >=4.0.0 <7.0.0)
      r3 <- step leafDepName leafDep4Version (depsOf leafDep4Manifest)
      let r3Names = Set.map _.name r3
      unless (Set.member midPkgName r3Names) do
        Assert.fail $ "Step 3: Expected mid-pkg in cascade from leaf-dep@4.0.0, got: "
          <> show (Set.map PackageName.print r3Names)

      -- mid-pkg@1.0.0 -> tries top-pkg, but top-pkg needs leaf-dep@6.0.0 which isn't ready
      r4 <- step midPkgName midPkgVersion (depsOf midPkgManifest)
      let r4Names = Set.map _.name r4
      when (Set.member topPkgName r4Names) do
        Assert.fail "Step 4: top-pkg should NOT be enqueued yet (leaf-dep@6.0.0 not ready)"

      -- New dep path resolves later
      -- leaf-dep@6.0.0 -> finds mid-pkg (already has compiler) -> PROPAGATES through ->
      -- finds top-pkg -> solver now succeeds because leaf-dep@6.0.0 is ready
      r5 <- step leafDepName leafDep6Version (depsOf leafDep6Manifest)
      let r5Names = Set.map _.name r5
      unless (Set.member topPkgName r5Names) do
        Assert.fail $ "Step 5: Expected top-pkg via propagation through mid-pkg, got: "
          <> show (Set.map PackageName.print r5Names)

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
      metadata = Map.fromFoldable
        [ Utils.unsafeMetadata "prelude" [ Tuple "6.0.0" [ "0.15.10", "0.15.11" ] ]
        , Utils.unsafeMetadata "effect" [ Tuple "4.0.0" effectCompilers ]
        ]

      compilerIndex = Solver.buildCompilerIndex allCompilers index metadata

      solverData =
        { compilerIndex
        , compiler: compiler_0_15_11
        , name: preludeName
        , version: preludeVersion
        , dependencies: Map.empty
        }

    { solverData, index, metadata }

  -- Cascade test: package graph with transitive version conflict
  leafDepName = Utils.unsafePackageName "leaf-dep"
  midPkgName = Utils.unsafePackageName "mid-pkg"
  topPkgName = Utils.unsafePackageName "top-pkg"

  prelude5Version = Utils.unsafeVersion "5.0.0"
  leafDep4Version = Utils.unsafeVersion "4.0.0"
  leafDep6Version = Utils.unsafeVersion "6.0.0"
  midPkgVersion = Utils.unsafeVersion "1.0.0"

  prelude5Manifest = Utils.unsafeManifest "prelude" "5.0.0" []
  leafDep4Manifest = Utils.unsafeManifest "leaf-dep" "4.0.0"
    [ Tuple "prelude" ">=4.0.0 <6.0.0" ]
  leafDep6Manifest = Utils.unsafeManifest "leaf-dep" "6.0.0"
    [ Tuple "prelude" ">=6.0.0 <7.0.0" ]
  midPkgManifest = Utils.unsafeManifest "mid-pkg" "1.0.0"
    [ Tuple "leaf-dep" ">=4.0.0 <7.0.0" ]
  topPkgManifest = Utils.unsafeManifest "top-pkg" "1.0.0"
    [ Tuple "mid-pkg" ">=1.0.0 <2.0.0"
    , Tuple "prelude" ">=6.0.0 <7.0.0"
    ]

  depsOf (Manifest m) = m.dependencies

  addCompilerToMetadata :: Ref (Map PackageName Metadata) -> PackageName -> Version -> Version -> Effect Unit
  addCompilerToMetadata ref name version compiler = do
    Ref.modify_ (Map.update (Just <<< addCompiler version compiler) name) ref

  addCompiler :: Version -> Version -> Metadata -> Metadata
  addCompiler version compiler (Metadata m) = Metadata $ m
    { published = Map.update
        ( \entry -> Just $ entry
            { compilers = Utils.unsafeNonEmptyArray
                (NonEmptyArray.toArray entry.compilers <> [ compiler ])
            }
        )
        version
        m.published
    }

  runSolver solverData index metadata = liftAff do
    indexRef <- liftEffect $ Ref.new index
    metadataRef <- liftEffect $ Ref.new metadata
    runRegistryMock metadataRef indexRef
      $ MatrixBuilder.solveDependantsForCompiler solverData
