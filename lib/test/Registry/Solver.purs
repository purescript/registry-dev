module Test.Registry.Solver (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int as Int
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un, wrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Data.Set.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Solver (Intersection(..), LocalSolverPosition(..), SolverError(..), SolverPosition(..), Sourced(..), buildCompilerIndex, initializeRegistry, initializeRequired, lowerBound, printSolverError, solve, solveSeed, solveSteps, solveWithCompiler, updateCompilerIndex, upperBound)
import Registry.Test.Assert as Assert
import Registry.Test.Utils (fromJust, fromRight, unsafeManifest, unsafeMetadata, unsafeNonEmptyArray, unsafePackageName, unsafeRange, unsafeVersion)
import Registry.Types (PackageName, Range, Version)
import Registry.Version as Version
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  let
    shouldSucceed goals result = pure unit >>= \_ ->
      solve solverIndex (Map.fromFoldable goals) `Assert.shouldContain` (Map.fromFoldable result)

    shouldSucceedSteps goals result = pure unit >>= \_ -> do
      let solved = solveSteps (solveSeed { registry: initializeRegistry solverIndex, required: initializeRequired (Map.fromFoldable goals) })
      let toRange intersect = fromJust "Bad intersection" (Range.mk (lowerBound intersect) (upperBound intersect))
      map toRange (un SemigroupMap solved.required) `Assert.shouldEqual` Map.fromFoldable result

    shouldFail goals errors = pure unit >>= \_ -> case solve solverIndex (Map.fromFoldable goals) of
      Left solverErrors -> do
        let expectedErrorCount = Array.length errors
        let receivedErrorCount = NonEmptyList.length solverErrors

        when (expectedErrorCount /= receivedErrorCount) do
          if expectedErrorCount == 0 then Assert.fail $ "Error(s): " <> intercalateMap "\n" printSolverError solverErrors
          else Assert.fail $ "Tests expect " <> show expectedErrorCount <> " errors, but received " <> show receivedErrorCount

        let receivedErrors = map (\error -> { error, message: printSolverError error }) solverErrors
        let combinedErrors = Array.zip errors (Array.fromFoldable receivedErrors)

        for_ combinedErrors \(Tuple expected received) -> do
          received.message `Assert.shouldEqual` expected.message
          received.error `Assert.shouldEqual` expected.error

      Right value ->
        Assert.fail $ "Expected failure, but received: " <> foldMapWithIndex (\p v -> "\n" <> PackageName.print p <> ": " <> Version.print v) value

  Spec.describe "Valid dependency ranges" do
    Spec.it "Solves simple range" do
      shouldSucceed
        [ simple.package /\ unsafeRange ">=0.0.0 <1.0.0" ]
        [ simple.package /\ unsafeVersion "0.0.0", prelude.package /\ unsafeVersion "0.0.0" ]

    Spec.it "Chooses range with highest SemVer versions from several solution" do
      shouldSucceed
        [ simple.package /\ unsafeRange ">=0.0.0 <2.0.0" ]
        [ simple.package /\ unsafeVersion "1.0.0", prelude.package /\ unsafeVersion "1.0.0" ]

    Spec.it "Solves for multiple packages" do
      shouldSucceed
        [ prelude.package /\ unsafeRange ">=0.0.0 <2.0.0", simple.package /\ unsafeRange ">=0.0.0 <2.0.0" ]
        [ simple.package /\ unsafeVersion "1.0.0", prelude.package /\ unsafeVersion "1.0.0" ]

  Spec.describe "Valid ranges with small intersection" do
    Spec.it "Tight bound on 'prelude@0'" do
      shouldSucceed
        [ simple.package /\ unsafeRange ">=0.0.0 <2.0.0" -- loose
        , prelude.package /\ unsafeRange ">=0.0.0 <1.0.0" -- tight
        ]
        [ simple.package /\ unsafeVersion "0.0.0"
        , prelude.package /\ unsafeVersion "0.0.0"
        ]

    Spec.it "Tight bound on 'prelude@1'" do
      shouldSucceed
        [ simple.package /\ unsafeRange ">=0.0.0 <2.0.0" -- loose
        , prelude.package /\ unsafeRange ">=1.0.0 <2.0.0" -- tight
        ]
        [ simple.package /\ unsafeVersion "1.0.0"
        , prelude.package /\ unsafeVersion "1.0.0"
        ]

    Spec.it "Tight bound on 'simple@0'" do
      shouldSucceed
        [ prelude.package /\ unsafeRange ">=0.0.0 <2.0.0" -- loose
        , simple.package /\ unsafeRange ">=0.0.0 <1.0.0" -- tight
        ]
        [ simple.package /\ unsafeVersion "0.0.0"
        , prelude.package /\ unsafeVersion "0.0.0"
        ]

    Spec.it "Tight bound on 'simple@1'" do
      shouldSucceed
        [ prelude.package /\ unsafeRange ">=0.0.0 <2.0.0" -- loose
        , simple.package /\ unsafeRange ">=1.0.0 <2.0.0" -- tight
        ]
        [ simple.package /\ unsafeVersion "1.0.0"
        , prelude.package /\ unsafeVersion "1.0.0"
        ]

  Spec.describe "Single-step expands bounds" do
    Spec.it "Simple range" do
      shouldSucceedSteps
        [ simple.package /\ unsafeRange ">=0.0.0 <1.0.0" ]
        [ simple.package /\ unsafeRange ">=0.0.0 <1.0.0", prelude.package /\ unsafeRange ">=0.0.0 <1.0.0" ]

    Spec.it "Multi-version range" do
      shouldSucceedSteps
        [ simple.package /\ unsafeRange ">=0.0.0 <2.0.0" ]
        [ simple.package /\ unsafeRange ">=0.0.0 <2.0.0", prelude.package /\ unsafeRange ">=0.0.0 <2.0.0" ]

    Spec.it "Transitive" do
      shouldSucceedSteps
        [ onlySimple.package /\ unsafeRange ">=0.0.0 <1.0.0" ]
        [ onlySimple.package /\ unsafeRange ">=0.0.0 <1.0.0", simple.package /\ unsafeRange ">=0.0.0 <1.0.0", prelude.package /\ unsafeRange ">=0.0.0 <1.0.0" ]

  Spec.describe "Valid dependency ranges containing some invalid versions solve" do
    Spec.it "Proceeds past broken ranges to find a later valid range" do
      -- 'broken-fixed' cannot be solved at the broken version 0, but it can be
      -- solved at the fixed version 1.
      shouldSucceed
        [ brokenFixed.package /\ unsafeRange ">=0.0.0 <2.0.0" ]
        [ brokenFixed.package /\ unsafeVersion "1.0.0", prelude.package /\ unsafeVersion "0.0.0" ]

    Spec.it "Backtracks from broken ranges to find the highest valid range." do
      -- 'fixed-broken' works at version 0 and 1, but is broken at version 2.
      shouldSucceed
        [ fixedBroken.package /\ unsafeRange ">=0.0.0 <3.0.0" ]
        [ fixedBroken.package /\ unsafeVersion "1.0.0", prelude.package /\ unsafeVersion "1.0.0" ]

  Spec.describe "Does not solve when no versions exist for the specified range" do
    Spec.it "No versions available for target package" do
      shouldFail
        [ unsafePackageName "does-not-exist" /\ unsafeRange ">=0.0.0 <4.0.0" ]
        [ { error: Conflicts $ Map.fromFoldable
              [ unsafePackageName "does-not-exist" `Tuple` intersection 0 (solveRoot "does-not-exist") 4 (solveRoot "does-not-exist") ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 (declared dependency)\n  <4.0.0 (declared dependency)"
          }
        ]

    Spec.it "Target package has versions, but none in range" do
      shouldFail
        [ prelude.package /\ unsafeRange ">=20.0.0 <50.0.0" ]
        [ { error: Conflicts $ Map.fromFoldable
              [ unsafePackageName "prelude" `Tuple` intersection 20 (solveRoot "prelude") 50 (solveRoot "prelude") ]
          , message: "No versions found in the registry for prelude in range\n  >=20.0.0 (declared dependency)\n  <50.0.0 (declared dependency)"
          }
        ]

    Spec.it "Direct dependency of target package has no versions in range." do
      shouldFail
        [ brokenFixed.package /\ unsafeRange ">=0.0.0 <1.0.0" ]
        [ { error: Conflicts $ Map.fromFoldable
              [ unsafePackageName "does-not-exist" `Tuple` intersection 0 (via' "broken-fixed" 0) 4 (via' "broken-fixed" 0)
              ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in broken-fixed@0.0.0\n  <4.0.0 seen in broken-fixed@0.0.0"
          }
        ]

    Spec.it "Nested dependency of target package has no versions in range." do
      shouldFail
        [ transitiveBroken.package /\ unsafeRange ">=0.0.0 <1.0.0" ]
        [ { error: Conflicts $ Map.fromFoldable
              [ unsafePackageName "does-not-exist" `Tuple` intersection 0 (via "fixed-broken" 2 [ "transitive-broken" ]) 5 (via "fixed-broken" 2 [ "transitive-broken" ])
              ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in fixed-broken@2.0.0 from declared dependencies transitive-broken\n  <5.0.0 seen in fixed-broken@2.0.0 from declared dependencies transitive-broken"
          }
        ]

    Spec.it "Fails when target package cannot be satisfied" do
      shouldFail
        [ brokenBroken.package /\ unsafeRange ">=0.0.0 <2.0.0" ]
        [ { error: Conflicts $ Map.fromFoldable
              [ unsafePackageName "does-not-exist" `Tuple` intersection 0 (via' "broken-broken" 0 <> via' "broken-broken" 1) 5 (via' "broken-broken" 0 <> via' "broken-broken" 1)
              ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in broken-broken@0.0.0, broken-broken@1.0.0\n  <5.0.0 seen in broken-broken@0.0.0, broken-broken@1.0.0"
          }
        ]

  Spec.describe "Does not solve when ranges do not intersect" do
    Spec.it "Simple disjoint ranges" do
      shouldFail
        [ simple.package /\ unsafeRange ">=0.0.0 <1.0.0", prelude.package /\ unsafeRange ">=1.0.0 <2.0.0" ]
        [ { error: WhileSolving (unsafePackageName "simple") $ Map.singleton (unsafeVersion "0.0.0") $ Conflicts $ Map.fromFoldable
              [ unsafePackageName "prelude" `Tuple` intersection 1 (solveRoot "prelude") 1 (via "simple" 0 [])
              ]
          , message: "While solving simple each version could not be solved:\n- 0.0.0: \n  Conflict in version ranges for prelude:\n    >=1.0.0 (declared dependency)\n    <1.0.0 seen in simple@0.0.0"
          }
        ]

    -- only-simple depends on simple@0, which is incompatible with the prelude
    -- range provided
    Spec.it "Transitive disjoint ranges" do
      shouldFail
        [ onlySimple.package /\ unsafeRange ">=0.0.0 <4.0.0"
        , prelude.package /\ unsafeRange ">=1.0.0 <2.0.0"
        ]
        [ { error: WhileSolving (unsafePackageName "simple") $ Map.singleton (unsafeVersion "0.0.0") $ Conflicts $ Map.fromFoldable
              [ unsafePackageName "prelude" `Tuple` intersection 1 (solveRoot "prelude") 1 (via "simple" 0 [])
              ]
          , message: "While solving simple each version could not be solved:\n- 0.0.0: \n  Conflict in version ranges for prelude:\n    >=1.0.0 (declared dependency)\n    <1.0.0 seen in simple@0.0.0"
          }
        ]

    Spec.it "Fails on disjoint ranges" do
      shouldFail
        [ brokenFixed.package /\ unsafeRange ">=0.0.0 <1.0.0"
        , fixedBroken.package /\ unsafeRange ">=2.0.0 <3.0.0"
        ]
        [ { error: Conflicts $ Map.fromFoldable
              [ unsafePackageName "does-not-exist" `Tuple` intersection 0 (via' "broken-fixed" 0 <> via' "fixed-broken" 2) 4 (via' "broken-fixed" 0)
              ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in broken-fixed@0.0.0, fixed-broken@2.0.0\n  <4.0.0 seen in broken-fixed@0.0.0"
          }
        ]

  Spec.describe "Reports multiple errors" do
    Spec.it "Reports different errors for different versions" do
      shouldFail
        [ chaotic.package /\ unsafeRange ">=1.0.0 <3.0.0"
        , prelude.package /\ unsafeRange ">=2.0.0 <4.0.0"
        ]
        [ { error: WhileSolving (unsafePackageName "chaotic") $ Map.fromFoldable
              [ Tuple (unsafeVersion "1.0.0") $ Conflicts $ Map.fromFoldable
                  [ unsafePackageName "prelude" `Tuple` intersection 2 (solveRoot "prelude") 1 (via "chaotic" 1 [])
                  ]
              , Tuple (unsafeVersion "2.0.0") $ Conflicts $ Map.fromFoldable
                  -- TODO: why no global "chaotic" here?
                  [ unsafePackageName "prelude" `Tuple` intersection 5 (via "chaotic" 2 []) 4 (solveRoot "prelude")
                  ]
              ]
          , message: "While solving chaotic each version could not be solved:\n- 1.0.0: \n  Conflict in version ranges for prelude:\n    >=2.0.0 (declared dependency)\n    <1.0.0 seen in chaotic@1.0.0\n- 2.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in chaotic@2.0.0\n    <4.0.0 (declared dependency)"
          }
        ]

    Spec.it "Groups same errors" do
      shouldFail
        [ chaotic.package /\ unsafeRange ">=1.0.0 <4.0.0"
        , prelude.package /\ unsafeRange ">=2.0.0 <4.0.0"
        ]
        [ { error: WhileSolving (unsafePackageName "chaotic") $ Map.fromFoldable
              [ Tuple (unsafeVersion "1.0.0") $ Conflicts $ Map.fromFoldable
                  [ unsafePackageName "prelude" `Tuple` intersection 2 (solveRoot "prelude") 1 (via "chaotic" 1 [])
                  ]
              , Tuple (unsafeVersion "2.0.0") $ Conflicts $ Map.fromFoldable
                  -- TODO: why no global "chaotic" here?
                  [ unsafePackageName "prelude" `Tuple` intersection 5 (via "chaotic" 2 []) 4 (solveRoot "prelude")
                  ]
              , Tuple (unsafeVersion "3.0.0") $ Conflicts $ Map.fromFoldable
                  -- TODO: why no global "chaotic" here?
                  [ unsafePackageName "prelude" `Tuple` intersection 5 (via "chaotic" 3 []) 4 (solveRoot "prelude")
                  ]
              ]
          , message: "While solving chaotic each version could not be solved:\n- 1.0.0: \n  Conflict in version ranges for prelude:\n    >=2.0.0 (declared dependency)\n    <1.0.0 seen in chaotic@1.0.0\n- 2.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in chaotic@2.0.0\n    <4.0.0 (declared dependency)\n- 3.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in chaotic@3.0.0\n    <4.0.0 (declared dependency)"
          }
        ]

    -- since we try packages in alphabetical order
    Spec.it "Reports different errors for different versions (converse)" do
      shouldFail
        [ qaotic.package /\ unsafeRange ">=1.0.0 <3.0.0"
        , prelude.package /\ unsafeRange ">=2.0.0 <4.0.0"
        ]
        [ { error: WhileSolving (unsafePackageName "qaotic") $ Map.fromFoldable
              [ Tuple (unsafeVersion "1.0.0") $ Conflicts $ Map.fromFoldable
                  [ unsafePackageName "prelude" `Tuple` intersection 2 (solveRoot "prelude") 1 (via "qaotic" 1 [])
                  ]
              , Tuple (unsafeVersion "2.0.0") $ Conflicts $ Map.fromFoldable
                  -- TODO: why no global "prelude" here?
                  [ unsafePackageName "prelude" `Tuple` intersection 5 (via "qaotic" 2 []) 4 (solveRoot "prelude")
                  ]
              ]
          , message: "While solving qaotic each version could not be solved:\n- 1.0.0: \n  Conflict in version ranges for prelude:\n    >=2.0.0 (declared dependency)\n    <1.0.0 seen in qaotic@1.0.0\n- 2.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in qaotic@2.0.0\n    <4.0.0 (declared dependency)"
          }
        ]

  Spec.describe "Resolution contracts" do
    Spec.it "Excludes broken dependencies belonging only to rejected versions" do
      let
        index = Map.fromFoldable
          [ unsafePackageName "a" /\ Map.fromFoldable
              [ unsafeVersion "1.0.0" /\ Map.singleton (unsafePackageName "orphan") (unsafeRange ">=1.0.0 <2.0.0")
              , unsafeVersion "2.0.0" /\ Map.empty
              ]
          , unsafePackageName "orphan" /\ Map.singleton (unsafeVersion "1.0.0") (Map.singleton (unsafePackageName "missing") (unsafeRange ">=1.0.0 <2.0.0"))
          ]
      solve index (Map.singleton (unsafePackageName "a") (unsafeRange ">=1.0.0 <3.0.0"))
        `Assert.shouldContain` Map.singleton (unsafePackageName "a") (unsafeVersion "2.0.0")

    Spec.it "Reports both originally requested packages and the package version directly requiring the missing dependency" do
      let
        index = Map.fromFoldable
          [ unsafePackageName "a" /\ Map.singleton (unsafeVersion "1.0.0") (Map.singleton (unsafePackageName "middle") (unsafeRange ">=2.0.0 <5.0.0"))
          , unsafePackageName "b" /\ Map.singleton (unsafeVersion "1.0.0") (Map.singleton (unsafePackageName "middle") (unsafeRange ">=1.0.0 <4.0.0"))
          , unsafePackageName "middle" /\ Map.singleton (unsafeVersion "2.0.0") (Map.singleton (unsafePackageName "missing") (unsafeRange ">=1.0.0 <2.0.0"))
          ]
        goals = Map.fromFoldable [ unsafePackageName "a" /\ unsafeRange ">=1.0.0 <2.0.0", unsafePackageName "b" /\ unsafeRange ">=1.0.0 <2.0.0" ]
        source = via "middle" 2 [ "a", "b" ]
      solve index goals `Assert.shouldEqual`
        Left (pure (Conflicts (Map.singleton (unsafePackageName "missing") (intersection 1 source 2 source))))

    Spec.it "Prefers the alphabetically first package, not the largest total versions" do
      let
        index = Map.fromFoldable
          [ unsafePackageName "a" /\ Map.fromFoldable
              [ unsafeVersion "1.0.0" /\ Map.singleton (unsafePackageName "z") (unsafeRange ">=1.0.0 <2.0.0")
              , unsafeVersion "2.0.0" /\ Map.singleton (unsafePackageName "z") (unsafeRange ">=2.0.0 <3.0.0")
              ]
          , unsafePackageName "b" /\ Map.fromFoldable
              [ unsafeVersion "1.0.0" /\ Map.singleton (unsafePackageName "z") (unsafeRange ">=2.0.0 <3.0.0")
              , unsafeVersion "9.0.0" /\ Map.singleton (unsafePackageName "z") (unsafeRange ">=1.0.0 <2.0.0")
              ]
          , unsafePackageName "z" /\ Map.fromFoldable [ unsafeVersion "1.0.0" /\ Map.empty, unsafeVersion "2.0.0" /\ Map.empty ]
          ]
      solve index (Map.fromFoldable [ unsafePackageName "a" /\ unsafeRange ">=1.0.0 <3.0.0", unsafePackageName "b" /\ unsafeRange ">=1.0.0 <10.0.0" ])
        `Assert.shouldContain` Map.fromFoldable [ unsafePackageName "a" /\ unsafeVersion "2.0.0", unsafePackageName "b" /\ unsafeVersion "1.0.0", unsafePackageName "z" /\ unsafeVersion "2.0.0" ]

    Spec.it "Matches exhaustive assignments on 4096 small cyclic version graphs" do
      -- Each of six package versions independently has no dependency, an exact
      -- low/high dependency, or a broad dependency on the next package in a
      -- three-package ring. Enumerating assignments (including absent packages)
      -- is independent of the solver's propagation and backtracking algorithm.
      let
        names = map unsafePackageName [ "a", "b", "c" ]

        assignments = do
          a <- [ Nothing, Just (unsafeVersion "1.0.0"), Just (unsafeVersion "2.0.0") ]
          b <- [ Nothing, Just (unsafeVersion "1.0.0"), Just (unsafeVersion "2.0.0") ]
          c <- [ Nothing, Just (unsafeVersion "1.0.0"), Just (unsafeVersion "2.0.0") ]
          pure $ Map.fromFoldable $ Array.catMaybes $ Array.zipWith (\p -> map (Tuple p)) names [ a, b, c ]

        goals = Map.singleton (unsafePackageName "a") (unsafeRange ">=1.0.0 <3.0.0")

        satisfies plan deps = all
          ( \(Tuple p bounds) -> case Map.lookup p plan of
              Nothing -> false
              Just v -> Range.includes bounds v
          )
          (Map.toUnfoldable deps :: Array (Tuple PackageName Range))

        valid index plan = satisfies plan goals && all
          ( \(Tuple p v) -> case Map.lookup p index >>= Map.lookup v of
              Nothing -> false
              Just deps -> satisfies plan deps
          )
          (Map.toUnfoldable plan :: Array (Tuple PackageName Version))

      for_ (Array.range 0 4095) \seed -> do
        let
          versionEntry i j = do
            let
              digit = (seed / Int.pow 4 (i * 2 + j - 1)) `mod` 4
              next = unsafePackageName (if i == 0 then "b" else if i == 1 then "c" else "a")
              deps = case digit of
                0 -> Map.empty
                1 -> Map.singleton next (unsafeRange ">=1.0.0 <2.0.0")
                2 -> Map.singleton next (unsafeRange ">=2.0.0 <3.0.0")
                _ -> Map.singleton next (unsafeRange ">=1.0.0 <3.0.0")

            majorVersion j /\ deps

          versions i = Map.fromFoldable $ map (versionEntry i) [ 1, 2 ]

          index = Map.fromFoldable $ Array.mapWithIndex (\i p -> p /\ versions i) names

        case solve index goals of
          Left _ -> when (Array.any (valid index) assignments) $ Assert.fail $ "False unsatisfiability for graph " <> show seed
          Right plan -> unless (valid index plan) $ Assert.fail $ "Invalid resolution for graph " <> show seed

  Spec.describe "CompilerIndex" do
    let
      -- Package graph:
      --   prelude@6.0.1  (no deps)
      --   effect@4.0.0   (depends on prelude >=6.0.0 <7.0.0)
      --   effect@5.0.0   (depends on prelude >=6.0.0 <7.0.0)
      --   my-pkg@1.0.0   (depends on prelude >=6.0.0 <7.0.0, effect >=4.0.0 <5.0.0)
      preludeManifest = unsafeManifest "prelude" "6.0.1" []
      effectManifest = unsafeManifest "effect" "4.0.0" [ Tuple "prelude" ">=6.0.0 <7.0.0" ]
      effect5Manifest = unsafeManifest "effect" "5.0.0" [ Tuple "prelude" ">=6.0.0 <7.0.0" ]
      myPkgManifest = unsafeManifest "my-pkg" "1.0.0" [ Tuple "prelude" ">=6.0.0 <7.0.0", Tuple "effect" ">=4.0.0 <5.0.0" ]

      Manifest myPkg = myPkgManifest

      compilers = unsafeNonEmptyArray [ unsafeVersion "0.15.15", unsafeVersion "0.15.16" ]

      manifestIndex = fromRight "Failed to build ManifestIndex" do
        ManifestIndex.insert ManifestIndex.ConsiderRanges preludeManifest ManifestIndex.empty
          >>= ManifestIndex.insert ManifestIndex.ConsiderRanges effectManifest
          >>= ManifestIndex.insert ManifestIndex.ConsiderRanges effect5Manifest
          >>= ManifestIndex.insert ManifestIndex.ConsiderRanges myPkgManifest

      -- All packages present, only support 0.15.15
      oldCompilerMetadata = Map.fromFoldable
        [ unsafeMetadata "prelude" [ Tuple "6.0.1" [ "0.15.15" ] ]
        , unsafeMetadata "effect"
            [ Tuple "4.0.0" [ "0.15.15" ]
            , Tuple "5.0.0" [ "0.15.15" ]
            ]
        , unsafeMetadata "my-pkg" [ Tuple "1.0.0" [ "0.15.15" ] ]
        ]

      -- All packages present, support both compilers
      bothCompilersMetadata = Map.fromFoldable
        [ unsafeMetadata "prelude" [ Tuple "6.0.1" [ "0.15.15", "0.15.16" ] ]
        , unsafeMetadata "effect"
            [ Tuple "4.0.0" [ "0.15.15", "0.15.16" ]
            , Tuple "5.0.0" [ "0.15.15", "0.15.16" ]
            ]
        , unsafeMetadata "my-pkg" [ Tuple "1.0.0" [ "0.15.15", "0.15.16" ] ]
        ]

    Spec.it "Complete metadata rejects incompatible compiler" do
      let compilerIndex = buildCompilerIndex compilers manifestIndex oldCompilerMetadata
      case solveWithCompiler (Range.exact (unsafeVersion "0.15.16")) compilerIndex myPkg.dependencies of
        Left _ -> pure unit -- expected: effect's purs range >=0.15.15 <0.15.16 excludes 0.15.16
        Right _ ->
          Assert.fail "Expected solver to reject build plan where effect only supports 0.15.15"

    Spec.it "Complete metadata accepts compatible compiler" do
      let compilerIndex = buildCompilerIndex compilers manifestIndex bothCompilersMetadata
      case solveWithCompiler (Range.exact (unsafeVersion "0.15.16")) compilerIndex myPkg.dependencies of
        Left errs ->
          Assert.fail $ "Expected solver to succeed but it failed:\n" <> foldMapWithIndex
            (\i error -> "[Error " <> show (i + 1) <> "] " <> printSolverError error <> "\n")
            errs
        Right (Tuple _solvedCompiler resolutions) -> do
          Map.lookup (unsafePackageName "prelude") resolutions `Assert.shouldEqual` Just (unsafeVersion "6.0.1")
          Map.lookup (unsafePackageName "effect") resolutions `Assert.shouldEqual` Just (unsafeVersion "4.0.0")

    Spec.it "Prefers the newer package version unless the compiler requirement forces an older one" do
      let
        metadata = Map.fromFoldable
          [ unsafeMetadata "prelude" [ Tuple "6.0.1" [ "0.15.15", "0.15.16" ] ]
          , unsafeMetadata "effect"
              [ Tuple "4.0.0" [ "0.15.16" ]
              , Tuple "5.0.0" [ "0.15.15" ]
              ]
          , unsafeMetadata "my-pkg" [ Tuple "1.0.0" [ "0.15.15" ] ]
          ]
        index = buildCompilerIndex compilers manifestIndex metadata
        goals = Map.singleton (unsafePackageName "effect") (unsafeRange ">=4.0.0 <6.0.0")
        expected compiler effect = Tuple (unsafeVersion compiler) $ Map.fromFoldable
          [ unsafePackageName "effect" /\ unsafeVersion effect
          , unsafePackageName "prelude" /\ unsafeVersion "6.0.1"
          ]
      solveWithCompiler (Range.exact (unsafeVersion "0.15.16")) index goals
        `Assert.shouldContain` expected "0.15.16" "4.0.0"
      solveWithCompiler (unsafeRange ">=0.15.15 <0.15.17") index goals
        `Assert.shouldContain` expected "0.15.15" "5.0.0"

    Spec.it "updateCompilerIndex produces same result as full rebuild" do
      -- Setup: effect has two versions (4.0.0 and 5.0.0) both supporting
      -- only 0.15.15. The incremental update adds 0.15.16 to effect@4.0.0;
      -- effect@5.0.0 must be left untouched. Equivalence with a full
      -- rebuild verifies both that the updated entry matches and that
      -- unrelated entries are preserved (e.g. a bug that inserted at the
      -- package-name level instead of the (name, version) level would
      -- drop effect@5.0.0 in the incremental).
      let
        fullOld = buildCompilerIndex compilers manifestIndex oldCompilerMetadata

        Tuple _ updatedEffectMeta = unsafeMetadata "effect"
          [ Tuple "4.0.0" [ "0.15.15", "0.15.16" ]
          , Tuple "5.0.0" [ "0.15.15" ]
          ]

        updatedMetadata = Map.fromFoldable
          [ unsafeMetadata "prelude" [ Tuple "6.0.1" [ "0.15.15" ] ]
          , Tuple (unsafePackageName "effect") updatedEffectMeta
          , unsafeMetadata "my-pkg" [ Tuple "1.0.0" [ "0.15.15" ] ]
          ]
        fullNew = buildCompilerIndex compilers manifestIndex updatedMetadata

        incremental = updateCompilerIndex fullOld effectManifest updatedEffectMeta

      incremental `Assert.shouldEqual` fullNew

solverIndex :: Map PackageName (Map Version (Map PackageName Range))
solverIndex = Map.fromFoldable $ map buildPkg
  -- simple and prelude have corresponding versions 0.0.0 and 1.0.0
  [ simple
  , prelude
  -- only depends on simple@0.0.0
  , onlySimple
  -- packages that are broken and fixed at different versions
  , brokenFixed
  , fixedBroken
  -- packages that are broken at all versions
  , brokenBroken
  , transitiveBroken
  -- packages with non-contiguous dependency ranges
  , chaotic
  , qaotic
  ]
  where
  buildPkg pkg = Tuple pkg.package (map buildVersion pkg.versions)
  buildVersion = case _ of
    Nothing -> Map.empty
    Just (Tuple a b) -> Map.singleton a b

type TestPackage =
  { package :: PackageName
  , versions :: Map Version (Maybe (Tuple PackageName Range))
  }

-- | Transitively depends on a broken range
transitiveBroken :: TestPackage
transitiveBroken =
  { package: unsafePackageName "transitive-broken"
  , versions: Map.fromFoldable
      [ unsafeVersion "0.0.0" /\ Just (fixedBroken.package /\ unsafeRange ">=2.0.0 <3.0.0")
      , unsafeVersion "1.0.0" /\ Just (brokenFixed.package /\ unsafeRange ">=0.0.0 <1.0.0")
      ]
  }

-- | Broken at v1 and v2
brokenBroken :: TestPackage
brokenBroken =
  { package: unsafePackageName "broken-broken"
  , versions: Map.fromFoldable
      [ unsafeVersion "0.0.0" /\ Just (unsafePackageName "does-not-exist" /\ unsafeRange ">=0.0.0 <5.0.0")
      , unsafeVersion "1.0.0" /\ Just (unsafePackageName "does-not-exist" /\ unsafeRange ">=0.0.0 <5.0.0")
      ]
  }

-- | Fixed at v0, v1, broken at v2
fixedBroken :: TestPackage
fixedBroken =
  { package: unsafePackageName "fixed-broken"
  , versions: Map.fromFoldable
      [ unsafeVersion "0.0.0" /\ Just (prelude.package /\ unsafeRange ">=0.0.0 <1.0.0")
      , unsafeVersion "1.0.0" /\ Just (prelude.package /\ unsafeRange ">=1.0.0 <2.0.0")
      , unsafeVersion "2.0.0" /\ Just (unsafePackageName "does-not-exist" /\ unsafeRange ">=0.0.0 <5.0.0")
      ]
  }

-- | Broken at v0, fixed at v1
brokenFixed :: TestPackage
brokenFixed =
  { package: unsafePackageName "broken-fixed"
  , versions: Map.fromFoldable
      [ unsafeVersion "0.0.0" /\ Just (unsafePackageName "does-not-exist" /\ unsafeRange ">=0.0.0 <4.0.0")
      , unsafeVersion "1.0.0" /\ Just (prelude.package /\ unsafeRange ">=0.0.0 <1.0.0")
      ]
  }

onlySimple :: TestPackage
onlySimple =
  { package: unsafePackageName "only-simple"
  , versions: Map.singleton (unsafeVersion "0.0.0") (Just (simple.package /\ unsafeRange ">=0.0.0 <1.0.0"))
  }

simple :: TestPackage
simple =
  { package: unsafePackageName "simple"
  , versions: Map.fromFoldable
      [ unsafeVersion "0.0.0" /\ Just (prelude.package /\ unsafeRange ">=0.0.0 <1.0.0")
      , unsafeVersion "1.0.0" /\ Just (prelude.package /\ unsafeRange ">=1.0.0 <2.0.0")
      ]
  }

prelude :: TestPackage
prelude =
  { package: unsafePackageName "prelude"
  , versions: Map.fromFoldable
      [ unsafeVersion "0.0.0" /\ Nothing
      , unsafeVersion "1.0.0" /\ Nothing
      , unsafeVersion "2.0.0" /\ Nothing
      , unsafeVersion "3.0.0" /\ Nothing
      , unsafeVersion "4.0.0" /\ Nothing
      , unsafeVersion "5.0.0" /\ Nothing
      ]
  }

chaotic :: TestPackage
chaotic =
  { package: unsafePackageName "chaotic"
  , versions: Map.fromFoldable
      [ unsafeVersion "1.0.0" /\ Just (prelude.package /\ unsafeRange ">=0.0.0 <1.0.0")
      , unsafeVersion "2.0.0" /\ Just (prelude.package /\ unsafeRange ">=5.0.0 <6.0.0")
      , unsafeVersion "3.0.0" /\ Just (prelude.package /\ unsafeRange ">=5.0.0 <6.0.0")
      ]
  }

qaotic :: TestPackage
qaotic =
  { package: unsafePackageName "qaotic"
  , versions: Map.fromFoldable
      [ unsafeVersion "1.0.0" /\ Just (prelude.package /\ unsafeRange ">=0.0.0 <1.0.0")
      , unsafeVersion "2.0.0" /\ Just (prelude.package /\ unsafeRange ">=5.0.0 <6.0.0")
      ]
  }

majorVersion :: Int -> Version
majorVersion = unsafeVersion <<< (_ <> ".0.0") <<< show

intersection :: Int -> SolverPosition -> Int -> SolverPosition -> Intersection
intersection lower lowerPos upper upperPos = Intersection
  { lower: wrap $ Sourced (majorVersion lower) lowerPos
  , upper: wrap $ Sourced (majorVersion upper) upperPos
  }

solveRoot :: String -> SolverPosition
solveRoot = Pos Root <<< Set.singleton <<< unsafePackageName

via :: String -> Int -> Array String -> SolverPosition
via p v = Pos (Solving (NES.singleton { package: unsafePackageName p, version: majorVersion v })) <<< Set.fromFoldable <<< map unsafePackageName

via' :: String -> Int -> SolverPosition
via' p v = Pos (Solving (NES.singleton { package: unsafePackageName p, version: majorVersion v })) (Set.singleton (unsafePackageName p))
