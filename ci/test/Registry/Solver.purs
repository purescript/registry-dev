module Test.Registry.Solver
  ( spec
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (uncurry)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Solver (SolverError(..), SolverPosition(..), printSolverError, solve)
import Registry.Version (ParseMode(..), Range, Version)
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

-- Other cleanup:
--   - Make tests into individual runs so that one failure is one test. Move
--     solver index out so that packages get reused throughout the tests.
--
-- Other test cases:
--   - Solves real-world example(?) such as transformers. Snapshot the manifests
--     from the registry index for core libraries at a specific version, use
--     that in the fixtures? Then we can easily write things more involved than
--     unit tests.

spec :: Spec.Spec Unit
spec = do
  let
    shouldSucceed goals result =
      solve solverIndex (Map.fromFoldable goals) `Assert.shouldContain` (Map.fromFoldable result)

    shouldFail goals errors = case solve solverIndex (Map.fromFoldable goals) of
      Left solverErrors -> do
        let expectedErrorCount = Array.length errors
        let receivedErrorCount = NonEmptyArray.length solverErrors

        when (expectedErrorCount /= receivedErrorCount) do
          Assert.fail $ "Tests expect " <> show expectedErrorCount <> " errors, but received " <> show receivedErrorCount

        let receivedErrors = map (\error -> { error, message: printSolverError error }) solverErrors
        let combinedErrors = Array.zip errors (NonEmptyArray.toArray receivedErrors)

        for_ combinedErrors \(Tuple expected received) -> do
          received.error `Assert.shouldEqual` expected.error
          received.message `Assert.shouldEqual` expected.message

      Right value ->
        Assert.fail $ "Expected failure, but received: " <> show value

    solverIndex :: Map PackageName (Map Version (Map PackageName Range))
    solverIndex = Map.fromFoldableWith (Map.unionWith Map.union) $ (map <<< map) (uncurry Map.singleton)
      -- simple and prelude have corresponding versions 0.0.0 and 1.0.0
      [ package "simple" /\ version 0 /\ Map.fromFoldable [ package "prelude" /\ range 0 1 ]
      , package "simple" /\ version 1 /\ Map.fromFoldable [ package "prelude" /\ range 1 2 ]
      , package "prelude" /\ version 0 /\ Map.empty
      , package "prelude" /\ version 1 /\ Map.empty
      -- only depends on simple@0.0.0
      , package "only-simple" /\ version 0 /\ Map.fromFoldable [ package "simple" /\ range 0 1 ]
      -- packages that are broken and fixed at different versions
      , package "broken-fixed" /\ version 0 /\ Map.fromFoldable [ package "does-not-exist" /\ range 0 4 ]
      , package "broken-fixed" /\ version 1 /\ Map.fromFoldable [ package "prelude" /\ range 0 1 ]
      , package "fixed-broken" /\ version 0 /\ Map.fromFoldable [ package "prelude" /\ range 0 1 ]
      , package "fixed-broken" /\ version 1 /\ Map.fromFoldable [ package "prelude" /\ range 1 2 ]
      , package "fixed-broken" /\ version 2 /\ Map.fromFoldable [ package "does-not-exist" /\ range 0 5 ]
      -- packages that are broken at all versions
      , package "broken-broken" /\ version 0 /\ Map.fromFoldable [ package "does-not-exist" /\ range 0 5 ]
      , package "broken-broken" /\ version 1 /\ Map.fromFoldable [ package "does-not-exist" /\ range 0 5 ]
      , package "transitive-broken1" /\ version 0 /\ Map.fromFoldable [ package "fixed-broken" /\ range 2 3 ]
      , package "transitive-broken2" /\ version 0 /\ Map.fromFoldable [ package "broken-fixed" /\ range 0 1 ]
      ]

  Spec.describe "Valid dependency ranges" do
    Spec.it "Solves simple range" do
      shouldSucceed
        [ package "simple" /\ range 0 1 ]
        [ package "simple" /\ version 0, package "prelude" /\ version 0 ]

    Spec.it "Chooses range with highest SemVer versions from several solution" do
      shouldSucceed
        [ package "simple" /\ range 0 2 ]
        [ package "simple" /\ version 1, package "prelude" /\ version 1 ]

    Spec.it "Solves for multiple packages" do
      shouldSucceed
        [ package "prelude" /\ range 0 2, package "simple" /\ range 0 2 ]
        [ package "simple" /\ version 1, package "prelude" /\ version 1 ]

  Spec.describe "Valid ranges with small intersection" do
    Spec.it "Tight bound on 'prelude@0'" do
      shouldSucceed
        [ package "simple" /\ range 0 2 -- loose
        , package "prelude" /\ range 0 1 -- tight
        ]
        [ package "simple" /\ version 0
        , package "prelude" /\ version 0
        ]

    Spec.it "Tight bound on 'prelude@1'" do
      shouldSucceed
        [ package "simple" /\ range 0 2 -- loose
        , package "prelude" /\ range 1 2 -- tight
        ]
        [ package "simple" /\ version 1
        , package "prelude" /\ version 1
        ]

    Spec.it "Tight bound on 'simple@0'" do
      shouldSucceed
        [ package "prelude" /\ range 0 2 -- loose
        , package "simple" /\ range 0 1 -- tight
        ]
        [ package "simple" /\ version 0
        , package "prelude" /\ version 0
        ]

    Spec.it "Tight bound on 'simple@1'" do
      shouldSucceed
        [ package "prelude" /\ range 0 2 -- loose
        , package "simple" /\ range 1 2 -- tight
        ]
        [ package "simple" /\ version 1
        , package "prelude" /\ version 1
        ]

  Spec.describe "Valid dependency ranges containing some invalid versions solve" do
    Spec.it "Proceeds past broken ranges to find a later valid range" do
      -- 'broken-fixed' cannot be solved at the broken version 0, but it can be
      -- solved at the fixed version 1.
      shouldSucceed
        [ package "broken-fixed" /\ range 0 2 ]
        [ package "broken-fixed" /\ version 1, package "prelude" /\ version 0 ]

    Spec.it "Backtracks from broken ranges to find the highest valid range." do
      -- 'fixed-broken' works at version 0 and 1, but is broken at version 2.
      shouldSucceed
        [ package "fixed-broken" /\ range 0 2 ]
        [ package "fixed-broken" /\ version 1, package "prelude" /\ version 1 ]

  Spec.describe "Does not solve when no versions exist for the specified range" do
    Spec.it "No versions available for target package" do
      shouldFail
        [ package "does-not-exist" /\ range 0 4 ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) SolveRoot
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none)"
          }
        ]

    Spec.it "Target package has versions, but none in range" do
      shouldFail
        [ package "prelude" /\ range 20 50 ]
        [ { error: NoVersionsInRange (package "prelude") (Set.fromFoldable [ version 0, version 1 ]) (range 20 50) SolveRoot
          , message: "Package index contained no versions for prelude in the range >=20.0.0 <50.0.0 (existing versions: 0.0.0, 1.0.0)"
          }
        ]

    Spec.it "Direct dependency of target package has no versions in range." do
      shouldFail
        [ package "broken-fixed" /\ range 0 1 ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) (Solving (package "broken-fixed") (version 0) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0"
          }
        ]

    Spec.it "Nested dependency of target package has no versions in range." do
      shouldFail
        [ package "transitive-broken1" /\ range 0 1 ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving (package "fixed-broken") (version 2) (Solving (package "transitive-broken1") (version 0) SolveRoot))
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@2.0.0 while solving transitive-broken1@0.0.0"
          }
        ]

  Spec.describe "Does not solve when ranges do not intersect" do
    Spec.it "Simple disjoint ranges" do
      shouldFail
        [ package "simple" /\ range 0 1, package "prelude" /\ range 1 2 ]
        [ { error: VersionNotInRange (package "prelude") (version 1) (range 0 1) (Solving (package "simple") (version 0) SolveRoot)
          , message: "Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0"
          }
        ]

    -- only-simple depends on simple@0, which is incompatible with the prelude
    -- range provided
    Spec.it "Transitive disjoint ranges" do
      shouldFail
        [ package "only-simple" /\ range 0 4
        , package "prelude" /\ range 1 2
        ]
        [ { error: VersionNotInRange (package "prelude") (version 1) (range 0 1) (Solving (package "simple") (version 0) (Solving (package "only-simple") (version 0) SolveRoot))
          , message: "Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0 while solving only-simple@0.0.0"
          }
        ]

  Spec.describe "Reports multiple errors" do
    Spec.it "Fails when target package cannot be satisfied" do
      shouldFail
        [ package "broken-broken" /\ range 0 2 ]
        ( [ version 1, version 0 ] <#> \brokenVersion ->
            { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving (package "broken-broken") brokenVersion SolveRoot)
            , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving broken-broken@" <> Version.printVersion brokenVersion
            }
        )

    Spec.it "Fails on disjoint ranges" do
      shouldFail
        [ package "broken-fixed" /\ range 0 1
        , package "fixed-broken" /\ range 2 3
        ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) (Solving (package "broken-fixed") (version 0) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0"
          }
        , { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving (package "fixed-broken") (version 2) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@2.0.0"
          }
        ]

package :: String -> PackageName
package = unsafeFromRight <<< PackageName.parse

version :: Int -> Version
version = unsafeFromRight <<< Version.parseVersion Strict <<< (_ <> ".0.0") <<< show

-- For all these tests, we work with major versions only because we do not
-- need to exercise the intricacies of the range relations, just the solver,
-- which does not care about what versions are, just how they relate
range :: Int -> Int -> Range
range lower upper = unsafeFromRight $ Version.parseRange Strict $ Array.fold
  [ ">="
  , show lower
  , ".0.0 <"
  , show upper
  , ".0.0"
  ]
