module Test.Registry.Solver (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NES
import Registry.App.Json as Json
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Solver (SolverError(..), SolverPosition(..), printSolverError, solve, solveAndValidate)
import Registry.Version (Version)
import Registry.Version as Version
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  let
    shouldSucceed goals result =
      solveAndValidate solverIndex (Map.fromFoldable goals) `Assert.shouldContain` (Map.fromFoldable result)

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
        Assert.fail $ Array.fold
          [ "Expected failure, but received: "
          , Json.stringifyJson (Internal.Codec.packageMap Version.codec) value
          ]

  Spec.describe "Valid dependency ranges" do
    Spec.it "Solves simple range" do
      shouldSucceed
        [ simple.package /\ range 0 1 ]
        [ simple.package /\ version 0, prelude.package /\ version 0 ]

    Spec.it "Chooses range with highest SemVer versions from several solution" do
      shouldSucceed
        [ simple.package /\ range 0 2 ]
        [ simple.package /\ version 1, prelude.package /\ version 1 ]

    Spec.it "Solves for multiple packages" do
      shouldSucceed
        [ prelude.package /\ range 0 2, simple.package /\ range 0 2 ]
        [ simple.package /\ version 1, prelude.package /\ version 1 ]

  Spec.describe "Valid ranges with small intersection" do
    Spec.it "Tight bound on 'prelude@0'" do
      shouldSucceed
        [ simple.package /\ range 0 2 -- loose
        , prelude.package /\ range 0 1 -- tight
        ]
        [ simple.package /\ version 0
        , prelude.package /\ version 0
        ]

    Spec.it "Tight bound on 'prelude@1'" do
      shouldSucceed
        [ simple.package /\ range 0 2 -- loose
        , prelude.package /\ range 1 2 -- tight
        ]
        [ simple.package /\ version 1
        , prelude.package /\ version 1
        ]

    Spec.it "Tight bound on 'simple@0'" do
      shouldSucceed
        [ prelude.package /\ range 0 2 -- loose
        , simple.package /\ range 0 1 -- tight
        ]
        [ simple.package /\ version 0
        , prelude.package /\ version 0
        ]

    Spec.it "Tight bound on 'simple@1'" do
      shouldSucceed
        [ prelude.package /\ range 0 2 -- loose
        , simple.package /\ range 1 2 -- tight
        ]
        [ simple.package /\ version 1
        , prelude.package /\ version 1
        ]

  Spec.describe "Valid dependency ranges containing some invalid versions solve" do
    Spec.it "Proceeds past broken ranges to find a later valid range" do
      -- 'broken-fixed' cannot be solved at the broken version 0, but it can be
      -- solved at the fixed version 1.
      shouldSucceed
        [ brokenFixed.package /\ range 0 2 ]
        [ brokenFixed.package /\ version 1, prelude.package /\ version 0 ]

    Spec.it "Backtracks from broken ranges to find the highest valid range." do
      -- 'fixed-broken' works at version 0 and 1, but is broken at version 2.
      shouldSucceed
        [ fixedBroken.package /\ range 0 2 ]
        [ fixedBroken.package /\ version 1, prelude.package /\ version 1 ]

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
        [ prelude.package /\ range 20 50 ]
        [ { error: NoVersionsInRange prelude.package (Set.fromFoldable [ version 0, version 1 ]) (range 20 50) SolveRoot
          , message: "Package index contained no versions for prelude in the range >=20.0.0 <50.0.0 (existing versions: 0.0.0, 1.0.0)"
          }
        ]

    Spec.it "Direct dependency of target package has no versions in range." do
      shouldFail
        [ brokenFixed.package /\ range 0 1 ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) (Solving brokenFixed.package (pure (version 0)) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0"
          }
        ]

    Spec.it "Nested dependency of target package has no versions in range." do
      shouldFail
        [ transitiveBroken.package /\ range 0 1 ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving fixedBroken.package (pure (version 2)) (Solving transitiveBroken.package (pure (version 0)) SolveRoot))
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@2.0.0 while solving transitive-broken@0.0.0"
          }
        ]

  Spec.describe "Does not solve when ranges do not intersect" do
    Spec.it "Simple disjoint ranges" do
      shouldFail
        [ simple.package /\ range 0 1, prelude.package /\ range 1 2 ]
        [ { error: VersionNotInRange prelude.package (NES.singleton (version 1)) (range 0 1) (Solving simple.package (pure (version 0)) SolveRoot)
          , message: "Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0"
          }
        ]

    -- only-simple depends on simple@0, which is incompatible with the prelude
    -- range provided
    Spec.it "Transitive disjoint ranges" do
      shouldFail
        [ onlySimple.package /\ range 0 4
        , prelude.package /\ range 1 2
        ]
        [ { error: VersionNotInRange prelude.package (NES.singleton (version 1)) (range 0 1) (Solving simple.package (pure (version 0)) (Solving onlySimple.package (pure (version 0)) SolveRoot))
          , message: "Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0 while solving only-simple@0.0.0"
          }
        ]

  Spec.describe "Reports multiple errors" do
    Spec.it "Fails when target package cannot be satisfied" do
      shouldFail
        [ brokenBroken.package /\ range 0 2 ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving brokenBroken.package (pure (version 1) <|> pure (version 0)) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving broken-broken@1.0.0, 0.0.0"
          }
        ]

    Spec.it "Fails on disjoint ranges" do
      shouldFail
        [ brokenFixed.package /\ range 0 1
        , fixedBroken.package /\ range 2 3
        ]
        [ { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) (Solving brokenFixed.package (pure (version 0)) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0"
          }
        , { error: NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving fixedBroken.package (pure (version 2)) SolveRoot)
          , message: "Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@2.0.0"
          }
        ]

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
  { package: package "transitive-broken"
  , versions: Map.fromFoldable
      [ version 0 /\ Just (fixedBroken.package /\ range 2 3)
      , version 1 /\ Just (brokenFixed.package /\ range 0 1)
      ]
  }

-- | Broken at v1 and v2
brokenBroken :: TestPackage
brokenBroken =
  { package: package "broken-broken"
  , versions: Map.fromFoldable
      [ version 0 /\ Just (package "does-not-exist" /\ range 0 5)
      , version 1 /\ Just (package "does-not-exist" /\ range 0 5)
      ]
  }

-- | Fixed at v0, v1, broken at v2
fixedBroken :: TestPackage
fixedBroken =
  { package: package "fixed-broken"
  , versions: Map.fromFoldable
      [ version 0 /\ Just (prelude.package /\ range 0 1)
      , version 1 /\ Just (prelude.package /\ range 1 2)
      , version 2 /\ Just (package "does-not-exist" /\ range 0 5)
      ]
  }

-- | Broken at v0, fixed at v1
brokenFixed :: TestPackage
brokenFixed =
  { package: package "broken-fixed"
  , versions: Map.fromFoldable
      [ version 0 /\ Just (package "does-not-exist" /\ range 0 4)
      , version 1 /\ Just (prelude.package /\ range 0 1)
      ]
  }

onlySimple :: TestPackage
onlySimple =
  { package: package "only-simple"
  , versions: Map.singleton (version 0) (Just (simple.package /\ range 0 1))
  }

simple :: TestPackage
simple =
  { package: package "simple"
  , versions: Map.fromFoldable
      [ version 0 /\ Just (prelude.package /\ range 0 1)
      , version 1 /\ Just (prelude.package /\ range 1 2)
      ]
  }

prelude :: TestPackage
prelude =
  { package: package "prelude"
  , versions: Map.fromFoldable
      [ version 0 /\ Nothing
      , version 1 /\ Nothing
      ]
  }

package :: String -> PackageName
package = unsafeFromRight <<< PackageName.parse

version :: Int -> Version
version = unsafeFromRight <<< Version.parse <<< (_ <> ".0.0") <<< show

-- For all these tests, we work with major versions only because we do not
-- need to exercise the intricacies of the range relations, just the solver,
-- which does not care about what versions are, just how they relate
range :: Int -> Int -> Range
range lower upper = unsafeFromRight $ Range.parse $ Array.fold
  [ ">="
  , show lower
  , ".0.0 <"
  , show upper
  , ".0.0"
  ]
