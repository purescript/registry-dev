module Test.Registry.Solver (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Data.Set.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Solver (Intersection(..), LocalSolverPosition(..), SolverError(..), SolverPosition(..), Sourced(..), printSolverError, solve)
import Registry.Types (PackageName, Range, Version)
import Registry.Version as Version
import Test.Assert as Assert
import Test.Spec as Spec
import Test.Utils (fromRight)

spec :: Spec.Spec Unit
spec = do
  let
    shouldSucceed goals result = pure unit >>= \_ ->
      solve solverIndex (Map.fromFoldable goals) `Assert.shouldContain` (Map.fromFoldable result)

    shouldFail goals errors = pure unit >>= \_ -> case solve solverIndex (Map.fromFoldable goals) of
      Left solverErrors -> do
        let expectedErrorCount = Array.length errors
        let receivedErrorCount = NonEmptyList.length solverErrors

        when (expectedErrorCount /= receivedErrorCount) do
          if expectedErrorCount == 0
            then Assert.fail $ "Error(s): " <> intercalateMap "\n" printSolverError solverErrors
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
        [ { error: Conflicts $ Map.fromFoldable
            [ package "does-not-exist" `Tuple` intersection 0 (solveRoot "does-not-exist") 4 (solveRoot "does-not-exist") ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 (declared dependency)\n  <4.0.0 (declared dependency)"
          }
        ]

    Spec.it "Target package has versions, but none in range" do
      shouldFail
        [ prelude.package /\ range 20 50 ]
        [ { error: Conflicts $ Map.fromFoldable
            [ package "prelude" `Tuple` intersection 20 (solveRoot "prelude") 50 (solveRoot "prelude") ]
          , message: "No versions found in the registry for prelude in range\n  >=20.0.0 (declared dependency)\n  <50.0.0 (declared dependency)"
          }
        ]

    Spec.it "Direct dependency of target package has no versions in range." do
      shouldFail
        [ brokenFixed.package /\ range 0 1 ]
        [ { error: Conflicts $ Map.fromFoldable
            [ package "does-not-exist" `Tuple` intersection 0 (via' "broken-fixed" 0) 4 (via' "broken-fixed" 0)
            ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in broken-fixed@0.0.0\n  <4.0.0 seen in broken-fixed@0.0.0"
          }
        ]

    Spec.it "Nested dependency of target package has no versions in range." do
      shouldFail
        [ transitiveBroken.package /\ range 0 1 ]
        [ { error: Conflicts $ Map.fromFoldable
            [ package "does-not-exist" `Tuple` intersection 0 (via "fixed-broken" 2 ["transitive-broken"]) 5 (via "fixed-broken" 2 ["transitive-broken"])
            ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in fixed-broken@2.0.0 from declared dependencies transitive-broken\n  <5.0.0 seen in fixed-broken@2.0.0 from declared dependencies transitive-broken"
          }
        ]

    Spec.it "Fails when target package cannot be satisfied" do
      shouldFail
        [ brokenBroken.package /\ range 0 2 ]
        [ { error: Conflicts $ Map.fromFoldable
            [ package "does-not-exist" `Tuple` intersection 0 (via' "broken-broken" 0 <> via' "broken-broken" 1) 5 (via' "broken-broken" 0 <> via' "broken-broken" 1)
            ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in broken-broken@0.0.0, broken-broken@1.0.0\n  <5.0.0 seen in broken-broken@0.0.0, broken-broken@1.0.0"
          }
        ]

  Spec.describe "Does not solve when ranges do not intersect" do
    Spec.it "Simple disjoint ranges" do
      shouldFail
        [ simple.package /\ range 0 1, prelude.package /\ range 1 2 ]
        [ { error: WhileSolving (package "simple") $ Map.singleton (version 0) $ Conflicts $ Map.fromFoldable
            [ package "prelude" `Tuple` intersection 1 (solveRoot "prelude") 1 (via "simple" 0 [])
            ]
          , message: "While solving simple each version could not be solved:\n- 0.0.0: \n  Conflict in version ranges for prelude:\n    >=1.0.0 (declared dependency)\n    <1.0.0 seen in simple@0.0.0"
          }
        ]

    -- only-simple depends on simple@0, which is incompatible with the prelude
    -- range provided
    Spec.it "Transitive disjoint ranges" do
      shouldFail
        [ onlySimple.package /\ range 0 4
        , prelude.package /\ range 1 2
        ]
        [ { error: WhileSolving (package "simple") $ Map.singleton (version 0) $ Conflicts $ Map.fromFoldable
            [ package "prelude" `Tuple` intersection 1 (solveRoot "prelude") 1 (via "simple" 0 [])
            ]
          , message: "While solving simple each version could not be solved:\n- 0.0.0: \n  Conflict in version ranges for prelude:\n    >=1.0.0 (declared dependency)\n    <1.0.0 seen in simple@0.0.0"
          }
        ]

    Spec.it "Fails on disjoint ranges" do
      shouldFail
        [ brokenFixed.package /\ range 0 1
        , fixedBroken.package /\ range 2 3
        ]
        [ { error: Conflicts $ Map.fromFoldable
            [ package "does-not-exist" `Tuple` intersection 0 (via' "broken-fixed" 0 <> via' "fixed-broken" 2) 4 (via' "broken-fixed" 0)
            ]
          , message: "No versions found in the registry for does-not-exist in range\n  >=0.0.0 seen in broken-fixed@0.0.0, fixed-broken@2.0.0\n  <4.0.0 seen in broken-fixed@0.0.0" }
        ]

  Spec.describe "Reports multiple errors" do
    Spec.it "Reports different errors for different versions" do
      shouldFail
        [ chaotic.package /\ range 1 3
        , prelude.package /\ range 2 4
        ]
        [ { error: WhileSolving (package "chaotic") $ Map.fromFoldable
            [ Tuple (version 1) $ Conflicts $ Map.fromFoldable
              [ package "prelude" `Tuple` intersection 2 (solveRoot "prelude") 1 (via "chaotic" 1 [])
              ]
            , Tuple (version 2) $ Conflicts $ Map.fromFoldable
              -- TODO: why no global "chaotic" here?
              [ package "prelude" `Tuple` intersection 5 (via "chaotic" 2 []) 4 (solveRoot "prelude")
              ]
            ]
          , message: "While solving chaotic each version could not be solved:\n- 1.0.0: \n  Conflict in version ranges for prelude:\n    >=2.0.0 (declared dependency)\n    <1.0.0 seen in chaotic@1.0.0\n- 2.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in chaotic@2.0.0\n    <4.0.0 (declared dependency)"
          }
        ]

    Spec.it "Groups same errors" do
      shouldFail
        [ chaotic.package /\ range 1 4
        , prelude.package /\ range 2 4
        ]
        [ { error: WhileSolving (package "chaotic") $ Map.fromFoldable
            [ Tuple (version 1) $ Conflicts $ Map.fromFoldable
              [ package "prelude" `Tuple` intersection 2 (solveRoot "prelude") 1 (via "chaotic" 1 [])
              ]
            , Tuple (version 2) $ Conflicts $ Map.fromFoldable
              -- TODO: why no global "chaotic" here?
              [ package "prelude" `Tuple` intersection 5 (via "chaotic" 2 []) 4 (solveRoot "prelude")
              ]
            , Tuple (version 3) $ Conflicts $ Map.fromFoldable
              -- TODO: why no global "chaotic" here?
              [ package "prelude" `Tuple` intersection 5 (via "chaotic" 3 []) 4 (solveRoot "prelude")
              ]
            ]
          , message: "While solving chaotic each version could not be solved:\n- 1.0.0: \n  Conflict in version ranges for prelude:\n    >=2.0.0 (declared dependency)\n    <1.0.0 seen in chaotic@1.0.0\n- 2.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in chaotic@2.0.0\n    <4.0.0 (declared dependency)\n- 3.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in chaotic@3.0.0\n    <4.0.0 (declared dependency)"
          }
        ]

    -- since we try packages in alphabetical order
    Spec.it "Reports different errors for different versions (converse)" do
      shouldFail
        [ qaotic.package /\ range 1 3
        , prelude.package /\ range 2 4
        ]
        [ { error: WhileSolving (package "qaotic") $ Map.fromFoldable
            [ Tuple (version 1) $ Conflicts $ Map.fromFoldable
              [ package "prelude" `Tuple` intersection 2 (solveRoot "prelude") 1 (via "qaotic" 1 [])
              ]
            , Tuple (version 2) $ Conflicts $ Map.fromFoldable
              -- TODO: why no global "prelude" here?
              [ package "prelude" `Tuple` intersection 5 (via "qaotic" 2 []) 4 (solveRoot "prelude")
              ]
            ]
          , message: "While solving qaotic each version could not be solved:\n- 1.0.0: \n  Conflict in version ranges for prelude:\n    >=2.0.0 (declared dependency)\n    <1.0.0 seen in qaotic@1.0.0\n- 2.0.0: \n  Conflict in version ranges for prelude:\n    >=5.0.0 seen in qaotic@2.0.0\n    <4.0.0 (declared dependency)"
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
      , version 2 /\ Nothing
      , version 3 /\ Nothing
      , version 4 /\ Nothing
      , version 5 /\ Nothing
      ]
  }

chaotic :: TestPackage
chaotic =
  { package: package "chaotic"
  , versions: Map.fromFoldable
      [ version 1 /\ Just (prelude.package /\ range 0 1)
      , version 2 /\ Just (prelude.package /\ range 5 6)
      , version 3 /\ Just (prelude.package /\ range 5 6)
      ]
  }

qaotic :: TestPackage
qaotic =
  { package: package "qaotic"
  , versions: Map.fromFoldable
      [ version 1 /\ Just (prelude.package /\ range 0 1)
      , version 2 /\ Just (prelude.package /\ range 5 6)
      ]
  }

package :: String -> PackageName
package = fromRight "bad package name" <<< PackageName.parse

version :: Int -> Version
version = fromRight "bad version" <<< Version.parse <<< (_ <> ".0.0") <<< show

-- For all these tests, we work with major versions only because we do not
-- need to exercise the intricacies of the range relations, just the solver,
-- which does not care about what versions are, just how they relate
range :: Int -> Int -> Range
range lower upper = fromRight "bad range" $ Range.parse $ Array.fold
  [ ">="
  , show lower
  , ".0.0 <"
  , show upper
  , ".0.0"
  ]

intersection :: Int -> SolverPosition -> Int -> SolverPosition -> Intersection
intersection lower lowerPos upper upperPos = Intersection
  { lower: wrap $ Sourced (version lower) lowerPos
  , upper: wrap $ Sourced (version upper) upperPos
  }

solveRoot :: String -> SolverPosition
solveRoot = Pos Root <<< Set.singleton <<< package

via :: String -> Int -> Array String -> SolverPosition
via p v = Pos (Solving (NES.singleton { package: package p, version: version v })) <<< Set.fromFoldable <<< map package

via' :: String -> Int -> SolverPosition
via' p v = Pos (Solving (NES.singleton { package: package p, version: version v })) (Set.singleton (package p))
