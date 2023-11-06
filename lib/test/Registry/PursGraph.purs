module Test.Registry.PursGraph (spec) where

import Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set as Set
import Data.String as String
import Node.Path as Path
import Registry.PackageName as PackageName
import Registry.PursGraph (ModuleName(..))
import Registry.PursGraph as PursGraph
import Registry.Test.Assert as Assert
import Registry.Test.Utils (unsafePackageName, unsafeStringify)
import Safe.Coerce (coerce)
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  let
    parse raw = case Argonaut.Parser.jsonParser raw of
      Left err -> Left $ "Failed to parse graph as JSON:\n\n" <> raw <> "\n\n  due to an error:\n\n" <> err
      Right json -> case CA.decode PursGraph.pursGraphCodec json of
        Left err -> Left $ "Failed to decode graph JSON:\n\n" <> CA.printJsonDecodeError err
        Right result -> pure result

  Spec.describe "type-equality" do
    case parse typeEquality of
      Left err ->
        Spec.it "Parses the graph" do
          Assert.fail err

      Right graph -> do
        Spec.it "Has the correct number of modules" do
          Map.size graph `Assert.shouldEqual` 1

        Spec.it "Parses packages from paths" do
          case PursGraph.associateModules (PackageName.parse <<< Maybe.fromMaybe "" <<< Array.head <<< String.split (String.Pattern Path.sep)) graph of
            Left errs -> Assert.fail $ "Failed to parse package names:\n\n" <> NonEmptyArray.foldMap1 (\{ path, error } -> path <> ": " <> error) errs
            Right names | Map.size names > 1 -> Assert.fail "Expected only one package name"
            Right names ->
              unless (Foldable.elem (unsafePackageName "type-equality") (Map.values names)) do
                Assert.fail $ "Expected to find package name 'type-equality': " <> unsafeStringify names

        Spec.it "Has correct direct dependencies for Type.Equality" do
          PursGraph.directDependencies (ModuleName "Type.Equality") graph `Assert.shouldEqual` (Just Set.empty)

        Spec.it "Has correct all dependencies for Type.Equality" do
          PursGraph.allDependencies (ModuleName "Type.Equality") graph `Assert.shouldEqual` (Just Set.empty)

  Spec.describe "prelude" do
    case parse prelude of
      Left err ->
        Spec.it "Parses the graph" do
          Assert.fail err

      Right graph -> do
        -- purs graph ... | jq length
        let size = 50

        Spec.it "Has the correct number of modules" do
          Map.size graph `Assert.shouldEqual` size

        Spec.it "Contains all expected modules" do
          let
            -- purs graph ... | jq keys
            expected =
              [ "Control.Applicative"
              , "Control.Apply"
              , "Control.Bind"
              , "Control.Category"
              , "Control.Monad"
              , "Control.Semigroupoid"
              , "Data.Boolean"
              , "Data.BooleanAlgebra"
              , "Data.Bounded"
              , "Data.Bounded.Generic"
              , "Data.CommutativeRing"
              , "Data.DivisionRing"
              , "Data.Eq"
              , "Data.Eq.Generic"
              , "Data.EuclideanRing"
              , "Data.Field"
              , "Data.Function"
              , "Data.Functor"
              , "Data.Generic.Rep"
              , "Data.HeytingAlgebra"
              , "Data.HeytingAlgebra.Generic"
              , "Data.Monoid"
              , "Data.Monoid.Additive"
              , "Data.Monoid.Conj"
              , "Data.Monoid.Disj"
              , "Data.Monoid.Dual"
              , "Data.Monoid.Endo"
              , "Data.Monoid.Generic"
              , "Data.Monoid.Multiplicative"
              , "Data.NaturalTransformation"
              , "Data.Ord"
              , "Data.Ord.Generic"
              , "Data.Ordering"
              , "Data.Reflectable"
              , "Data.Ring"
              , "Data.Ring.Generic"
              , "Data.Semigroup"
              , "Data.Semigroup.First"
              , "Data.Semigroup.Generic"
              , "Data.Semigroup.Last"
              , "Data.Semiring"
              , "Data.Semiring.Generic"
              , "Data.Show"
              , "Data.Show.Generic"
              , "Data.Symbol"
              , "Data.Unit"
              , "Data.Void"
              , "Prelude"
              , "Record.Unsafe"
              , "Type.Proxy"
              ]

          if Array.length expected == size then
            for_ expected \name -> case Map.lookup (coerce name) graph of
              Nothing -> Assert.fail $ "Missing expected module name " <> name
              Just _ -> pure unit
          else
            Assert.fail "Expected and actual module counts do not match; please update the test."

        -- This one was worked by hand.
        let workedModule = "Data.Field"
        Spec.it ("Has correct direct dependencies for " <> workedModule) do
          let
            expected :: Array ModuleName
            expected = coerce
              [ "Data.CommutativeRing"
              , "Data.DivisionRing"
              , "Data.EuclideanRing"
              , "Data.Ring"
              , "Data.Semiring"
              ]

          case PursGraph.directDependencies (ModuleName workedModule) graph of
            Nothing -> Assert.fail "Expected allDependencies to return a result"
            Just deps -> Assert.shouldEqual (Set.toUnfoldable deps) expected

        Spec.it ("Has correct all dependencies for " <> workedModule) do
          let
            expected :: Array ModuleName
            expected = Array.sort $ coerce
              [ -- directs
                "Data.CommutativeRing"
              , "Data.DivisionRing"
              , "Data.EuclideanRing"
              , "Data.Ring"
              , "Data.Semiring"

              -- transitive via Data.CommutativeRing
              , "Data.Symbol"
              , "Data.Unit"
              , "Type.Proxy"

              -- transitive via Data.EuclideanRing
              , "Data.BooleanAlgebra"
              , "Data.Eq"

              -- transitive via Data.Ring
              , "Record.Unsafe"

              -- transitive via Data.BooleanAlgebra
              , "Data.HeytingAlgebra"

              -- transitive via Data.Eq
              , "Data.Void"
              ]

          case PursGraph.allDependencies (ModuleName workedModule) graph of
            Nothing -> Assert.fail "Expected allDependencies to return a result"
            Just deps -> Assert.shouldEqual (Set.toUnfoldable deps) expected

typeEquality :: String
typeEquality =
  """
  {
    "Type.Equality": {
      "depends": [],
      "path": "type-equality/src/Type/Equality.purs"
    }
  }
  """

prelude :: String
prelude =
  """
  {
    "Control.Applicative": {
      "depends": [
        "Control.Apply",
        "Data.Functor",
        "Data.Unit",
        "Type.Proxy"
      ],
      "path": "prelude/src/Control/Applicative.purs"
    },
    "Control.Apply": {
      "depends": [
        "Data.Functor",
        "Data.Function",
        "Control.Category",
        "Type.Proxy"
      ],
      "path": "prelude/src/Control/Apply.purs"
    },
    "Control.Bind": {
      "depends": [
        "Control.Applicative",
        "Control.Apply",
        "Control.Category",
        "Data.Function",
        "Data.Functor",
        "Data.Unit",
        "Type.Proxy"
      ],
      "path": "prelude/src/Control/Bind.purs"
    },
    "Control.Category": {
      "depends": [
        "Control.Semigroupoid"
      ],
      "path": "prelude/src/Control/Category.purs"
    },
    "Control.Monad": {
      "depends": [
        "Control.Applicative",
        "Control.Apply",
        "Control.Bind",
        "Data.Functor",
        "Data.Unit",
        "Type.Proxy"
      ],
      "path": "prelude/src/Control/Monad.purs"
    },
    "Control.Semigroupoid": {
      "depends": [],
      "path": "prelude/src/Control/Semigroupoid.purs"
    },
    "Data.Boolean": {
      "depends": [],
      "path": "prelude/src/Data/Boolean.purs"
    },
    "Data.BooleanAlgebra": {
      "depends": [
        "Data.HeytingAlgebra",
        "Data.Symbol",
        "Data.Unit",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/BooleanAlgebra.purs"
    },
    "Data.Bounded": {
      "depends": [
        "Data.Ord",
        "Data.Symbol",
        "Data.Unit",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Bounded.purs"
    },
    "Data.Bounded.Generic": {
      "depends": [
        "Data.Generic.Rep",
        "Data.Bounded"
      ],
      "path": "prelude/src/Data/Bounded/Generic.purs"
    },
    "Data.CommutativeRing": {
      "depends": [
        "Data.Ring",
        "Data.Semiring",
        "Data.Symbol",
        "Data.Unit",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/CommutativeRing.purs"
    },
    "Data.DivisionRing": {
      "depends": [
        "Data.EuclideanRing",
        "Data.Ring",
        "Data.Semiring"
      ],
      "path": "prelude/src/Data/DivisionRing.purs"
    },
    "Data.Eq": {
      "depends": [
        "Data.HeytingAlgebra",
        "Data.Symbol",
        "Data.Unit",
        "Data.Void",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Eq.purs"
    },
    "Data.Eq.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep"
      ],
      "path": "prelude/src/Data/Eq/Generic.purs"
    },
    "Data.EuclideanRing": {
      "depends": [
        "Data.BooleanAlgebra",
        "Data.CommutativeRing",
        "Data.Eq",
        "Data.Ring",
        "Data.Semiring"
      ],
      "path": "prelude/src/Data/EuclideanRing.purs"
    },
    "Data.Field": {
      "depends": [
        "Data.DivisionRing",
        "Data.CommutativeRing",
        "Data.EuclideanRing",
        "Data.Ring",
        "Data.Semiring"
      ],
      "path": "prelude/src/Data/Field.purs"
    },
    "Data.Function": {
      "depends": [
        "Control.Category",
        "Data.Boolean",
        "Data.Ord",
        "Data.Ring"
      ],
      "path": "prelude/src/Data/Function.purs"
    },
    "Data.Functor": {
      "depends": [
        "Data.Function",
        "Data.Unit",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Functor.purs"
    },
    "Data.Generic.Rep": {
      "depends": [
        "Data.Semigroup",
        "Data.Show",
        "Data.Symbol",
        "Data.Void",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Generic/Rep.purs"
    },
    "Data.HeytingAlgebra": {
      "depends": [
        "Data.Symbol",
        "Data.Unit",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/HeytingAlgebra.purs"
    },
    "Data.HeytingAlgebra.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep",
        "Data.HeytingAlgebra"
      ],
      "path": "prelude/src/Data/HeytingAlgebra/Generic.purs"
    },
    "Data.Monoid": {
      "depends": [
        "Data.Boolean",
        "Data.Eq",
        "Data.EuclideanRing",
        "Data.Ord",
        "Data.Ordering",
        "Data.Semigroup",
        "Data.Symbol",
        "Data.Unit",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Monoid.purs"
    },
    "Data.Monoid.Additive": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Monoid/Additive.purs"
    },
    "Data.Monoid.Conj": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.HeytingAlgebra",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Monoid/Conj.purs"
    },
    "Data.Monoid.Disj": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.HeytingAlgebra",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Monoid/Disj.purs"
    },
    "Data.Monoid.Dual": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Monoid/Dual.purs"
    },
    "Data.Monoid.Endo": {
      "depends": [
        "Prelude"
      ],
      "path": "prelude/src/Data/Monoid/Endo.purs"
    },
    "Data.Monoid.Generic": {
      "depends": [
        "Data.Monoid",
        "Data.Generic.Rep"
      ],
      "path": "prelude/src/Data/Monoid/Generic.purs"
    },
    "Data.Monoid.Multiplicative": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Monoid/Multiplicative.purs"
    },
    "Data.NaturalTransformation": {
      "depends": [],
      "path": "prelude/src/Data/NaturalTransformation.purs"
    },
    "Data.Ord": {
      "depends": [
        "Data.Eq",
        "Data.Symbol",
        "Data.Ordering",
        "Data.Ring",
        "Data.Unit",
        "Data.Void",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Ord.purs"
    },
    "Data.Ord.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep"
      ],
      "path": "prelude/src/Data/Ord/Generic.purs"
    },
    "Data.Ordering": {
      "depends": [
        "Data.Eq",
        "Data.Semigroup",
        "Data.Show"
      ],
      "path": "prelude/src/Data/Ordering.purs"
    },
    "Data.Reflectable": {
      "depends": [
        "Data.Ord",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Reflectable.purs"
    },
    "Data.Ring": {
      "depends": [
        "Data.Semiring",
        "Data.Symbol",
        "Data.Unit",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Ring.purs"
    },
    "Data.Ring.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep"
      ],
      "path": "prelude/src/Data/Ring/Generic.purs"
    },
    "Data.Semigroup": {
      "depends": [
        "Data.Symbol",
        "Data.Unit",
        "Data.Void",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Semigroup.purs"
    },
    "Data.Semigroup.First": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Semigroup/First.purs"
    },
    "Data.Semigroup.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep"
      ],
      "path": "prelude/src/Data/Semigroup/Generic.purs"
    },
    "Data.Semigroup.Last": {
      "depends": [
        "Prelude",
        "Data.Eq",
        "Data.Ord"
      ],
      "path": "prelude/src/Data/Semigroup/Last.purs"
    },
    "Data.Semiring": {
      "depends": [
        "Data.Symbol",
        "Data.Unit",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Semiring.purs"
    },
    "Data.Semiring.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep"
      ],
      "path": "prelude/src/Data/Semiring/Generic.purs"
    },
    "Data.Show": {
      "depends": [
        "Data.Semigroup",
        "Data.Symbol",
        "Data.Unit",
        "Data.Void",
        "Record.Unsafe",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Show.purs"
    },
    "Data.Show.Generic": {
      "depends": [
        "Prelude",
        "Data.Generic.Rep",
        "Data.Symbol",
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Show/Generic.purs"
    },
    "Data.Symbol": {
      "depends": [
        "Type.Proxy"
      ],
      "path": "prelude/src/Data/Symbol.purs"
    },
    "Data.Unit": {
      "depends": [],
      "path": "prelude/src/Data/Unit.purs"
    },
    "Data.Void": {
      "depends": [],
      "path": "prelude/src/Data/Void.purs"
    },
    "Prelude": {
      "depends": [
        "Control.Applicative",
        "Control.Apply",
        "Control.Bind",
        "Control.Category",
        "Control.Monad",
        "Control.Semigroupoid",
        "Data.Boolean",
        "Data.BooleanAlgebra",
        "Data.Bounded",
        "Data.CommutativeRing",
        "Data.DivisionRing",
        "Data.Eq",
        "Data.EuclideanRing",
        "Data.Field",
        "Data.Function",
        "Data.Functor",
        "Data.HeytingAlgebra",
        "Data.Monoid",
        "Data.NaturalTransformation",
        "Data.Ord",
        "Data.Ordering",
        "Data.Ring",
        "Data.Semigroup",
        "Data.Semiring",
        "Data.Show",
        "Data.Unit",
        "Data.Void"
      ],
      "path": "prelude/src/Prelude.purs"
    },
    "Record.Unsafe": {
      "depends": [],
      "path": "prelude/src/Record/Unsafe.purs"
    },
    "Type.Proxy": {
      "depends": [],
      "path": "prelude/src/Type/Proxy.purs"
    }
  }
  """
