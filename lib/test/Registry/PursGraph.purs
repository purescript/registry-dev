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
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
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

  Spec.it "type-equality (no deps)" do
    json <- FS.Aff.readTextFile UTF8 $ Path.concat [ "lib", "fixtures", "purs-graph", "type-equality.json" ]
    case parse json of
      Left err -> Assert.fail err
      Right graph -> do
        Map.size graph `Assert.shouldEqual` 1

        case PursGraph.associateModules (PackageName.parse <<< Maybe.fromMaybe "" <<< Array.head <<< String.split (String.Pattern Path.sep)) graph of
          Left errs -> Assert.fail $ "Failed to parse package names:\n\n" <> NonEmptyArray.foldMap1 (\{ path, error } -> path <> ": " <> error) errs
          Right names | Map.size names > 1 -> Assert.fail "Expected only one package name"
          Right names ->
            unless (Foldable.elem (unsafePackageName "type-equality") (Map.values names)) do
              Assert.fail $ "Expected to find package name 'type-equality': " <> unsafeStringify names

        PursGraph.directDependencies (ModuleName "Type.Equality") graph `Assert.shouldEqual` (Just Set.empty)
        PursGraph.allDependencies (ModuleName "Type.Equality") graph `Assert.shouldEqual` (Just Set.empty)

  Spec.it "prelude (small graph)" do
    json <- FS.Aff.readTextFile UTF8 $ Path.concat [ "lib", "fixtures", "purs-graph", "prelude.json" ]
    case parse json of
      Left err -> Assert.fail err
      Right graph -> do
        -- purs graph ... | jq length
        let size = 50
        Map.size graph `Assert.shouldEqual` size

        let
          -- purs graph ... | jq keys
          expectedKeys =
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

        if Array.length expectedKeys == size then
          for_ expectedKeys \name -> case Map.lookup (coerce name) graph of
            Nothing -> Assert.fail $ "Missing expected module name " <> name
            Just _ -> pure unit
        else
          Assert.fail "Expected and actual module counts do not match; please update the test."

        -- This one was worked by hand.
        let
          workedModule :: String
          workedModule = "Data.Field"

          expectedDirects :: Array ModuleName
          expectedDirects = coerce
            [ "Data.CommutativeRing"
            , "Data.DivisionRing"
            , "Data.EuclideanRing"
            , "Data.Ring"
            , "Data.Semiring"
            ]

        case PursGraph.directDependencies (ModuleName workedModule) graph of
          Nothing -> Assert.fail "Expected directDependencies to return a result"
          Just deps -> Assert.shouldEqual (Set.toUnfoldable deps) expectedDirects

        let
          expectedAll :: Array ModuleName
          expectedAll = Array.sort $ coerce
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
          Just deps -> Assert.shouldEqual (Set.toUnfoldable deps) expectedAll

  -- For this test we're simply ensuring that performance is acceptable: we don't
  -- run out of memory getting all dependencies.
  Spec.it "bookhound (medium graph)" do
    json <- FS.Aff.readTextFile UTF8 $ Path.concat [ "lib", "fixtures", "purs-graph", "bookhound.json" ]
    case parse json of
      Left err -> Assert.fail err
      Right graph -> do
        let
          sourceModules :: Set ModuleName
          sourceModules = Map.keys $ Map.filter (Maybe.isJust <<< String.stripPrefix (String.Pattern "src") <<< _.path) graph

          reachableModules :: Set ModuleName
          reachableModules = PursGraph.allDependenciesOf sourceModules graph

        Assert.shouldSatisfy reachableModules (not <<< Set.isEmpty)
        Set.intersection sourceModules reachableModules `Assert.shouldEqual` Set.empty
