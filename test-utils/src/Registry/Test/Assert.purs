module Registry.Test.Assert where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.String as String
import Effect.Exception (Error)
import JSON as JSON
import Registry.Test.Utils as Utils
import Test.Spec.Assertions (AnyShow(..))
import Test.Spec.Assertions as Assertions

fail :: forall m. MonadThrow Error m => String -> m Unit
fail = Assertions.fail

shouldEqual :: forall m a. MonadThrow Error m => Eq a => a -> a -> m Unit
shouldEqual a b = Assertions.shouldEqual (AnyShow a) (AnyShow b)

shouldEqualRight :: forall m e a. MonadThrow Error m => Eq a => a -> Either e a -> m Unit
shouldEqualRight a = case _ of
  Left e -> fail ("Expected Right, but received Left with value: " <> Utils.unsafeStringify e)
  Right b -> shouldEqual a b

shouldContain :: forall m f a. MonadThrow Error m => Eq a => Foldable f => f a -> a -> m Unit
shouldContain container elem =
  when (elem `Foldable.notElem` container) do
    fail (Utils.unsafeStringify elem <> "\n\nshould be a member of\n\n" <> Utils.unsafeStringify container)

shouldNotContain :: forall m f a. MonadThrow Error m => Eq a => Foldable f => f a -> a -> m Unit
shouldNotContain container elem =
  when (elem `Foldable.elem` container) do
    fail (Utils.unsafeStringify elem <> "\n\nshould not be a member of\n\n" <> Utils.unsafeStringify container)

-- | Assert that all elements in `expected` are present in `actual`.
-- | This is a subset check, not an equality check - `actual` may contain
-- | additional elements.
-- |
-- | Useful for E2E tests where a shared database means we can't predict
-- | exact contents, only that certain expected items are present.
shouldContainAll :: forall m a. MonadThrow Error m => Eq a => Array a -> Array a -> m Unit
shouldContainAll actual expected =
  Foldable.for_ expected \elem ->
    when (elem `Foldable.notElem` actual) do
      fail ("Expected element not found:\n" <> Utils.unsafeStringify elem <> "\n\nin array:\n" <> Utils.unsafeStringify actual)

shouldSatisfy :: forall m a. MonadThrow Error m => a -> (a -> Boolean) -> m Unit
shouldSatisfy a predicate =
  unless (predicate a) do
    fail (Utils.unsafeStringify a <> " doesn't satisfy predicate.")

shouldNotSatisfy :: forall m a. MonadThrow Error m => a -> (a -> Boolean) -> m Unit
shouldNotSatisfy a predicate =
  when (predicate a) do
    fail (Utils.unsafeStringify a <> " satisfies predicate, but should not.")

type Fixture = { label :: String, value :: String }

shouldRoundTrip :: forall m a. MonadThrow Error m => String -> CJ.Codec a -> Array Fixture -> m Unit
shouldRoundTrip type_ codec fixtures = do
  let
    parseFixture { label, value } =
      case lmap CJ.DecodeError.print <<< CJ.decode codec =<< JSON.parse value of
        Left error -> Left { label, input: value, error }
        Right result -> Right { label, input: value, result }

    fixtureParseResult = Utils.partitionEithers $ map parseFixture fixtures

    formatFixtureError { label, input, error } = label <> " failed with " <> error <> " for input:\n" <> input

  unless (Array.null fixtureParseResult.fail) do
    fail $ String.joinWith "\n"
      [ "Some well-formed " <> type_ <> " strings were not parsed correctly:"
      , Array.foldMap (append "\n  - " <<< formatFixtureError) fixtureParseResult.fail
      ]

  let
    roundtrip = fixtureParseResult.success <#> \fields -> do
      let printed = JSON.printIndented $ CJ.encode codec fields.result
      let input = String.trim fields.input
      if input == printed then Right unit else Left { label: fields.label, input, printed }

    roundtripResult = Utils.partitionEithers roundtrip

    formatRoundtripError { label, input, printed } =
      String.joinWith "\n"
        [ label <> " input does not match output."
        , String.joinWith "\n" [ input, "/=", printed ]
        ]

  unless (Array.null roundtripResult.fail) do
    fail $ String.joinWith "\n"
      [ "Some well-formed " <> type_ <> " did not round-trip:"
      , Array.foldMap (append "\n  - " <<< formatRoundtripError) roundtripResult.fail
      ]
