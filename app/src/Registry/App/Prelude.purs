module Registry.App.Prelude
  ( fromJust'
  , guardA
  , mapKeys
  , module Either
  , module Extra
  , module Maybe
  , module Prelude
  , module Registry.Types
  , module Registry.App.Types
  , newlines
  , partitionEithers
  , stripPureScriptPrefix
  , traverseKeys
  , unsafeFromJust
  , unsafeFromRight
  , withBackoff
  , withBackoff'
  , nowUTC
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Control.Alternative (class Alternative, empty)
import Control.Monad.Error.Class (throwError) as Extra
import Control.Monad.Except (ExceptT(..)) as Extra
import Control.Monad.Trans.Class (lift) as Extra
import Control.Parallel.Class as Parallel
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray) as Extra
import Data.Bifunctor (bimap, lmap, rmap) as Extra
import Data.Bitraversable (ltraverse)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..), either, fromLeft, fromRight', hush, isRight, note) as Either
import Data.Foldable (all, and, any, fold) as Extra
import Data.Foldable as Foldable
import Data.FoldableWithIndex (foldlWithIndex, forWithIndex_) as Extra
import Data.Identity (Identity) as Extra
import Data.Int as Int
import Data.List (List) as Extra
import Data.Map (Map) as Extra
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe) as Maybe
import Data.Newtype (class Newtype, un) as Extra
import Data.Newtype as Newtype
import Data.Nullable (Nullable, toMaybe, toNullable) as Extra
import Data.Set (Set) as Extra
import Data.String as String
import Data.String.NonEmpty (NonEmptyString) as Extra
import Data.Time.Duration as Duration
import Data.Traversable (for, for_, sequence, traverse) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..), fst, snd) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect)
import Effect (Effect) as Extra
import Effect.Aff (Aff, launchAff_, try) as Extra
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff) as Extra
import Effect.Class (class MonadEffect, liftEffect) as Extra
import Effect.Class.Console (error, info, log, logShow) as Extra
import Effect.Now as Now
import Effect.Ref (Ref) as Extra
import Foreign.Object (Object) as Extra
import Node.Buffer (Buffer) as Extra
import Node.Encoding (Encoding(..)) as Extra
import Node.Path (FilePath) as Extra
import Partial.Unsafe (unsafeCrashWith) as Extra
import Registry.App.Types (RawPackageName(..), RawVersion(..), RawVersionRange(..), rawPackageNameCodec, rawPackageNameMapCodec, rawVersionCodec, rawVersionMapCodec, rawVersionRangeCodec)
import Registry.Types (License, Location(..), Manifest(..), ManifestIndex, Metadata(..), Owner(..), PackageName, PackageSet(..), PublishedMetadata, Range, Sha256, UnpublishedMetadata, Version)

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either.Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Either.Left err -> { fail: [ err ], success: [] }
  Either.Right res -> { fail: [], success: [ res ] }

-- | Strip the "purescript-" prefix from a package name, if present.
-- |
-- | ```purs
-- | > stripPureScriptPrefix "purescript-numbers"
-- | "numbers"
-- |
-- | > stripPureScriptPrefix "numbers"
-- | "numbers"
-- | ```
stripPureScriptPrefix :: String -> String
stripPureScriptPrefix pkg =
  Maybe.fromMaybe pkg $ String.stripPrefix (String.Pattern "purescript-") pkg

-- | Create a string containing `n` newline characters.
-- |
-- | ```purs
-- | > newlines 3
-- | "\n\n\n"
-- | ```
newlines :: Int -> String
newlines n = Array.fold $ Array.replicate n "\n"

fromJust' :: forall a. (Unit -> a) -> Maybe.Maybe a -> a
fromJust' _ (Maybe.Just a) = a
fromJust' failed _ = failed unit

unsafeFromJust :: forall a. Maybe.Maybe a -> a
unsafeFromJust = fromJust' (\_ -> Extra.unsafeCrashWith "Unexpected Nothing")

unsafeFromRight :: forall e a. Either.Either e a -> a
unsafeFromRight = Either.fromRight' (\_ -> Extra.unsafeCrashWith "Unexpected Left")

mapKeys :: forall a b v. Ord a => Ord b => (a -> b) -> Extra.Map a v -> Extra.Map b v
mapKeys k = Map.fromFoldable <<< map (Extra.lmap k) <<< (Map.toUnfoldable :: _ -> Array _)

traverseKeys :: forall a b v. Ord a => Ord b => (a -> Either.Either String b) -> Extra.Map a v -> Either.Either String (Extra.Map b v)
traverseKeys k = map Map.fromFoldable <<< Extra.traverse (ltraverse k) <<< (Map.toUnfoldable :: _ -> Array _)

guardA :: forall f. Alternative f => Boolean -> f Unit
guardA = if _ then pure unit else empty

-- | Attempt an effectful computation with exponential backoff.
withBackoff' :: forall a. Extra.Aff a -> Extra.Aff (Maybe.Maybe a)
withBackoff' action = withBackoff
  { delay: Aff.Milliseconds 5_000.0
  , action
  , shouldCancel: \_ -> pure true
  , shouldRetry: \attempt -> if attempt > 3 then pure Maybe.Nothing else pure (Maybe.Just action)
  }

type Backoff a =
  { delay :: Aff.Milliseconds
  , action :: Extra.Aff a
  , shouldCancel :: Int -> Extra.Aff Boolean
  , shouldRetry :: Int -> Extra.Aff (Maybe.Maybe (Extra.Aff a))
  }

-- | Attempt an effectful computation with exponential backoff, starting with
-- | the provided timeout.
withBackoff :: forall a. Backoff a -> Extra.Aff (Maybe.Maybe a)
withBackoff { delay: Aff.Milliseconds timeout, action, shouldCancel, shouldRetry } = do
  let
    runAction attempt action' ms =
      Parallel.sequential $ Foldable.oneOf
        [ Parallel.parallel (map Maybe.Just action')
        , Parallel.parallel (runTimeout attempt ms)
        ]

    runTimeout attempt ms = do
      _ <- Aff.delay (Aff.Milliseconds (Int.toNumber ms))
      shouldCancel attempt >>=
        if _ then do
          Extra.log $ "Cancelled after " <> show ms <> " milliseconds, retrying (attempt " <> show attempt <> ")..."
          pure Maybe.Nothing
        else runTimeout attempt (ms * 2)

    loop :: Int -> Maybe.Maybe a -> Extra.Aff (Maybe.Maybe a)
    loop attempt = case _ of
      Maybe.Nothing -> do
        maybeRetry <- shouldRetry attempt
        case maybeRetry of
          Maybe.Nothing -> pure Maybe.Nothing
          Maybe.Just newAction -> do
            let newTimeout = Int.floor timeout `Int.pow` (attempt + 1)
            maybeResult <- runAction attempt newAction newTimeout
            loop (attempt + 1) maybeResult
      Maybe.Just result ->
        pure (Maybe.Just result)

  maybeResult <- runAction 0 action (Int.floor timeout)
  loop 1 maybeResult

-- | Get the current time, standardizing on the UTC timezone to avoid ambiguity
-- | when running on different machines.
nowUTC :: Effect DateTime
nowUTC = do
  offset <- Newtype.over Duration.Minutes negate <$> Now.getTimezoneOffset
  now <- Now.nowDateTime
  pure $ Maybe.fromMaybe now $ DateTime.adjust (offset :: Duration.Minutes) now
