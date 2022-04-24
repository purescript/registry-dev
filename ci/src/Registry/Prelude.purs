module Registry.Prelude
  ( module Prelude
  , module Extra
  , module Either
  , module Maybe
  , module Registry.Json
  , module Registry.Types
  , partitionEithers
  , stripPureScriptPrefix
  , newlines
  , fromJust'
  , unsafeFromJust
  , unsafeFromRight
  , mapKeys
  , traverseKeys
  , guardA
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Control.Alternative (class Alternative, empty)
import Control.Monad.Error.Class (throwError) as Extra
import Control.Monad.Except (ExceptT(..)) as Extra
import Control.Monad.Trans.Class (lift) as Extra
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray) as Extra
import Data.Bifunctor (bimap, lmap, rmap) as Extra
import Data.Bitraversable (ltraverse)
import Data.Either (Either(..), either, fromLeft, fromRight', isRight, hush, note) as Either
import Data.Foldable (and, any, all, fold) as Extra
import Data.FoldableWithIndex (forWithIndex_, foldlWithIndex) as Extra
import Data.Identity (Identity) as Extra
import Data.List (List) as Extra
import Data.Map (Map) as Extra
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing, isJust, maybe) as Maybe
import Data.Newtype (un, class Newtype) as Extra
import Data.Nullable (toMaybe, toNullable, Nullable) as Extra
import Data.Set (Set) as Extra
import Data.Show.Generic (genericShow) as Extra
import Data.String as String
import Data.Traversable (for, for_, traverse, sequence) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..), fst, snd) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect) as Extra
import Effect.Aff (Aff, launchAff_, try) as Extra
import Effect.Aff.Class (liftAff, class MonadAff) as Extra
import Effect.Class (liftEffect, class MonadEffect) as Extra
import Effect.Class.Console (error, log, info, logShow) as Extra
import Effect.Ref (Ref) as Extra
import Foreign.Object (Object) as Extra
import Node.Buffer (Buffer) as Extra
import Node.Encoding (Encoding(..)) as Extra
import Node.Path (FilePath) as Extra
import Partial.Unsafe (unsafeCrashWith) as Extra
import Registry.Json (Json, class RegistryJson) as Registry.Json
import Registry.Types (RawPackageName(..), RawVersion(..)) as Registry.Types

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
