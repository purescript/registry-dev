module Registry.Prelude
  ( module Prelude
  , module Extra
  , module Either
  , module Maybe
  , PackageURL(..)
  , partitionEithers
  , readJsonFile
  , writeJsonFile
  , stripPureScriptPrefix
  , newlines
  , objectFromMap
  , objectToMap
  , fromJust'
  ) where

import Prelude

import Control.Monad.Error.Class (throwError) as Extra
import Control.Monad.Except (ExceptT(..)) as Extra
import Control.Monad.Trans.Class (lift) as Extra
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray) as Extra
import Data.Bifunctor (bimap, lmap, rmap) as Extra
import Data.Either (Either(..), either, fromLeft, fromRight', isRight, hush, note) as Either
import Data.Foldable (and, any, all, fold) as Extra
import Data.FoldableWithIndex (forWithIndex_, foldlWithIndex) as Extra
import Data.Identity (Identity) as Extra
import Data.List (List) as Extra
import Data.Map (Map) as Extra
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing, isJust) as Maybe
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
import Effect.Class.Console (error, log, info) as Extra
import Effect.Ref (Ref) as Extra
import Foreign.Jsonic as Jsonic
import Foreign.Object (Object) as Extra
import Foreign.Object as Object
import Node.Buffer (Buffer) as Extra
import Node.Encoding (Encoding(..)) as Extra
import Node.FS.Aff as FS
import Node.Path (FilePath) as Extra
import Partial.Unsafe (unsafePartial, unsafeCrashWith) as Extra

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either.Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Either.Left err -> { fail: [ err ], success: [] }
  Either.Right res -> { fail: [], success: [ res ] }

-- | Encode data as JSON and write it to the provided filepath
writeJsonFile :: forall a. Json.EncodeJson a => Extra.FilePath -> a -> Extra.Aff Unit
writeJsonFile path = FS.writeTextFile Extra.UTF8 path <<< Json.stringifyWithIndent 2 <<< Json.encodeJson

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a. Json.DecodeJson a => Extra.FilePath -> Extra.Aff (Either.Either Json.JsonDecodeError a)
readJsonFile path = do
  contents <- FS.readTextFile Extra.UTF8 path
  pure $ Json.decodeJson =<< Jsonic.parseJson contents

-- | Convert a Map into an Object, converting its keys to strings along the way.
objectFromMap :: forall k a. Ord k => (k -> String) -> Extra.Map k a -> Extra.Object a
objectFromMap toString = Object.fromFoldable <<< map (Extra.lmap toString) <<< (Map.toUnfoldable :: _ -> Array _)

-- | Convert an Object into a Map. Keys that cannot be parsed from a string will
-- | be removed from the map.
objectToMap :: forall k a. Ord k => (String -> Maybe.Maybe k) -> Extra.Object a -> Extra.Map k a
objectToMap fromString = do
  let
    parse (Extra.Tuple str a) = case fromString str of
      Maybe.Nothing -> Maybe.Nothing
      Maybe.Just key -> Maybe.Just (Extra.Tuple key a)

  Map.fromFoldable <<< Array.catMaybes <<< map parse <<< (Object.toUnfoldable :: _ -> Array _)

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

newtype PackageURL = PackageURL String

derive instance Extra.Newtype PackageURL _
derive newtype instance Eq PackageURL
derive newtype instance Ord PackageURL
derive newtype instance Json.EncodeJson PackageURL
derive newtype instance Json.DecodeJson PackageURL

fromJust' :: forall a. (Unit -> a) -> Maybe.Maybe a -> a
fromJust' _ (Maybe.Just a) = a
fromJust' failed _ = failed unit
