module Registry.Prelude
  ( module Prelude
  , module Extra
  , module Either
  , module Maybe
  , partitionEithers
  , unpackErrors
  , writeJsonFile
  , stripPureScriptPrefix
  , newlines
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, stringifyWithIndent)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap, rmap) as Extra
import Data.Either (Either(..), either, fromLeft, fromRight', isRight, hush, note) as Either
import Data.Foldable (and) as Extra
import Data.FoldableWithIndex (forWithIndex_) as Extra
import Data.Identity (Identity) as Extra
import Data.List (List) as Extra
import Data.Map (Map) as Extra
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing, isJust) as Maybe
import Data.Newtype (un, class Newtype) as Extra
import Data.Nullable (toMaybe, toNullable, Nullable) as Extra
import Data.Set (Set) as Extra
import Data.Show.Generic (genericShow) as Extra
import Data.String as String
import Data.Traversable (for, for_, traverse, sequence) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..)) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect) as Extra
import Effect.Aff (Aff, launchAff_, try) as Extra
import Effect.Aff.Class (liftAff, class MonadAff) as Extra
import Effect.Class (liftEffect, class MonadEffect) as Extra
import Effect.Class.Console (error, log, info) as Extra
import Effect.Ref (Ref) as Extra
import Foreign.Object (Object) as Extra
import Node.Buffer (Buffer) as Extra
import Node.Encoding (Encoding(..)) as Extra
import Node.FS.Aff as FS
import Node.Path (FilePath) as Extra

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either.Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Either.Left err -> { fail: [ err ], success: [] }
  Either.Right res -> { fail: [], success: [ res ] }

unpackErrors :: forall e a. Either.Either e a -> Maybe.Maybe e
unpackErrors =
  case _ of
    Either.Left e -> Maybe.Just e
    Either.Right _ -> Maybe.Nothing

-- | Encode data as JSON and write it to the provided filepath
writeJsonFile :: forall a. EncodeJson a => Extra.FilePath -> a -> Extra.Aff Unit
writeJsonFile path = FS.writeTextFile Extra.UTF8 path <<< stringifyWithIndent 2 <<< encodeJson

-- | Strip the "purescript-" prefix from a package name, if present.
-- |
-- | ```purs
-- | > stripPureScriptPrefix "purescript-numbers"
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
