module Registry.App.Prelude
  ( LogVerbosity(..)
  , PackageSource(..)
  , PursPublishMethod(..)
  , Retry
  , RetryResult(..)
  , URL
  , class Functor2
  , defaultRetry
  , formatPackageVersion
  , fromJust'
  , map2
  , mapKeys
  , module Either
  , module Extra
  , module Maybe
  , module Prelude
  , module Registry.Types
  , nowUTC
  , pacchettibottiEmail
  , pacchettibottiKeyType
  , parseJson
  , parseYaml
  , partitionEithers
  , printJson
  , printPackageSource
  , pursPublishMethod
  , readJsonFile
  , readYamlFile
  , scratchDir
  , stringifyJson
  , traverseKeys
  , unsafeFromJust
  , unsafeFromRight
  , withRetry
  , withRetryOnTimeout
  , writeJsonFile
  ) where

import Prelude

import Control.Alt ((<|>)) as Extra
import Control.Alternative (guard) as Extra
import Control.Monad.Except (ExceptT(..)) as Extra
import Control.Monad.Trans.Class (lift) as Extra
import Control.Parallel.Class as Parallel
import Data.Argonaut.Core (Json) as Extra
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray) as Extra
import Data.Bifunctor (bimap, lmap) as Extra
import Data.Bitraversable (ltraverse) as Extra
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError) as Extra
import Data.Codec.Argonaut as CA
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
import Data.String.NonEmpty (NonEmptyString) as Extra
import Data.Time.Duration as Duration
import Data.Traversable (for, for_, sequence, traverse) as Extra
import Data.TraversableWithIndex (forWithIndex) as Extra
import Data.Tuple (Tuple(..), fst, snd) as Extra
import Data.Tuple.Nested ((/\)) as Extra
import Effect (Effect) as Extra
import Effect.Aff (Aff, launchAff_, try) as Extra
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff) as Extra
import Effect.Class (class MonadEffect, liftEffect) as Extra
import Effect.Now as Now
import Effect.Ref (Ref) as Extra
import Foreign.Object (Object) as Extra
import Node.Buffer (Buffer) as Extra
import Node.Encoding (Encoding(..)) as Extra
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath) as Extra
import Partial.Unsafe (unsafeCrashWith) as Extra
import Registry.Foreign.Yaml as Yaml
import Registry.PackageName (stripPureScriptPrefix) as Extra
import Registry.PackageName as PackageName
import Registry.Types (License, Location(..), Manifest(..), ManifestIndex, Metadata(..), Owner(..), PackageName, PackageSet(..), PublishedMetadata, Range, Sha256, UnpublishedMetadata, Version)
import Registry.Version as Version
import Type.Proxy (Proxy(..)) as Extra
import Type.Row (type (+)) as Extra

type URL = String

class Functor2 (c :: Type -> Type -> Type) where
  map2 :: forall a b z. (a -> b) -> c z a -> c z b

-- | The location of the .gitignored scratch directory
scratchDir :: Extra.FilePath
scratchDir = "scratch"

-- | The email address of the @pacchettibotti account
pacchettibottiEmail :: String
pacchettibottiEmail = "pacchettibotti@purescript.org"

-- | The public key type of the @pacchettibotti account
pacchettibottiKeyType :: String
pacchettibottiKeyType = "ssh-ed25519"

-- | Print a type as a formatted JSON string
printJson :: forall a. Extra.JsonCodec a -> a -> String
printJson codec = Argonaut.stringifyWithIndent 2 <<< CA.encode codec

-- | Print a type as a JSON string without formatting
stringifyJson :: forall a. Extra.JsonCodec a -> a -> String
stringifyJson codec = Argonaut.stringify <<< CA.encode codec

-- | Parse a type from a string of JSON data.
parseJson :: forall a. Extra.JsonCodec a -> String -> Either.Either Extra.JsonDecodeError a
parseJson codec = CA.decode codec <=< Extra.lmap (\err -> CA.TypeMismatch ("JSON: " <> err)) <<< Argonaut.Parser.jsonParser

-- | Encode data as formatted JSON and write it to the provided filepath
writeJsonFile :: forall a. Extra.JsonCodec a -> Extra.FilePath -> a -> Extra.Aff Unit
writeJsonFile codec path = FS.Aff.writeTextFile Extra.UTF8 path <<< (_ <> "\n") <<< printJson codec

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a. Extra.JsonCodec a -> Extra.FilePath -> Extra.Aff (Either.Either String a)
readJsonFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile Extra.UTF8 path
  pure (Extra.lmap Aff.message result >>= parseJson codec >>> Extra.lmap CA.printJsonDecodeError)

-- | Parse a type from a string of YAML data after converting it to JSON.
parseYaml :: forall a. Extra.JsonCodec a -> String -> Either.Either String a
parseYaml codec yaml = do
  json <- Extra.lmap (append "YAML: ") (Yaml.yamlParser yaml)
  Extra.lmap CA.printJsonDecodeError (CA.decode codec json)

-- | Decode data from a YAML file at the provided filepath
readYamlFile :: forall a. Extra.JsonCodec a -> Extra.FilePath -> Extra.Aff (Either.Either String a)
readYamlFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile Extra.UTF8 path
  pure (Extra.lmap Aff.message result >>= parseYaml codec)

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either.Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Either.Left err -> { fail: [ err ], success: [] }
  Either.Right res -> { fail: [], success: [ res ] }

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
traverseKeys k = map Map.fromFoldable <<< Extra.traverse (Extra.ltraverse k) <<< (Map.toUnfoldable :: _ -> Array _)

withRetryOnTimeout :: forall err a. Extra.Aff (Either.Either err a) -> Extra.Aff (RetryResult err a)
withRetryOnTimeout = withRetry defaultRetry

type Retry err =
  { timeout :: Aff.Milliseconds
  , retryOnCancel :: Int -> Boolean
  , retryOnFailure :: Int -> err -> Boolean
  }

-- | Default retry configuration, which retries on cancellation but does not
-- | retry on failure.
defaultRetry :: forall err. Retry err
defaultRetry =
  { timeout: Aff.Milliseconds 5000.0
  , retryOnCancel: \attempt -> attempt <= 3
  , retryOnFailure: \_ _ -> false
  }

data RetryResult err a
  = Cancelled
  | Failed err
  | Succeeded a

derive instance (Eq err, Eq a) => Eq (RetryResult err a)

-- | Attempt an effectful computation that can fail by specifying how to retry
-- | the request and whether it should time out.
withRetry :: forall err a. Retry err -> Extra.Aff (Either.Either err a) -> Extra.Aff (RetryResult err a)
withRetry { timeout: Aff.Milliseconds timeout, retryOnCancel, retryOnFailure } action = do
  let
    runAction :: Extra.Aff (Either.Either err a) -> Int -> Extra.Aff (RetryResult err a)
    runAction action' ms = do
      Parallel.sequential $ Foldable.oneOf
        [ Parallel.parallel $ action' >>= case _ of
            Either.Left err -> pure $ Failed err
            Either.Right val -> pure $ Succeeded val
        , Parallel.parallel (runTimeout ms)
        ]

    runTimeout :: Int -> Extra.Aff (RetryResult err a)
    runTimeout ms = do
      _ <- Aff.delay (Aff.Milliseconds (Int.toNumber ms))
      pure Cancelled

    retry :: Int -> RetryResult err a -> Extra.Aff (RetryResult err a)
    retry attempt = case _ of
      Cancelled ->
        if retryOnCancel attempt then do
          let newTimeout = Int.floor timeout `Int.pow` (attempt + 1)
          retry (attempt + 1) =<< runAction action newTimeout
        else
          pure Cancelled
      Failed err ->
        if retryOnFailure attempt err then do
          let newTimeout = Int.floor timeout `Int.pow` (attempt + 1)
          retry (attempt + 1) =<< runAction action newTimeout
        else
          pure (Failed err)
      Succeeded result ->
        pure (Succeeded result)

  retry 1 =<< runAction action (Int.floor timeout)

-- | Get the current time, standardizing on the UTC timezone to avoid ambiguity
-- | when running on different machines.
nowUTC :: forall m. Extra.MonadEffect m => m DateTime
nowUTC = Extra.liftEffect do
  offset <- Newtype.over Duration.Minutes negate <$> Now.getTimezoneOffset
  now <- Now.nowDateTime
  pure $ Maybe.fromMaybe now $ DateTime.adjust (offset :: Duration.Minutes) now

formatPackageVersion :: PackageName -> Version -> String
formatPackageVersion name version = PackageName.print name <> "@" <> Version.print version

data LogVerbosity = Quiet | Normal | Verbose

derive instance Eq LogVerbosity
derive instance Ord LogVerbosity

-- | A temporary flag that records whether we are using legacy purs publish
-- | (which requires all packages to be a Git repository) or new purs publish
-- | (which accepts any directory with package sources).
data PursPublishMethod = LegacyPursPublish | PursPublish

-- | The current purs publish method
pursPublishMethod :: PursPublishMethod
pursPublishMethod = LegacyPursPublish

-- | Operations can be exercised for old, pre-registry packages, or for packages
-- | which are on the 0.15 compiler series. If a true legacy package is uploaded
-- | then we do not require compilation to succeed and we don't publish docs.
data PackageSource = LegacyPackage | CurrentPackage

derive instance Eq PackageSource

printPackageSource :: PackageSource -> String
printPackageSource = case _ of
  LegacyPackage -> "legacy"
  CurrentPackage -> "current"
