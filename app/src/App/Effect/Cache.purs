module Registry.App.Effect.Cache where

import Registry.App.Prelude hiding (Manifest(..))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Const (Const(..))
import Data.DateTime (DateTime)
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Map as Map
import Data.Maybe (maybe')
import Data.String as String
import Effect.Aff as Aff
import Effect.Ref as Ref
import JSURI as JSURI
import Node.FS.Aff as FS.Aff
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.Types as Types
import Registry.Foreign.Octokit (GitHubError, GitHubRoute)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
import Registry.Manifest as Manifest
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run

-- | A typed key for the standard app cache. Caches using this key should
-- | use the 'get', 'put', and 'delete' functions from this module.
data AppCache (c :: Type -> Type -> Type) a
  = Manifest PackageName Version (c Manifest.Manifest a)
  | Request GitHubRoute (c (Either GitHubError Json) a)
  | LegacyPackageSet String (c (Either GitHubError Types.LegacyPackageSet) a)
  | LegacyPackageSetUnion Sha256 (c Types.LegacyPackageSetUnion a)

-- Ideally, with quantified constraints, this could be written as:
--   (forall x. Functor (c x)) => Functor (AppCache c)
-- but since PureScript doesn't have them, we lean on a 'Functor2' class and
-- a manual instance.
instance Functor2 c => Functor (AppCache c) where
  map k = case _ of
    Manifest name version a -> Manifest name version (map2 k a)
    Request route a -> Request route (map2 k a)
    LegacyPackageSet ref a -> LegacyPackageSet ref (map2 k a)
    LegacyPackageSetUnion tagsHash a -> LegacyPackageSetUnion tagsHash (map2 k a)

-- | A cache covering the common types used in the registry. Can be combined
-- | with other caches so long as they use different labels.
type CACHE r = (appCache :: Cache AppCache | r)

_appCache :: Proxy "appCache"
_appCache = Proxy

-- | A handler for the AppCache key for JSON caches. Can be used to
-- | implement interpreters for the CACHE effect.
jsonKeyHandler :: JsonKeyHandler AppCache
jsonKeyHandler = case _ of
  Manifest name version next -> Exists.mkExists $ flip JsonKey next
    { id: "AppCache__ManifestFile__" <> PackageName.print name <> "__" <> Version.print version
    , codec: Manifest.codec
    }
  Request route next -> Exists.mkExists $ flip JsonKey next
    { id: "AppCache__GitHubRequest__" <> Octokit.printGitHubRoute route
    , codec: CA.Common.either Octokit.githubErrorCodec CA.Common.json
    }
  LegacyPackageSet ref next -> Exists.mkExists $ flip JsonKey next
    { id: "AppCache__LegacyPackageSet__" <> ref
    , codec: CA.Common.either Octokit.githubErrorCodec Types.legacyPackageSetCodec
    }
  LegacyPackageSetUnion tagsHash next -> Exists.mkExists $ flip JsonKey next
    { id: "AppCache__LegacyPackageSetUnion__" <> Sha256.print tagsHash
    , codec: Types.legacyPackageSetUnionCodec
    }

-- | Get an item from the app cache.
get :: forall a r. CacheKey AppCache a -> Run (CACHE + r) (Maybe (CacheEntry a))
get key = Run.lift _appCache (getCache key)

-- | Put an item to the app cache.
-- | ```
put :: forall a r. CacheKey AppCache a -> a -> Run (CACHE + r) Unit
put key val = Run.lift _appCache (putCache key val)

-- | Delete an item from the app cache.
delete :: forall a r. CacheKey AppCache a -> Run (CACHE + r) Unit
delete key = Run.lift _appCache (deleteCache key)

-- | Handle the application cache using the file system
handleAppCacheFs :: forall a r. Ref (Map String Json) -> Cache AppCache a -> Run (LOG + AFF + EFFECT + r) a
handleAppCacheFs ref = handleCacheFs ref jsonKeyHandler

-- INTERNAL
--
-- The code below is used to implement arbitrary caches such as the app cache
-- above. You can use it to implement independent caches that can be handled
-- differently, or to extend the cache with new keys in downstream modules such
-- as the scripts.

class Functor2 (c :: Type -> Type -> Type) where
  map2 :: forall a b z. (a -> b) -> c z a -> c z b

newtype Reply a b = Reply (Maybe (CacheEntry a) -> b)

instance Functor2 Reply where
  map2 k (Reply f) = Reply (map k f)

newtype Ignore :: forall k. k -> Type -> Type
newtype Ignore a b = Ignore b

instance Functor2 Ignore where
  map2 k (Ignore b) = Ignore (k b)

-- | An effect for caching values with an extensible key to support multiple
-- | independent caches.
data Cache key a
  = Get (key Reply a)
  | Put (forall void. key Const void) a
  | Delete (key Ignore a)

derive instance (Functor (k Reply), Functor (k Ignore)) => Functor (Cache k)

type CacheKey :: ((Type -> Type -> Type) -> Type -> Type) -> Type -> Type
type CacheKey k a = forall c b. c a b -> k c b

-- | An entry in the cache, including its last-modified time.
type CacheEntry a =
  { modified :: DateTime
  , value :: a
  }

cacheEntryCodec :: forall a. JsonCodec a -> JsonCodec (CacheEntry a)
cacheEntryCodec codec = CA.Record.object "CacheEntry"
  { modified: Internal.Codec.iso8601DateTime
  , value: codec
  }

-- | Get a value from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'get' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data MyCache c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | type MY_CACHE r = (myCache :: Cache MyCache r)
-- | _myCache = Proxy :: Proxy "myCache"
-- |
-- | get :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe (CacheEntry a))
-- | get key = Run.lift _myCache (getCache key)
-- | ```
getCache :: forall k a. CacheKey k a -> Cache k (Maybe (CacheEntry a))
getCache key = Get (key (Reply identity))

-- | Put a value in the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'put' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data MyCache c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | type MY_CACHE r = (myCache :: Cache MyCache r)
-- | _myCache = Proxy :: Proxy "myCache"
-- |
-- | put :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | put key = Run.lift _myCache (putCache key)
-- | ```
putCache :: forall k a. CacheKey k a -> a -> Cache k Unit
putCache key value = Put (key (Const value)) unit

-- | Delete a key from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'delete' for a user-defined key type:
-- |
-- | ```purs
-- | import Registry.Effect.Cache
-- |
-- | data MyCache c a = ManifestFile PackageName Version (c Manifest a)
-- |
-- | type MY_CACHE r = (myCache :: Cache MyCache r)
-- | _myCache = Proxy :: Proxy "myCache"
-- |
-- | delete :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | delete key = Run.lift _myCache (deleteCache key)
-- | ```
deleteCache :: forall k a. CacheKey k a -> Cache k Unit
deleteCache key = Delete (key (Ignore unit))

-- | Given some type `a` to be stored in the cache, provides a unique identifier
-- | for a value of that type and a codec for encoding and decoding it as JSON.
type JsonKey a =
  { id :: String
  , codec :: JsonCodec a
  }

-- | Convert a cache identifier into a safe file path.
safePath :: String -> FilePath
safePath id =
  maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
    $ JSURI.encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") id

-- | A mapping of key types to a unique cache identifier and codec for encoding
-- | and decoding the value as JSON. This uses an existential encoding, so you
-- | must use `Exists.mkExists` to hide the value's type.
-- |
-- | ```purs
-- | import Data.Exists as Exists
-- | import Registry.Effect.Cache as Cache
-- | import Registry.Manifest as Manifest
-- |
-- | data Key c a = ManifestFile PackageName Version (c Manifest.Manifest a)
-- |
-- | keyHandler :: Cache.JsonKeyHandler Key
-- | keyHandler = case _ of
-- |   ManifestFile name version k -> do
-- |     let id = "ManifestFile" <> print name <> print version
-- |     Exists.mkExists $ Cache.JsonKey { id, codec: Manifest.codec } next
-- | ```
type JsonKeyHandler key = forall b z. key z b -> JsonEncoded z b

-- | A box that can be used with `Exists` to hide the value associated with a
-- | particular key constructor.
data JsonEncodedBox :: (Type -> Type -> Type) -> Type -> Type -> Type
data JsonEncodedBox z b a = JsonKey (JsonKey a) (z a b)

type JsonEncoded z b = Exists (JsonEncodedBox z b)

-- | Run a cache backed by the file system. The cache will try to minimize
-- | writes and reads to the file system by storing data in memory when possible.
--
-- TODO: This could be much improved, but it's not urgent. Some ideas:
--   - Accept a fetching function to produce the cached value, and call it if
--     the cache has no entry? This makes the cache more transparent vs. get/put.
--   - Separate the in-memory and filesystem handlers such that they can be
--     combined? ie. try in-memory, fall back to file system, fall back to fetcher.
--   - Push more cache behavior into the key handler, such that a key can specify
--     how it should be cached (memory only, file system only?)
handleCacheFs :: forall key a r. Ref (Map String Json) -> JsonKeyHandler key -> Cache key a -> Run (LOG + AFF + EFFECT + r) a
handleCacheFs cacheRef handler = case _ of
  -- TODO: Expire entries after they've not been fetched for N seconds?
  Get key -> handler key # Exists.runExists \(JsonKey { id, codec } (Reply reply)) -> do
    readMemory >>= Map.lookup id >>> case _ of
      Nothing -> do
        Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 (safePath id))) >>= case _ of
          Left error -> do
            Log.debug $ "Did not find " <> id <> " in memory or file system cache: " <> Aff.message error
            pure $ reply Nothing
          Right value -> do
            Log.debug $ "Found " <> id <> " in file system cache, writing to memory..."
            case Argonaut.Parser.jsonParser value of
              Left error -> do
                Log.debug $ "Invalid JSON for " <> id <> ":\n" <> value <> ").\nParsing failed with error: " <> error <> "\nRemoving from cache!"
                deleteMemoryAndDisk id
                pure $ reply Nothing
              Right json -> case CA.decode (cacheEntryCodec codec) json of
                Left error -> do
                  Log.debug $ "Failed to decode JSON for " <> id <> ":\n" <> value <> ").\nParsing failed with error: " <> CA.printJsonDecodeError error <> "\nRemoving from cache!"
                  deleteMemoryAndDisk id
                  pure $ reply Nothing
                Right decoded ->
                  pure $ reply $ Just decoded

      Just json -> do
        Log.debug $ "Found " <> id <> " in memory cache."
        case CA.decode (cacheEntryCodec codec) json of
          Left error -> do
            Log.debug $ "Failed to decode JSON for " <> id <> ":\n" <> Argonaut.Core.stringify json <> ").\nParsing failed with error: " <> CA.printJsonDecodeError error <> "\nRemoving from cache!"
            deleteMemoryAndDisk id
            pure $ reply Nothing
          Right decoded ->
            pure $ reply $ Just decoded

  Put key next -> handler key # Exists.runExists \(JsonKey { id, codec } (Const value)) -> do
    Log.debug $ "Putting " <> id <> " in cache..."
    now <- Run.liftAff $ liftEffect nowUTC
    let encoded = CA.encode (cacheEntryCodec codec) { modified: now, value }
    readMemory >>= Map.lookup id >>> case _ of
      Just previous | encoded == previous -> do
        Log.debug "Put value matches old value, skipping update."
        pure next
      _ -> do
        modifyMemory (Map.insert id encoded)
        Run.liftAff (Aff.attempt (FS.Aff.writeTextFile UTF8 (safePath id) (Argonaut.Core.stringify encoded))) >>= case _ of
          Left error -> Log.debug $ "Unable to write cache entry to file system: " <> Aff.message error
          Right _ -> Log.debug "Wrote cache entry!"
        pure next

  Delete key -> handler key # Exists.runExists \(JsonKey { id } (Ignore next)) -> do
    deleteMemoryAndDisk id
    pure next

  where
  readMemory = Run.liftEffect $ Ref.read cacheRef

  modifyMemory k = Run.liftEffect $ Ref.modify_ k cacheRef

  deleteMemoryAndDisk id = do
    modifyMemory (Map.delete id)
    Run.liftAff (Aff.attempt (FS.Aff.rm (safePath id))) >>= case _ of
      Left fsError -> Log.debug $ "Could not delete file system entry: " <> Aff.message fsError
      Right _ -> pure unit
