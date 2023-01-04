-- | A generic, extensible, typed cache effect suitable for retaining data that
-- | is slow or expensive to compute. Multiple caches can be created so long as
-- | they use different keys, and caches can be interpreted with different
-- | strategies, such as in-memory only, or backed by the file system, or by a
-- | database like SQLite.
-- |
-- | A typed cache is more complicated than an untyped one, but it has two
-- | significant benefits: first, you know what type you will receive when
-- | reading from the cache, and second, you can store types directly to
-- | in-memory caches, without the overhead of serialization and deserialization.
module Registry.App.Effect.Cache
  ( Reply(..)
  , Ignore(..)
  , CacheKey(..)
  , getCache
  , putCache
  , deleteCache
  , runCacheAt
  , MemoryFsCacheEnv(..)
  , handleCacheMemoryFs
  , MemoryCacheEnv(..)
  , CacheValue
  , CacheRef(..)
  , newCacheRef
  , MemoryEncoder(..)
  , MemoryEncoding(..)
  , handleCacheMemory
  , FsCacheEnv(..)
  , FsEncoder(..)
  , FsEncoding(..)
  , handleCacheFs
  , Cache(..)
  ) where

import Registry.App.Prelude hiding (Manifest(..), Metadata(..))

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Const (Const(..))
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.Symbol (class IsSymbol)
import Effect.Aff as Aff
import Effect.Ref as Ref
import JSURI as JSURI
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Prim.Row as Row
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Run (AFF, EFFECT, Run)
import Run as Run
import Unsafe.Coerce (unsafeCoerce)

newtype Reply a b = Reply (Maybe a -> b)

instance Functor2 Reply where
  map2 k (Reply f) = Reply (map k f)

newtype Ignore :: forall k. k -> Type -> Type
newtype Ignore a b = Ignore b

instance Functor2 Ignore where
  map2 k (Ignore b) = Ignore (k b)

-- | An effect for caching values with an extensible key to support multiple
-- | independent caches.
--
-- Note: We could also formulate this as a pair of Get / Alter, where Alter is
-- of type (Maybe a -> Maybe a) and allows you to insert, modify, or delete in
-- one operation.
data Cache key a
  = Get (key Reply a)
  | Put (forall void. key Const void) a
  | Delete (key Ignore a)

derive instance (Functor (k Reply), Functor (k Ignore)) => Functor (Cache k)

type CacheKey :: ((Type -> Type -> Type) -> Type -> Type) -> Type -> Type
type CacheKey k a = forall c b. c a b -> k c b

-- | Get a value from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'get' for a user-defined key type:
-- |
-- | ```purs
-- | get :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | get key = Run.lift _myCache (getCache key)
-- | ```
getCache :: forall k a. CacheKey k a -> Cache k (Maybe a)
getCache key = Get (key (Reply identity))

-- | Put a value in the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'put' for a user-defined key type:
-- |
-- | ```purs
-- | put :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | put key = Run.lift _myCache (putCache key)
-- | ```
putCache :: forall k a. CacheKey k a -> a -> Cache k Unit
putCache key value = Put (key (Const value)) unit

-- | Delete a key from the cache, given an appropriate cache key. This function
-- | is useful for defining a concrete 'delete' for a user-defined key type:
-- |
-- | ```purs
-- | delete :: forall a r. CacheKey MyCache a -> Run (MY_CACHE + r) (Maybe a)
-- | delete key = Run.lift _myCache (Cache.deleteCache key)
-- | ```
deleteCache :: forall k a. CacheKey k a -> Cache k Unit
deleteCache key = Delete (key (Ignore unit))

runCacheAt
  :: forall s k a r t
   . IsSymbol s
  => Row.Cons s (Cache k) t r
  => Proxy s
  -> (Cache k ~> Run t)
  -> Run r a
  -> Run t a
runCacheAt sym handler =
  Run.interpret (Run.on sym handler Run.send)

-- | The environment for a combined in-memory cache backed by the file system
type MemoryFsCacheEnv k =
  { ref :: CacheRef
  , fs :: FsEncoder k
  , memory :: MemoryEncoder k
  , cacheDir :: FilePath
  }

handleCacheMemoryFs :: forall k r a. MemoryFsCacheEnv k -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheMemoryFs env = case _ of
  Get key -> Exists.runExists get (env.memory key)
    where
    get :: forall x. MemoryEncoding _ _ x -> Run _ a
    get (Key memId (Reply reply)) = do
      let (unCache :: CacheValue -> x) = unsafeCoerce
      inMemory <- getMemoryImpl env.ref (Key memId (Reply identity))
      case inMemory of
        Nothing -> do
          inFs <- env.fs key # Exists.runExists case _ of
            AsJson fsId codec (Reply _) -> do
              value <- getFsImpl env.cacheDir (AsJson fsId codec (Reply identity))
              pure (map toCacheValue value)
            AsBuffer fsId (Reply _) -> do
              buffer <- getFsImpl env.cacheDir (AsBuffer fsId (Reply identity))
              pure (map toCacheValue buffer)
          case inFs of
            Nothing -> pure $ reply Nothing
            Just entry -> do
              putMemoryImpl env.ref unit (Key memId (Const entry))
              pure $ reply $ Just $ unCache entry
        Just cached ->
          pure $ reply $ Just cached

  Put key next -> do
    Exists.runExists (putMemoryImpl env.ref unit) (env.memory key)
    Exists.runExists (putFsImpl env.cacheDir unit) (env.fs key)
    pure next

  Delete key -> do
    _ <- Exists.runExists (deleteMemoryImpl env.ref) (env.memory key)
    Exists.runExists (deleteFsImpl env.cacheDir) (env.fs key)

-- | The environment for an in-memory cache implementation, where keys must
-- | be mappable to a type with an Ord instance.
type MemoryCacheEnv k =
  { ref :: CacheRef
  , encoder :: MemoryEncoder k
  }

type CacheRef = Ref (Map String CacheValue)

newCacheRef :: forall m. MonadEffect m => m (CacheRef)
newCacheRef = liftEffect $ Ref.new Map.empty

data CacheValue

toCacheValue :: forall a. a -> CacheValue
toCacheValue = unsafeCoerce

type MemoryEncoder key = forall b z. key z b -> Exists (MemoryEncoding z b)

-- | An encoding for keys so they can be stored in a map (we don't have an Ord
-- | instance for key types, but they can generally be mapped to a String, which
-- | does have one).
data MemoryEncoding :: (Type -> Type -> Type) -> Type -> Type -> Type
data MemoryEncoding z b a = Key String (z a b)

-- | Handle the Cache effect by caching values in a map in memory, given a way
-- | to encode keys as a type with an Ord instance.
--
-- Note: This doesn't currently support expiration, but we can implement it by
-- storing a fiber along with each cache value, where the fiber will delete the
-- value after _n_ minutes. When the value is accessed the fiber is killed and
-- a new one spawned.
handleCacheMemory :: forall k r a. MemoryCacheEnv k -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheMemory env = case _ of
  Get key -> Exists.runExists (getMemoryImpl env.ref) (env.encoder key)
  Put key next -> Exists.runExists (putMemoryImpl env.ref next) (env.encoder key)
  Delete key -> Exists.runExists (deleteMemoryImpl env.ref) (env.encoder key)

getMemoryImpl :: forall x r a. CacheRef -> MemoryEncoding Reply a x -> Run (LOG + EFFECT + r) a
getMemoryImpl ref (Key id (Reply reply)) = do
  let (unCache :: CacheValue -> x) = unsafeCoerce
  cache <- Run.liftEffect $ Ref.read ref
  case Map.lookup id cache of
    Nothing -> do
      Log.debug $ "No cache entry found for " <> id <> " in memory."
      pure $ reply Nothing
    Just cached -> do
      Log.debug $ "Read cache entry for " <> id <> " in memory."
      pure $ reply $ Just $ unCache cached

putMemoryImpl :: forall x r a. CacheRef -> a -> MemoryEncoding Const a x -> Run (LOG + EFFECT + r) a
putMemoryImpl ref next (Key id (Const value)) = do
  let (toCache :: x -> CacheValue) = unsafeCoerce
  Run.liftEffect $ Ref.modify_ (Map.insert id (toCache value)) ref
  Log.debug $ "Wrote cache entry for " <> id <> " in memory."
  pure next

deleteMemoryImpl :: forall x r a. CacheRef -> MemoryEncoding Ignore a x -> Run (LOG + EFFECT + r) a
deleteMemoryImpl ref (Key id (Ignore next)) = do
  Run.liftEffect $ Ref.modify_ (Map.delete id) ref
  pure next

-- | The environment for a filesystem-backed cache implementation, where values
-- | associated with the cache keys must be serializable to the file system.
type FsCacheEnv k =
  { encoder :: FsEncoder k
  , cacheDir :: FilePath
  }

-- | A mapping of key types to a unique cache identifier and codec for encoding
-- | and decoding the value as JSON. This uses an existential encoding, so you
-- | must use `Exists.mkExists` to hide the value's type.
-- |
-- | ```purs
-- | data MyKey c a
-- |   = MetadataFile PackageName (c Metadata a)
-- |   | Package PackageName Version (c Buffer a)
-- |
-- | myKeyHandler :: FsKeyHandler MyKey
-- | myKeyHandler = case _ of
-- |   MetadataFile name next -> mkExists $ AsJson (PackageName.print name) Metadata.codec next
-- |   Package name version next -> mkExists $ AsBuffer (formatPackageVersion name version) next
-- | ```
type FsEncoder key = forall b z. key z b -> Exists (FsEncoding z b)

-- | A box used with `Exists` to capture the encoding associated with values
-- | of a particular key. Essentially, these are serialization formats:
-- | sometimes we want a cache backed by JSON, sometimes backed by a raw buffer.
-- | We can add more if we ever need them.
data FsEncoding :: (Type -> Type -> Type) -> Type -> Type -> Type
data FsEncoding z b a
  = AsJson String (JsonCodec a) (z a b)
  | AsBuffer String (z Buffer b)

-- | Handle the Cache effect by caching values on the file system, given a
-- | file encoding to use.
--
-- Note: This doesn't currently support expiration. But we could support it by
-- looking at the 'mtime' for modification and 'atime' for access via stat and
-- expiring entries accessed too long ago.
handleCacheFs :: forall k r a. FsCacheEnv k -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheFs env = case _ of
  Get key -> Exists.runExists (getFsImpl env.cacheDir) (env.encoder key)
  Put key next -> Exists.runExists (putFsImpl env.cacheDir next) (env.encoder key)
  Delete key -> Exists.runExists (deleteFsImpl env.cacheDir) (env.encoder key)

getFsImpl :: forall a b r. FilePath -> FsEncoding Reply a b -> Run (LOG + AFF + r) a
getFsImpl cacheDir = case _ of
  AsBuffer id (Reply reply) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left _ -> do
        Log.debug $ "No cache found for " <> id <> " at path " <> path
        pure $ reply Nothing
      Right buf -> do
        Log.debug $ "Read cached buffer for " <> id
        pure $ reply $ Just buf

  AsJson id codec (Reply reply) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
      Left _ -> do
        Log.debug $ "No cache file found for " <> id <> " at path " <> path
        pure $ reply Nothing
      Right content -> case Argonaut.Parser.jsonParser content of
        Left parseError -> do
          Log.error $ "Found cache file for " <> id <> " at path " <> path <> " but its contents are not valid JSON: " <> parseError
          deletePathById cacheDir id *> pure (reply Nothing)
        Right jsonContent -> case CA.decode codec jsonContent of
          Left decodeError -> do
            let error = CA.printJsonDecodeError decodeError
            Log.error $ "Found cache file for " <> id <> " at path " <> path <> " but its contents could not be decoded with the provided codec:\n" <> error
            deletePathById cacheDir id *> pure (reply Nothing)
          Right entry -> do
            Log.debug $ "Read cached JSON for " <> id
            pure $ reply $ Just entry

putFsImpl :: forall a b r. FilePath -> a -> FsEncoding Const a b -> Run (LOG + AFF + r) a
putFsImpl cacheDir next = case _ of
  AsBuffer id (Const value) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    Run.liftAff (Aff.attempt (FS.Aff.writeFile path value)) >>= case _ of
      Left fsError -> do
        Log.warn $ "Failed to write cache entry for " <> id <> " at path " <> path <> " as a buffer: " <> Aff.message fsError
        pure next
      Right _ -> do
        Log.debug $ "Wrote cache entry for " <> id <> " as a buffer at path " <> path
        pure next

  AsJson id codec (Const value) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    Run.liftAff (Aff.attempt (writeJsonFile codec path value)) >>= case _ of
      Left fsError -> do
        Log.warn $ "Failed to write cache entry for " <> id <> " at path " <> path <> " as JSON: " <> Aff.message fsError
        pure next
      Right _ -> do
        Log.debug $ "Wrote cache entry for " <> id <> " at path " <> path <> " as JSON."
        pure next

deleteFsImpl :: forall a b r. FilePath -> FsEncoding Ignore a b -> Run (LOG + AFF + r) a
deleteFsImpl cacheDir = case _ of
  AsBuffer id (Ignore next) ->
    deletePathById cacheDir id *> pure next
  AsJson id _ (Ignore next) ->
    deletePathById cacheDir id *> pure next

deletePathById :: forall r. FilePath -> String -> Run (LOG + AFF + r) Unit
deletePathById cacheDir id = do
  let path = Path.concat [ cacheDir, safePath id ]
  Run.liftAff (Aff.attempt (FS.Aff.unlink path)) >>= case _ of
    Left fsError -> do
      Log.warn $ "Failed to delete cache entry for " <> id <> " at path " <> path <> ": " <> Aff.message fsError
    Right _ -> do
      Log.debug $ "Deleted cache entry for " <> id

safePath :: String -> FilePath
safePath id =
  Maybe.maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
    $ JSURI.encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") id
