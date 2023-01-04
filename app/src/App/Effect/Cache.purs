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
  , get
  , put
  , delete
  , runCache
  , MemoryFsEnv(..)
  , handleCacheMemoryFs
  , CacheValue
  , CacheRef(..)
  , newCacheRef
  , class MemoryEncodable
  , encodeMemory
  , MemoryEncoding(..)
  , handleCacheMemory
  , class FsEncodable
  , encodeFs
  , FsEncoding(..)
  , handleCacheFs
  , Cache(..)
  ) where

import Registry.App.Prelude

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

-- | A type used with 'get' to associate the key with the return type of 'get',
-- | providing type safety to the cache.
newtype Reply a b = Reply (Maybe a -> b)

instance Functor2 Reply where
  map2 k (Reply f) = Reply (map k f)

-- | A type used with 'delete' to ignore type information associated with a key,
-- | as it isn't used when deleting information.
data Ignore (a :: Type) b = Ignore b

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

derive instance (Functor (key Reply), Functor (key Ignore)) => Functor (Cache key)

-- | A synonym for a partially-applied cache key (one usable with any of Reply,
-- | Const, or Ignore). For example:
-- |
-- | ```purs
-- | data MyKey c a = Package String (c Manifest a)
-- |
-- | packageKey :: CacheKey MyKey
-- | packageKey = Package "aff"
-- | ```
type CacheKey :: ((Type -> Type -> Type) -> Type -> Type) -> Type -> Type
type CacheKey k a = forall c b. c a b -> k c b

-- | Get a value from the cache
get
  :: forall sym q k r a
   . Functor (k Reply)
  => Functor (k Ignore)
  => IsSymbol sym
  => Row.Cons sym (Cache k) q r
  => Proxy sym
  -> CacheKey k a
  -> Run r (Maybe a)
get label key = Run.lift label (Get (key (Reply identity)))

-- | Put a value to the cache
put
  :: forall sym q k r a
   . Functor (k Reply)
  => Functor (k Ignore)
  => IsSymbol sym
  => Row.Cons sym (Cache k) q r
  => Proxy sym
  -> CacheKey k a
  -> a
  -> Run r Unit
put label key value = Run.lift label (Put (key (Const value)) unit)

-- | Delete a key from the cache
delete
  :: forall sym q k r a
   . Functor (k Reply)
  => Functor (k Ignore)
  => IsSymbol sym
  => Row.Cons sym (Cache k) q r
  => Proxy sym
  -> CacheKey k a
  -> Run r Unit
delete label key = Run.lift label (Delete (key (Ignore unit)))

runCache
  :: forall s k a r t
   . IsSymbol s
  => Row.Cons s (Cache k) t r
  => Proxy s
  -> (Cache k ~> Run t)
  -> Run r a
  -> Run t a
runCache label handler = Run.interpret (Run.on label handler Run.send)

-- | The environment for a combined in-memory cache backed by the file system
type MemoryFsEnv =
  { ref :: CacheRef
  , cache :: FilePath
  }

handleCacheMemoryFs :: forall k r a. MemoryEncodable k => FsEncodable k => MemoryFsEnv -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheMemoryFs env = case _ of
  Get key -> Exists.runExists getImpl (encodeMemory key)
    where
    getImpl :: forall x. MemoryEncoding _ _ x -> Run _ a
    getImpl (Key memory (Reply reply)) = do
      let (unCache :: CacheValue -> x) = unsafeCoerce
      inMemory <- getMemoryImpl env.ref (Key memory (Reply identity))
      case inMemory of
        Nothing -> do
          inFs <- encodeFs key # Exists.runExists case _ of
            AsJson fs codec (Reply _) -> do
              value <- getFsImpl env.cache (AsJson fs codec (Reply identity))
              pure (map toCacheValue value)
            AsBuffer fs (Reply _) -> do
              buffer <- getFsImpl env.cache (AsBuffer fs (Reply identity))
              pure (map toCacheValue buffer)
          case inFs of
            Nothing -> pure $ reply Nothing
            Just entry -> do
              Log.debug $ "Fell back to on-disk entry for " <> memory
              putMemoryImpl env.ref unit (Key memory (Const entry))
              pure $ reply $ Just $ unCache entry
        Just cached ->
          pure $ reply $ Just cached

  Put key next -> do
    Exists.runExists (putMemoryImpl env.ref unit) (encodeMemory key)
    Exists.runExists (putFsImpl env.cache unit) (encodeFs key)
    pure next

  Delete key -> do
    void $ Exists.runExists (deleteMemoryImpl env.ref) (encodeMemory key)
    Exists.runExists (deleteFsImpl env.cache) (encodeFs key)

-- | A class for encoding the values associated with a cache key in a form
-- | suitable for the file system.
class MemoryEncodable key where
  encodeMemory :: forall b z. key z b -> Exists (MemoryEncoding z b)

-- | An encoding for keys so they can be stored in a map (we don't have an Ord
-- | instance for key types, but they can generally be mapped to a String, which
-- | does have one).
data MemoryEncoding :: (Type -> Type -> Type) -> Type -> Type -> Type
data MemoryEncoding z b a = Key String (z a b)

-- | An opaque type representing a cached value.
data CacheValue

toCacheValue :: forall a. a -> CacheValue
toCacheValue = unsafeCoerce

-- | Hide the type information associated with a value so it can be placed into
-- | an in-memory map with heterogeneous values.
-- | A type synonym for an in-memory cache as a mutable map.
type CacheRef = Ref (Map String CacheValue)

newCacheRef :: forall m. MonadEffect m => m (CacheRef)
newCacheRef = liftEffect $ Ref.new Map.empty

-- | Handle the Cache effect by caching values in a map in memory, given a way
-- | to encode keys as a type with an Ord instance.
--
-- Note: This doesn't currently support expiration, but we can implement it by
-- storing a fiber along with each cache value, where the fiber will delete the
-- value after _n_ minutes. When the value is accessed the fiber is killed and
-- a new one spawned.
handleCacheMemory :: forall k r a. MemoryEncodable k => CacheRef -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheMemory ref = case _ of
  Get key -> Exists.runExists (getMemoryImpl ref) (encodeMemory key)
  Put key next -> Exists.runExists (putMemoryImpl ref next) (encodeMemory key)
  Delete key -> Exists.runExists (deleteMemoryImpl ref) (encodeMemory key)

getMemoryImpl :: forall a b r. CacheRef -> MemoryEncoding Reply a b -> Run (LOG + EFFECT + r) a
getMemoryImpl ref (Key id (Reply reply)) = do
  let (unCache :: CacheValue -> b) = unsafeCoerce
  cache <- Run.liftEffect $ Ref.read ref
  case Map.lookup id cache of
    Nothing -> do
      Log.debug $ "No cache entry found for " <> id <> " in memory."
      pure $ reply Nothing
    Just cached -> do
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

-- | A class for encoding the values associated with a cache key in a form
-- | suitable for the file system.
class FsEncodable key where
  encodeFs :: forall b z. key z b -> Exists (FsEncoding z b)

-- | Capture the file system encoding suitable for a particular key. Essentially
-- | these are serialization formats; we can add more formats if we ever need to
-- | cache values as something other than JSON or a raw buffer.
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
handleCacheFs :: forall k r a. FsEncodable k => FilePath -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheFs cacheDir = case _ of
  Get key -> Exists.runExists (getFsImpl cacheDir) (encodeFs key)
  Put key next -> Exists.runExists (putFsImpl cacheDir next) (encodeFs key)
  Delete key -> Exists.runExists (deleteFsImpl cacheDir) (encodeFs key)

getFsImpl :: forall a b r. FilePath -> FsEncoding Reply a b -> Run (LOG + AFF + r) a
getFsImpl cacheDir = case _ of
  AsBuffer id (Reply reply) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left _ -> do
        Log.debug $ "No cache found for " <> id <> " at path " <> path
        pure $ reply Nothing
      Right buf -> do
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
