module Registry.App.Monad.Cache where

import Registry.App.Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Const (Const(..))
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Effect.Aff as Aff
import Effect.Ref as Ref
import JSURI as JSURI
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
import Unsafe.Coerce (unsafeCoerce)

-- | A type used with 'get' to associate the key with the return type of 'get',
-- | providing type safety to the cache.
newtype Reply a b = Reply (Maybe a -> b)

-- | A type used with 'delete' to ignore type information associated with a key,
-- | as it isn't used when deleting information.
data Ignore (a :: Type) (b :: Type) = Ignore

-- | An effect for writing, reading, and deleting values from a cache that
-- | retains type information for keys and their values. Note: you should not
-- | use these type class members directly. Use 'get', 'put', and 'delete' from
-- | this module instead.
class Monad m <= MonadCache key m where
  getCache :: forall a. key Reply a -> m a
  -- Note: We could also formulate this as 'alterCache', where input values are
  -- of type (Maybe a -> Maybe a), and we can use it to implement insert /
  -- modify / delete. This would let the operation be atomic.
  putCache :: (forall void. key Const void) -> m Unit
  deleteCache :: (forall void. key Ignore void) -> m Unit

instance MonadCache key m => MonadCache key (ExceptT e m) where
  getCache key = lift (getCache key)
  putCache key = lift (putCache key)
  deleteCache key = lift (deleteCache key)

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

-- | Get the value associated with the cache key.
get :: forall m k a. MonadCache k m => CacheKey k a -> m (Maybe a)
get key = getCache (key (Reply identity))

-- | Put a value to the cache at the given cache key.
put :: forall m k a. MonadCache k m => CacheKey k a -> a -> m Unit
put key value = putCache (key (Const value))

-- | Delete a value from the cache at the given cache key.
delete :: forall m k a. MonadCache k m => CacheKey k a -> m Unit
delete key = deleteCache (key Ignore)

-- | The environment for a combined in-memory cache backed by the file system
type MemoryFsEnv =
  { ref :: CacheRef
  , cacheDir :: FilePath
  }

handleGetMemoryFs
  :: forall key m a
   . MemoryEncodable key
  => FsEncodable key
  => MonadLog m
  => MonadAff m
  => MemoryFsEnv
  -> key Reply a
  -> m a
handleGetMemoryFs { ref, cacheDir } key = Exists.runExists getImpl (encodeMemory key)
  where
  getImpl :: forall b. MemoryEncoding Reply a b -> m a
  getImpl (Key memory (Reply reply)) = do
    let (unCache :: CacheValue -> b) = unsafeCoerce
    getMemoryImpl ref (Key memory (Reply identity)) >>= case _ of
      Nothing -> do
        fsEntry <- encodeFs key # Exists.runExists case _ of
          AsJson fs codec _ -> do
            value <- getFsImpl cacheDir (AsJson fs codec (Reply identity))
            pure (map toCacheValue value)
          AsBuffer fs _ -> do
            buffer <- getFsImpl cacheDir (AsBuffer fs (Reply identity))
            pure (map toCacheValue buffer)
        case fsEntry of
          Nothing -> pure $ reply Nothing
          Just entry -> do
            putMemoryImpl ref (Key memory (Const entry))
            pure $ reply $ Just $ unCache entry
      Just cached ->
        pure $ reply $ Just cached

handlePutMemoryFs
  :: forall key m
   . MemoryEncodable key
  => FsEncodable key
  => MonadLog m
  => MonadAff m
  => MemoryFsEnv
  -> (forall void. key Const void)
  -> m Unit
handlePutMemoryFs { ref, cacheDir } key = do
  Exists.runExists (putMemoryImpl ref) (encodeMemory key)
  Exists.runExists (putFsImpl cacheDir) (encodeFs key)

handleDeleteMemoryFs
  :: forall key m
   . MemoryEncodable key
  => FsEncodable key
  => MonadLog m
  => MonadAff m
  => MemoryFsEnv
  -> (forall void. key Ignore void)
  -> m Unit
handleDeleteMemoryFs { ref, cacheDir } key = do
  Exists.runExists (deleteMemoryImpl ref) (encodeMemory key)
  Exists.runExists (deleteFsImpl cacheDir) (encodeFs key)

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

type MemoryEnv =
  { ref :: CacheRef
  }

handleGetMemory
  :: forall key m a
   . MemoryEncodable key
  => MonadLog m
  => MonadEffect m
  => MemoryEnv
  -> key Reply a
  -> m a
handleGetMemory env key = Exists.runExists (getMemoryImpl env.ref) (encodeMemory key)

getMemoryImpl :: forall m a b. MonadLog m => MonadEffect m => CacheRef -> MemoryEncoding Reply a b -> m a
getMemoryImpl ref (Key id (Reply reply)) = do
  let (unCache :: CacheValue -> b) = unsafeCoerce
  cache <- liftEffect $ Ref.read ref
  case Map.lookup id cache of
    Nothing -> do
      Log.debug $ "No cache entry found for " <> id <> " in memory."
      pure $ reply Nothing
    Just cached ->
      pure $ reply $ Just $ unCache cached

handlePutMemory
  :: forall key m
   . MemoryEncodable key
  => MonadLog m
  => MonadEffect m
  => MemoryEnv
  -> (forall void. key Const void)
  -> m Unit
handlePutMemory env key = Exists.runExists (putMemoryImpl env.ref) (encodeMemory key)

putMemoryImpl :: forall void a m. MonadLog m => MonadEffect m => CacheRef -> MemoryEncoding Const void a -> m Unit
putMemoryImpl ref (Key id (Const value)) = do
  liftEffect $ Ref.modify_ (Map.insert id (toCacheValue value)) ref
  Log.debug $ "Wrote cache entry for " <> id <> " in memory."

handleDeleteMemory
  :: forall key m
   . MemoryEncodable key
  => MonadLog m
  => MonadEffect m
  => MemoryEnv
  -> (forall void. key Ignore void)
  -> m Unit
handleDeleteMemory env key = Exists.runExists (deleteMemoryImpl env.ref) (encodeMemory key)

deleteMemoryImpl :: forall void1 void2 m. MonadLog m => MonadEffect m => CacheRef -> MemoryEncoding Ignore void1 void2 -> m Unit
deleteMemoryImpl ref (Key id Ignore) = do
  liftEffect $ Ref.modify_ (Map.delete id) ref
  Log.debug $ "Deleted cache entry for " <> id <> " in memory."

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

-- | The environment associated with handling MonadCache on the file system.
type FsCacheEnv =
  { cacheDir :: FilePath
  }

handleGetFs
  :: forall key m r
   . FsEncodable key
  => MonadLog m
  => MonadAff m
  => FsCacheEnv
  -> key Reply r
  -> m r
handleGetFs env key = Exists.runExists (getFsImpl env.cacheDir) (encodeFs key)

getFsImpl :: forall m a b. MonadLog m => MonadAff m => FilePath -> FsEncoding Reply a b -> m a
getFsImpl cacheDir = case _ of
  AsBuffer id (Reply reply) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
      Left _ -> do
        Log.debug $ "No cached buffer found for " <> id <> " at path " <> path
        pure $ reply Nothing
      Right buf ->
        pure $ reply $ Just buf

  AsJson id codec (Reply reply) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
      Left _ -> do
        Log.debug $ "No JSON cache file found for " <> id <> " at path " <> path
        pure $ reply Nothing
      Right content -> case Argonaut.Parser.jsonParser content of
        Left parseError ->
          liftAff (Aff.attempt (FS.Aff.unlink path)) >>= case _ of
            Left fsError -> do
              Log.error $ "Failed to delete JSON cache file " <> id <> " at path " <> path <> ": " <> Aff.message fsError
              pure (reply Nothing)
            Right _ -> do
              Log.error $ "Found JSON cache file for " <> id <> " at path " <> path <> " but its contents are not valid JSON: " <> parseError
              pure (reply Nothing)
        Right jsonContent -> case CA.decode codec jsonContent of
          Left decodeError -> do
            let error = CA.printJsonDecodeError decodeError
            Log.error $ "Found JSON cache file for " <> id <> " at path " <> path <> " but its contents could not be decoded with the provided codec: " <> error
            liftAff (Aff.attempt (FS.Aff.unlink path)) >>= case _ of
              Left fsError -> do
                Log.error $ "Failed to delete JSON cache file " <> id <> " at path " <> path <> ": " <> Aff.message fsError
                pure (reply Nothing)
              Right _ ->
                pure (reply Nothing)
          Right entry ->
            pure $ reply $ Just entry

handlePutFs
  :: forall key m
   . FsEncodable key
  => MonadLog m
  => MonadAff m
  => FsCacheEnv
  -> (forall void. key Const void)
  -> m Unit
handlePutFs env key = Exists.runExists (putFsImpl env.cacheDir) (encodeFs key)

putFsImpl :: forall m a b. MonadLog m => MonadAff m => FilePath -> FsEncoding Const a b -> m Unit
putFsImpl cacheDir = case _ of
  AsBuffer id (Const value) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    liftAff (Aff.attempt (FS.Aff.writeFile path value)) >>= case _ of
      Left fsError ->
        Log.error $ "Failed to write buffer for " <> id <> " to cache at path " <> path <> ": " <> Aff.message fsError
      Right _ ->
        pure unit

  AsJson id codec (Const value) -> do
    let path = Path.concat [ cacheDir, safePath id ]
    let encoded = Argonaut.stringify $ CA.encode codec value
    liftAff (Aff.attempt (FS.Aff.writeTextFile UTF8 path encoded)) >>= case _ of
      Left fsError ->
        Log.error $ "Failed to write JSON for " <> id <> " to cache at path " <> path <> ": " <> Aff.message fsError
      Right _ ->
        pure unit

handleDeleteFs
  :: forall key m
   . FsEncodable key
  => MonadLog m
  => MonadAff m
  => FsCacheEnv
  -> (forall void. key Ignore void)
  -> m Unit
handleDeleteFs env key = Exists.runExists (deleteFsImpl env.cacheDir) (encodeFs key)

deleteFsImpl :: forall m a b. MonadLog m => MonadAff m => FilePath -> FsEncoding Ignore a b -> m Unit
deleteFsImpl cacheDir = case _ of
  AsBuffer id Ignore -> do
    let path = Path.concat [ cacheDir, safePath id ]
    liftAff (Aff.attempt (FS.Aff.unlink path)) >>= case _ of
      Left fsError -> Log.error $ "Failed to delete cached buffer for " <> id <> " from path " <> path <> ": " <> Aff.message fsError
      Right _ -> pure unit
  AsJson id _ Ignore -> do
    let path = Path.concat [ cacheDir, safePath id ]
    liftAff (Aff.attempt (FS.Aff.unlink path)) >>= case _ of
      Left fsError -> Log.error $ "Failed to delete JSON cache file for " <> id <> " from path " <> path <> ": " <> Aff.message fsError
      Right _ -> pure unit

safePath :: String -> FilePath
safePath id =
  Maybe.maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
    $ JSURI.encodeURIComponent
    $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
    $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
    $ String.replaceAll (String.Pattern " ") (String.Replacement "__") id
