module Registry.App.Effect.Cache where

import Registry.App.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Minutes)
import Data.Time.Duration as Duration
import Effect.Aff (Fiber)
import Effect.Aff as Aff
import Effect.Ref as Ref
import JSURI as JSURI
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS
import Node.Path as Path
import Prim.Row as Row
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Internal.Codec as Internal.Codec
import Run (AFF, EFFECT, Run)
import Run as Run

data Cache k a
  = Get CacheId (Maybe (CacheEntry k) -> a)
  | Put CacheId k a
  | Delete CacheId a

derive instance Functor (Cache k)

newtype CacheId = CacheId String

derive newtype instance Eq CacheId
derive newtype instance Ord CacheId

-- | Get an item from the specified cache.
get :: forall s k t r. IsSymbol s => Row.Cons s (Cache k) t r => Proxy s -> CacheId -> Run r (Maybe (CacheEntry k))
get sym key = Run.lift sym (Get key identity)

-- | Put an item in the specified cache.
put :: forall s k t r. IsSymbol s => Row.Cons s (Cache k) t r => Proxy s -> CacheId -> k -> Run r Unit
put sym key val = Run.lift sym (Put key val unit)

-- | Delete an item from the specified cache.
delete :: forall s k t r. IsSymbol s => Row.Cons s (Cache k) t r => Proxy s -> CacheId -> Run r Unit
delete sym key = Run.lift sym (Delete key unit)

getJson
  :: forall s t r a
   . IsSymbol s
  => Row.Cons s (Cache Json) t (LOG + r)
  => Proxy s
  -> CacheId
  -> JsonCodec a
  -> Run (LOG + r) (Maybe (CacheEntry a))
getJson sym key@(CacheId id) codec =
  get sym key >>= case _ of
    Nothing -> pure Nothing
    Just entry -> case CA.decode codec entry.value of
      Left decodeError -> do
        Log.error $ "Failed to decode cached value for " <> id <> ": " <> CA.printJsonDecodeError decodeError
        delete sym key
        pure Nothing
      Right decoded -> do
        pure $ Just $ entry { value = decoded }

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

-- | Handle the Cache effect using an in-memory cache that falls back to the
-- | file system on cache misses.
handleCacheMemoryFs
  :: forall k r a
   . MemoryCacheEnv k
  -> FsCacheEnv k
  -> Cache k a
  -> Run (LOG + AFF + EFFECT + r) a
handleCacheMemoryFs memoryCacheEnv fsCacheEnv = case _ of
  Get cacheId reply -> do
    memory <- handleCacheMemory memoryCacheEnv (Get cacheId identity)
    case memory of
      Nothing -> do
        fs <- handleCacheFs fsCacheEnv (Get cacheId identity)
        case fs of
          Nothing -> pure $ reply Nothing
          Just entry -> do
            handleCacheMemory memoryCacheEnv (Put cacheId entry.value unit)
            pure $ reply $ Just entry
      Just cached ->
        pure $ reply $ Just cached

  Put cacheId value next -> do
    handleCacheMemory memoryCacheEnv (Put cacheId value unit)
    handleCacheFs fsCacheEnv (Put cacheId value unit)
    pure next

  Delete cacheId next -> do
    handleCacheMemory memoryCacheEnv (Delete cacheId unit)
    handleCacheFs fsCacheEnv (Delete cacheId unit)
    pure next

type CacheRef k = Ref (Map CacheId { entry :: CacheEntry k, kill :: Maybe (Fiber Unit) })

newCacheRef :: forall k m. MonadEffect m => m (CacheRef k)
newCacheRef = liftEffect (Ref.new Map.empty)

-- | An environment for the in-memory cache handler for the Cache effect.
type MemoryCacheEnv k =
  { cacheRef :: CacheRef k
  , expiry :: Expiry
  }

-- | Controls how values are expired from the in-memory cache.
data Expiry
  = Never
  | After Minutes
  | ById (CacheId -> Maybe Minutes)

-- | A handler for the Cache effect which stores cached values in memory. You can
-- | optionally specify an expiration time after which the data will be purged.
handleCacheMemory :: forall k r a. MemoryCacheEnv k -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheMemory { cacheRef, expiry } = case _ of
  Get cacheId@(CacheId id) reply -> do
    Log.debug $ "Reading cache entry for " <> id <> " from memory."
    cache <- Run.liftEffect $ Ref.read cacheRef
    case Map.lookup cacheId cache of
      Nothing -> do
        Log.debug $ "No cache entry found for " <> id
        pure $ reply Nothing
      Just { entry, kill } -> do
        let
          -- We've accessed the cached value, so first we terminate the timer to
          -- prevent expiration. Then we create a new expiry timer, which will
          -- delete the cache value after the given duration, unless it is killed.
          resetExpiration duration = do
            for_ kill \fiber -> Run.liftAff do
              Aff.killFiber (Aff.error "Reset timer") fiber
            fiber <- Run.liftAff $ Aff.forkAff do
              Aff.delay (Duration.fromDuration duration)
              liftEffect (Ref.modify_ (Map.delete cacheId) cacheRef)
            let update old = old { kill = Just fiber }
            Run.liftEffect $ Ref.modify_ (Map.update (Just <<< update) cacheId) cacheRef

        case expiry of
          Never -> pure unit
          After duration -> resetExpiration duration
          ById k -> for_ (k cacheId) resetExpiration

        pure $ reply $ Just entry

  Put cacheId@(CacheId id) value next -> do
    Log.debug $ "Writing cache entry for " <> id <> " to memory."
    modified <- Run.liftEffect nowUTC
    let
      entry = { modified, value }
      insertWithExpiration duration = do
        cache <- Run.liftEffect $ Ref.read cacheRef
        for_ (Map.lookup cacheId cache) \{ kill } ->
          for_ kill \fiber -> Run.liftAff do
            Aff.killFiber (Aff.error "Reset timer") fiber
        fiber <- Run.liftAff $ Aff.forkAff do
          Aff.delay (Duration.fromDuration duration)
          liftEffect (Ref.modify_ (Map.delete cacheId) cacheRef)
        Run.liftEffect $ Ref.modify_ (Map.insert cacheId { entry, kill: Just fiber }) cacheRef

    case expiry of
      Never ->
        Run.liftEffect $ Ref.modify_ (Map.insert cacheId { entry, kill: Nothing }) cacheRef
      After duration ->
        insertWithExpiration duration
      ById k -> case (k cacheId) of
        Nothing ->
          Run.liftEffect $ Ref.modify_ (Map.insert cacheId { entry, kill: Nothing }) cacheRef
        Just duration ->
          insertWithExpiration duration

    pure next

  Delete cacheId@(CacheId id) next -> do
    Log.debug $ "Removing cache entry for " <> id <> " from memory."
    Run.liftEffect $ Ref.modify_ (Map.delete cacheId) cacheRef
    pure next

-- | The environment for a filesystem-backed cache implementation.
type FsCacheEnv k =
  { encoding :: FsCacheEncoding k
  , cacheDir :: FilePath
  }

-- | How to encode values of type `k` on the filesystem. Most well-typed data
-- | should use JSON.
data FsCacheEncoding k
  = Buffer { encode :: k -> Buffer, decode :: Buffer -> k }
  | String { encode :: k -> String, decode :: String -> Either String k }
  | Json (JsonCodec k)

-- | A default encoding for caching `Buffer` values.
buffer :: FsCacheEncoding Buffer
buffer = Buffer { encode: identity, decode: identity }

-- | A default encoding for caching `String` values.
string :: FsCacheEncoding String
string = String { encode: identity, decode: pure }

json :: FsCacheEncoding Json
json = Json CA.json

-- | Handle the Cache effect by caching values on the file system, given a
-- | file encoding to use.
handleCacheFs :: forall k r a. FsCacheEnv k -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheFs env = case _ of
  Get cacheId@(CacheId id) reply -> do
    let path = safePath cacheId
    Log.debug $ "Reading cache entry for " <> id <> " at path " <> path
    let removeEntry = handleCacheFs env (Delete cacheId unit)
    case env.encoding of
      Buffer { decode } -> do
        Run.liftAff (Aff.attempt (FS.Aff.stat path)) >>= case _ of
          Left _ -> do
            Log.debug $ "No cache entry found for " <> id <> " at path " <> path
            pure $ reply Nothing
          -- TODO: Is mtime the right one? Is there a way to see the last time
          -- the file was accessed vs. the last time it was modified? So I can
          -- implement expiry here too?
          Right (FS.Stats { mtime }) -> case JSDate.toDateTime mtime of
            Nothing -> do
              Log.error $ "Found cache file for " <> id <> " at path " <> path <> " but could not read modification time as a DateTime: " <> show mtime
              removeEntry
              pure $ reply Nothing
            Just modified -> Run.liftAff (Aff.attempt (FS.Aff.readFile path)) >>= case _ of
              Left fsError -> do
                Log.error $ "Found cache file for " <> id <> " at path " <> path <> " but could not read it: " <> Aff.message fsError
                removeEntry
                pure $ reply Nothing
              Right buf ->
                pure $ reply $ Just { modified, value: decode buf }

      String { decode } -> do
        Run.liftAff (Aff.attempt (readJsonFile (cacheEntryCodec CA.string) path)) >>= case _ of
          Left fsError -> do
            Log.debug $ "No cache file at path " <> path <> ": " <> Aff.message fsError
            pure $ reply Nothing
          Right (Left jsonError) -> do
            Log.error $ "Failed to decode CacheEntry for value at key " <> id <> ": " <> jsonError
            removeEntry
            pure $ reply Nothing
          Right (Right entry) -> case decode entry.value of
            Left error -> do
              Log.error $ "Failed to decode cached value at key " <> id <> ": " <> error
              removeEntry
              pure $ reply Nothing
            Right value ->
              pure $ reply $ Just { modified: entry.modified, value }

      Json codec -> do
        Run.liftAff (Aff.attempt (readJsonFile (cacheEntryCodec CA.json) path)) >>= case _ of
          Left _ -> do
            Log.debug $ "No cache entry for " <> id <> " at path " <> path
            pure $ reply Nothing
          Right (Left jsonError) -> do
            Log.error $ "Failed to parse entry file for " <> id <> ": " <> jsonError
            removeEntry
            pure $ reply Nothing
          Right (Right entry) -> case CA.decode codec entry.value of
            Left decodeError -> do
              let error = CA.printJsonDecodeError decodeError
              Log.error $ "Failed to decode value at key " <> id <> " using its provided codec: " <> error
              removeEntry
              pure $ reply Nothing
            Right value ->
              pure $ reply $ Just { modified: entry.modified, value }

  Put cacheId@(CacheId id) value next -> do
    let path = safePath cacheId
    Log.debug $ "Writing cache entry for " <> id <> " at path " <> path
    case env.encoding of
      Buffer { encode } ->
        Run.liftAff (Aff.attempt (FS.Aff.writeFile path (encode value))) >>= case _ of
          Left fsError -> do
            Log.warn $ "Failed to write cache entry for " <> id <> " at path " <> path <> " as a buffer: " <> Aff.message fsError
            pure next
          Right _ -> pure next

      String { encode } -> do
        modified <- Run.liftEffect nowUTC
        let entry = { modified, value: encode value }
        Run.liftAff (Aff.attempt (writeJsonFile (cacheEntryCodec CA.string) path entry)) >>= case _ of
          Left fsError -> do
            Log.warn $ "Failed to write cache entry for " <> id <> " at path " <> path <> " as a string: " <> Aff.message fsError
            pure next
          Right _ -> pure next

      Json codec -> do
        modified <- Run.liftEffect nowUTC
        let entry = { modified, value }
        Run.liftAff (Aff.attempt (writeJsonFile (cacheEntryCodec codec) path entry)) >>= case _ of
          Left fsError -> do
            Log.warn $ "Failed to write cache entry for " <> id <> " at path " <> path <> " as JSON: " <> Aff.message fsError
            pure next
          Right _ -> pure next

  Delete cacheId@(CacheId id) next -> do
    let path = safePath cacheId
    Log.debug $ "Deleting cache entry for " <> id <> " at path " <> path
    Run.liftAff (Aff.attempt (FS.Aff.unlink path)) >>= case _ of
      Left fsError -> do
        Log.warn $ "Failed to delete cache entry for " <> id <> " at path " <> path <> ": " <> Aff.message fsError
        pure next
      Right _ ->
        pure next

  where
  safePath :: CacheId -> FilePath
  safePath (CacheId id) = do
    let
      encoded =
        Maybe.maybe' (\_ -> unsafeCrashWith ("Unable to encode " <> id <> " as a safe file path.")) identity
          $ JSURI.encodeURIComponent
          $ String.replaceAll (String.Pattern "@") (String.Replacement "$")
          $ String.replaceAll (String.Pattern "/") (String.Replacement "_")
          $ String.replaceAll (String.Pattern " ") (String.Replacement "__") id

    Path.concat [ env.cacheDir, encoded ]
