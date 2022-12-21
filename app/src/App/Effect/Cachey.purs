module Registry.App.Effect.Cachey where

import Registry.App.Prelude

import Data.Argonaut.Core as Argonaut.Core
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Symbol (class IsSymbol)
import Node.Buffer as Buffer
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.FS.Aff as FS.Aff
import Prim.Row as Row
import Registry.App.Effect.Cache (AppCache(..))
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Manifest as Manifest
import Run (AFF, EFFECT, Run)
import Run as Run
import Web.File.Blob (Blob)

data Cache k a
  = Get CacheId (Maybe k -> a)
  | Put CacheId k a
  | Delete CacheId a

derive instance Functor (Cache k)

newtype CacheId = CacheId String

get :: forall s k t r. IsSymbol s => Row.Cons s (Cache k) t r => Proxy s -> CacheId -> Run r (Maybe k)
get sym key = Run.lift sym (Get key identity)

put :: forall s k t r. IsSymbol s => Row.Cons s (Cache k) t r => Proxy s -> CacheId -> k -> Run r Unit
put sym key val = Run.lift sym (Put key val unit)

delete :: forall s k t r. IsSymbol s => Row.Cons s (Cache k) t r => Proxy s -> CacheId -> Run r Unit
delete sym key = Run.lift sym (Delete key unit)

--
-- IN-MEMORY CACHE
--

--
-- FILE SYSTEM CACHE
--

data KeyEncoding k
  = Buffer { encode :: k -> Buffer, decode :: Buffer -> k }
  | String { encode :: k -> String, decode :: String -> Either String k }
  | Json (JsonCodec k)

buffer :: KeyEncoding Buffer
buffer = Buffer { encode: identity, decode: identity }

string :: KeyEncoding String
string = String { encode: identity, decode: pure }

handleCacheFs :: forall k a r. KeyEncoding k -> Cache k a -> Run (LOG + AFF + EFFECT + r) a
handleCacheFs encoding = case _ of
  Get (CacheId id) reply -> do
    Log.debug $ "Reading cache entry for " <> id
    case encoding of
      Buffer { decode } -> do
        buf <- Run.liftAff (FS.Aff.readFile id)
        pure $ reply $ Just $ decode buf

      String { decode } -> do
        str <- Run.liftAff (FS.Aff.readTextFile UTF8 id)
        case decode str of
          Left error -> do
            Log.error $ "Failed to decode cached value at key " <> id <> ": " <> error
            pure $ reply Nothing
          Right value ->
            pure $ reply $ Just value

      Json codec -> do
        str <- Run.liftAff (FS.Aff.readTextFile UTF8 id)
        case Argonaut.Parser.jsonParser str of
          Left error -> do
            Log.error $ "Failed to parse value at key " <> id <> " as JSON: " <> error
            Log.debug $ "Expiring cache entry for key " <> id <> " because it cannot be parsed."
            handleCacheFs encoding (Delete (CacheId id) unit)
            pure $ reply Nothing
          Right json -> case CA.decode codec json of
            Left jsonError -> do
              let error = CA.printJsonDecodeError jsonError
              Log.error $ "Failed to parse value at key " <> id <> " using the provided codec: " <> error
              Log.debug $ "Expiring cache entry for key " <> id <> " because it cannot be decoded."
              handleCacheFs encoding (Delete (CacheId id) unit)
              pure $ reply Nothing
            Right value ->
              pure $ reply $ Just value

  Put _ _ next -> do
    pure next

  Delete _ next -> do
    pure next
