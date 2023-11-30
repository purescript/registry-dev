module Registry.Foreign.Gzip
  ( Gzip(..)
  , compress
  ) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Fetch (class ToRequestBody)
import JS.Fetch.RequestBody (RequestBody)
import Node.Buffer (Buffer)
import Unsafe.Coerce (unsafeCoerce)

newtype Gzip = Gzip Buffer

-- A `Buffer` can be passed as a `RequestBody`, but this is not yet reflected in
-- the types of the upstream `fetch` library. See:
-- https://github.com/rowtype-yoga/purescript-fetch/issues/11
gzipToRequestBody :: Gzip -> RequestBody
gzipToRequestBody = unsafeCoerce

instance ToRequestBody Gzip where
  toRequestBody = gzipToRequestBody

foreign import compressImpl :: EffectFn3 String (EffectFn1 Error Unit) (EffectFn1 Gzip Unit) Unit

-- | Compress a string using gzip.
compress :: forall m. MonadAff m => String -> m Gzip
compress input = liftAff do
  makeAff \handle -> do
    runEffectFn3 compressImpl input
      (mkEffectFn1 \error -> handle (Left error))
      (mkEffectFn1 \buffer -> handle (Right buffer))
    pure mempty
