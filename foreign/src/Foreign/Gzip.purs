module Registry.Foreign.Gzip
  ( compress
  ) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Node.Buffer (Buffer)

foreign import compressImpl :: EffectFn3 String (EffectFn1 Error Unit) (EffectFn1 Buffer Unit) Unit

-- | Compress a string using gzip.
compress :: forall m. MonadAff m => String -> m Buffer
compress input = liftAff do
  makeAff \handle -> do
    runEffectFn3 compressImpl input
      (mkEffectFn1 \error -> handle (Left error))
      (mkEffectFn1 \buffer -> handle (Right buffer))
    pure mempty
