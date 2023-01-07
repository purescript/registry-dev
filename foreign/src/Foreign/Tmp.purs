module Registry.Foreign.Tmp (mkTmpDir) where

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import mkTmpDirImpl :: Effect String

mkTmpDir :: forall m. MonadEffect m => m String
mkTmpDir = liftEffect mkTmpDirImpl
