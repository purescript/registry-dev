module Foreign.Tmp (mkTmpDir) where

import Data.Function.Uncurried (Fn0, runFn0)
import Effect (Effect)

foreign import mkTmpDirImpl :: Fn0 (Effect String)
mkTmpDir :: Effect String
mkTmpDir = runFn0 mkTmpDirImpl
