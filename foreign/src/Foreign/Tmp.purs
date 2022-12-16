module Registry.Foreign.Tmp (mkTmpDir) where

import Effect (Effect)

foreign import mkTmpDirImpl :: Effect String

mkTmpDir :: Effect String
mkTmpDir = mkTmpDirImpl
