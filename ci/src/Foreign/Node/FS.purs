module Foreign.Node.FS
  ( ensureDirectory
  , remove
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Path (FilePath)

foreign import ensureDirectoryImpl :: String -> Effect (Promise Unit)

-- | Ensures that the directory exists. If the directory structure does not
-- | exist, it is created.
ensureDirectory :: FilePath -> Aff Unit
ensureDirectory = ensureDirectoryImpl >>> Promise.toAffE

foreign import removeImpl :: String -> Effect (Promise Unit)

-- | Removes a file or directory. The directory can have contents. If the
-- | path does not exist, silently does nothing.
remove :: FilePath -> Aff Unit
remove = removeImpl >>> Promise.toAffE
