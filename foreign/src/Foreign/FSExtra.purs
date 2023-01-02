module Registry.Foreign.FSExtra
  ( ensureDirectory
  , remove
  , copy
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.Path (FilePath)

foreign import ensureDirectoryImpl :: String -> Effect (Promise Unit)

-- | Ensures that the directory exists. If the directory structure does not
-- | exist, it is created.
ensureDirectory :: forall m. MonadAff m => FilePath -> m Unit
ensureDirectory = ensureDirectoryImpl >>> Promise.toAffE >>> liftAff

foreign import removeImpl :: String -> Effect (Promise Unit)

-- | Removes a file or directory. The directory can have contents. If the
-- | path does not exist, silently does nothing.
remove :: forall m. MonadAff m => FilePath -> m Unit
remove = removeImpl >>> Promise.toAffE >>> liftAff

type JSCopyOptions =
  { preserveTimestamps :: Boolean
  }

foreign import copyImpl :: String -> String -> JSCopyOptions -> Effect (Promise Unit)

type CopyArgs =
  { from :: FilePath
  , to :: FilePath
  , preserveTimestamps :: Boolean
  }

-- | Copies a file or directory. If a file or directory already exists at the
-- | destination, that file or directory will be overwritten.
-- |
-- | When `from` is a file then `to` must be a file.
-- |
-- | When `from` is a directory then `to` must be a directory; only directory
-- | contents are copied, not the directory itself.
copy :: forall m. MonadAff m => CopyArgs -> m Unit
copy { from, to, preserveTimestamps } = liftAff $ Promise.toAffE $ copyImpl from to { preserveTimestamps }
