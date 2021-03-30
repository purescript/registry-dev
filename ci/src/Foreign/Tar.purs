module Foreign.Tar (getToplevelDir, create, extract) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe)
import Effect (Effect)

foreign import getToplevelDirImpl :: Fn1 String (Effect (Array String))
getToplevelDir :: String -> Effect (Maybe String)
getToplevelDir filename = do
  paths <- runFn1 getToplevelDirImpl filename
  pure $ Array.head paths

foreign import extractImpl :: Fn2 String String (Effect Unit)
extract :: ExtractArgs -> Effect Unit
extract { cwd, filename } = runFn2 extractImpl cwd filename

type ExtractArgs = { cwd :: String, filename :: String }

foreign import createImpl :: Fn3 String String String (Effect Unit)
create :: CreateArgs -> Effect Unit
create { cwd, archiveName, folderName } = runFn3 createImpl cwd folderName archiveName

type CreateArgs = { cwd :: String, archiveName :: String, folderName :: String }