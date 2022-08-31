module Foreign.Tar (getToplevelDir, create, extract) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Node.ChildProcess as ChildProcess

foreign import getToplevelDirImpl :: Fn1 String (Effect (Array String))

getToplevelDir :: String -> Effect (Maybe String)
getToplevelDir filename = do
  paths <- runFn1 getToplevelDirImpl filename
  pure $ Array.head paths

type ExtractArgs = { cwd :: String, archive :: String }

-- | Extracts the tarball at the given relative file path into cwd.
extract :: ExtractArgs -> Effect Unit
extract { cwd, archive } = do
  let cmd = "tar -xzf " <> archive
  void $ ChildProcess.execSync cmd (ChildProcess.defaultExecSyncOptions { cwd = Just cwd })

type CreateArgs = { cwd :: String, folderName :: String }

create :: CreateArgs -> Effect Unit
create { cwd, folderName } = do
  let
    cmd = String.joinWith " | " [ tarCmd, gzipCmd ]
    gzipCmd = "gzip " <> String.joinWith " " [ "--name", ">", folderName <> ".tar.gz" ]
    tarCmd = "tar " <> String.joinWith " "
      [ "--sort=name"
      , "--mtime=1970-01-01 00:00:Z"
      , "--owner=0"
      , "--group=0"
      , "--numeric-owner"
      , "--pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime"
      , "-cf"
      , "-"
      , folderName
      ]
  void $ ChildProcess.execSync cmd (ChildProcess.defaultExecSyncOptions { cwd = Just cwd })
