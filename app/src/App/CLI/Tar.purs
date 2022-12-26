module Registry.App.CLI.Tar
  ( create
  , extract
  ) where

import Registry.App.Prelude

import Data.String as String
import Node.ChildProcess as ChildProcess

type ExtractArgs = { cwd :: String, archive :: FilePath }

-- | Extracts the tarball at the given relative file path into cwd. Resulting
-- | directory name will be the archive name minus '.tar.gz'.
extract :: ExtractArgs -> Effect Unit
extract { cwd, archive } = do
  let cmd = "tar -xvzf " <> archive
  void $ ChildProcess.execSync cmd (ChildProcess.defaultExecSyncOptions { cwd = Just cwd })

type CreateArgs = { cwd :: String, folderName :: String }

-- | Create a tarball in cwd given a relative path to a folder in that
-- | directory, writing logs to stdout.
create :: CreateArgs -> Effect Unit
create { cwd, folderName } = do
  let
    cmd = String.joinWith " | " [ tarCmd, gzipCmd ]
    gzipCmd = "gzip " <> String.joinWith " " [ "--name", ">", folderName <> ".tar.gz" ]
    -- All these flags are here to ensure that the tarball creation is deterministic/reproducible.
    -- They come from https://reproducible-builds.org/docs/archives/
    tarCmd = "tar " <> String.joinWith " "
      [ "--sort=name"
      , "--mtime=1970-01-01 00:00:Z"
      , "--owner=0"
      , "--group=0"
      , "--numeric-owner"
      , "--pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime"
      , "-vcf"
      , "-"
      , folderName
      ]
  void $ ChildProcess.execSync cmd (ChildProcess.defaultExecSyncOptions { cwd = Just cwd })
