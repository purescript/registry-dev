module App.CLI.Git where

import Registry.App.Prelude

import Data.String as String
import Node.ChildProcess as ChildProcess
import Sunde as Sunde

-- | Run the `git` tool via the command line.
gitCLI :: Array String -> Maybe FilePath -> Aff (Either String String)
gitCLI args cwd = do
  result <- liftAff $ Sunde.spawn { cmd: "git", args, stdin: Nothing } (ChildProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  pure $ case result.exit of
    ChildProcess.Normally 0 -> Right stdout
    _ -> Left (stdout <> stderr)
