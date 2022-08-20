module Foreign.Git where

import Registry.Prelude

import Data.String as String
import Node.ChildProcess as NodeProcess
import Sunde as Process

runGit_ :: Array String -> Maybe FilePath -> ExceptT String Aff Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGit args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  case result.exit of
    NodeProcess.Normally 0 -> do
      unless (String.null stdout) (info stdout)
      pure $ Right stdout
    _ -> do
      unless (String.null stderr) (error stderr)
      pure $ Left stderr

runGitSilent :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGitSilent args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  case result.exit of
    NodeProcess.Normally 0 -> do
      let stdout = String.trim result.stdout
      pure $ Right stdout
    _ -> pure $ Left $ "Failed to run git command via runGitSilent."
