module Registry.App.CLI.Git where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.String as String
import Effect.Aff as Aff
import Node.ChildProcess as NodeProcess
import Registry.Constants as Constants
import Registry.Foreign.Octokit (GitHubToken(..))
import Sunde as Process

runGit_ :: Array String -> Maybe FilePath -> ExceptT String Aff Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGit args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right stdout
    _ -> Left stderr

runGitSilent :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGitSilent args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  case result.exit of
    NodeProcess.Normally 0 -> do
      let stdout = String.trim result.stdout
      pure $ Right stdout
    _ -> pure $ Left $ "Failed to run git command via runGitSilent."

-- | Clone a package from a Git location to the provided directory.
cloneGitTag :: String -> String -> FilePath -> Aff Unit
cloneGitTag url ref targetDir = do
  let args = [ "clone", url, "--branch", ref, "--single-branch", "-c", "advice.detachedHead=false" ]
  withBackoff' (Except.runExceptT (runGit args (Just targetDir))) >>= case _ of
    Nothing -> Aff.throwError $ Aff.error $ "Timed out attempting to clone git tag: " <> url <> " " <> ref
    Just (Left err) -> Aff.throwError $ Aff.error err
    Just (Right _) -> pure unit

-- | Read the published time of the checked-out commit.
gitGetRefTime :: String -> FilePath -> ExceptT String Aff DateTime
gitGetRefTime ref repoDir = do
  timestamp <- runGit [ "log", "-1", "--date=iso8601-strict", "--format=%cd", ref ] (Just repoDir)
  jsDate <- liftEffect $ JSDate.parse timestamp
  dateTime <- Except.except $ note "Failed to convert JSDate to DateTime" $ JSDate.toDateTime jsDate
  pure dateTime

data CommitMode = CommitOnly | CommitAndPush

derive instance Eq CommitMode

type PacchettiBottiCommit =
  { token :: GitHubToken
  , mode :: CommitMode
  , paths :: String.Pattern
  , message :: String
  }

pacchettiBottiCommitRegistry :: FilePath -> PacchettiBottiCommit -> ExceptT String Aff Unit
pacchettiBottiCommitRegistry registry { token, mode, paths: String.Pattern paths, message } = do
  let upstream = Constants.registry.owner <> "/" <> Constants.registry.repo
  let origin = "https://pacchettibotti:" <> un GitHubToken token <> "@github.com/" <> upstream <> ".git"
  runGit_ [ "config", "user.name", "PacchettiBotti" ] (Just registry)
  runGit_ [ "config", "user.email", "<" <> pacchettiBottiEmail <> ">" ] (Just registry)
  runGit_ [ "add", paths ] (Just registry)
  runGit_ [ "commit", "-m", message ] (Just registry)
  when (mode == CommitAndPush) do
    runGit_ [ "push", origin, "main" ] (Just registry)

pacchettiBottiCommitRegistryIndex :: FilePath -> PacchettiBottiCommit -> ExceptT String Aff Unit
pacchettiBottiCommitRegistryIndex registryIndex { token, mode, paths: String.Pattern paths, message } = do
  let upstream = Constants.manifestIndex.owner <> "/" <> Constants.manifestIndex.repo
  let origin = "https://pacchettibotti:" <> un GitHubToken token <> "@github.com/" <> upstream <> ".git"
  runGit_ [ "config", "user.name", "PacchettiBotti" ] (Just registryIndex)
  runGit_ [ "config", "user.email", "<" <> pacchettiBottiEmail <> ">" ] (Just registryIndex)
  runGit_ [ "add", paths ] (Just registryIndex)
  runGit_ [ "commit", "-m", message ] (Just registryIndex)
  when (mode == CommitAndPush) do
    runGit_ [ "push", origin, "master" ] (Just registryIndex)
