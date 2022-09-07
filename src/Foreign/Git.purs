module Foreign.Git where

import Registry.Prelude

import Affjax as Http
import Control.Monad.Except as Except
import Data.JSDate as JSDate
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String)
import Data.String as String
import Effect.Aff as Aff
import Effect.Exception as Exception
import Foreign.GitHub as GitHub
import Node.ChildProcess as NodeProcess
import Node.Process as Env
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

-- | Clone a package from a Git location to the provided directory.
cloneGitTag :: Http.URL -> String -> FilePath -> Aff Unit
cloneGitTag url ref targetDir = do
  let args = [ "clone", url, "--branch", ref, "--single-branch", "-c", "advice.detachedHead=false" ]
  withBackoff' (Except.runExceptT (runGit args (Just targetDir))) >>= case _ of
    Nothing -> Aff.throwError $ Aff.error $ "Timed out attempting to clone git tag: " <> url <> " " <> ref
    Just (Left err) -> Aff.throwError $ Aff.error err
    Just (Right _) -> log "Successfully cloned package."

-- | Read the published time of the checked-out commit.
gitGetRefTime :: String -> FilePath -> ExceptT String Aff RFC3339String
gitGetRefTime ref repoDir = do
  timestamp <- runGit [ "log", "-1", "--date=iso8601-strict", "--format=%cd", ref ] (Just repoDir)
  jsDate <- liftEffect $ JSDate.parse timestamp
  dateTime <- Except.except $ note "Failed to convert JSDate to DateTime" $ JSDate.toDateTime jsDate
  pure $ PDT.toRFC3339String $ PDT.fromDateTime dateTime

configurePacchettiBotti :: Maybe FilePath -> ExceptT String Aff GitHub.GitHubToken
configurePacchettiBotti cwd = do
  pacchettiBotti <- liftEffect do
    Env.lookupEnv "PACCHETTIBOTTI_TOKEN"
      >>= maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment") pure
  runGit_ [ "config", "user.name", "PacchettiBotti" ] cwd
  runGit_ [ "config", "user.email", "<" <> pacchettiBottiEmail <> ">" ] cwd
  pure (GitHub.GitHubToken pacchettiBotti)

pacchettiBottiEmail :: String
pacchettiBottiEmail = "pacchettibotti@purescript.org"
