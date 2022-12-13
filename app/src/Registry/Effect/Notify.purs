module Registry.Effect.Notify where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.Int as Int
import Dodo as Dodo
import Foreign.GitHub (IssueNumber(..), Octokit)
import Foreign.GitHub as GitHub
import Node.Process as Process
import Registry.Effect.Log (class Loggable, class MonadLog)
import Registry.Effect.Log as Log

class MonadNotify m where
  notify :: forall a. Loggable a => a -> m Unit
  exit :: forall a void. Loggable a => a -> m void

type GitHubEnv =
  { octokit :: Octokit
  , issue :: IssueNumber
  }

runNotifyGitHub :: forall m a. Loggable a => MonadLog m => MonadAff m => GitHubEnv -> a -> m Unit
runNotifyGitHub env message = do
  let issue = Int.toStringAs Int.decimal $ un IssueNumber env.issue
  Log.debug $ "Creating GitHub comment on issue " <> issue
  Log.info message
  let comment = Dodo.print Dodo.plainText Dodo.twoSpaces (Log.toLog message)
  liftAff (Except.runExceptT (GitHub.createComment env.octokit env.issue comment)) >>= case _ of
    Left error -> do
      Log.error $ "Could not send comment to GitHub due to an unexpected error."
      Log.debug $ GitHub.printGitHubError error
      liftEffect $ Process.exit 1
    Right _ ->
      Log.debug $ "Created GitHub comment on issue " <> issue

runExitGitHub :: forall m a void. Loggable a => MonadLog m => MonadAff m => GitHubEnv -> a -> m void
runExitGitHub env message = do
  runNotifyGitHub env message
  liftEffect $ Process.exit 1
