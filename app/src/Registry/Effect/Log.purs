module Registry.Effect.Log where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Formatter.DateTime as Formatter.DateTime
import Effect.Class.Console as Console
import Foreign.GitHub (IssueNumber, Octokit)
import Foreign.GitHub as GitHub
import Node.FS.Aff as FS.Aff
import Node.Process as Process
import Registry.Internal.Format as Internal.Format

data LogLevel = Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

class MonadLog m where
  log :: LogLevel -> String -> m Unit

debug :: forall m. MonadLog m => String -> m Unit
debug = log Debug

info :: forall m. MonadLog m => String -> m Unit
info = log Info

warn :: forall m. MonadLog m => String -> m Unit
warn = log Warn

error :: forall m. MonadLog m => String -> m Unit
error = log Error

die :: forall m a. MonadLog m => MonadAff m => String -> m a
die message = error message *> liftEffect (Process.exit 1)

runLogConsole :: forall m. MonadEffect m => LogLevel -> String -> m Unit
runLogConsole level = case level of
  Debug -> Console.debug
  Info -> Console.info
  Warn -> Console.warn
  Error -> Console.error

runLogGitHub :: forall m. MonadAff m => Octokit -> IssueNumber -> LogLevel -> String -> m Unit
runLogGitHub octokit issue level message = do
  runLogConsole level message
  unless (level == Debug) do
    liftAff (Except.runExceptT (GitHub.createComment octokit issue message)) >>= case _ of
      Left err -> do
        Console.error "UNEXPECTED ERROR: Could not send comment to GitHub."
        Console.error $ GitHub.printGitHubError err
        liftEffect $ Process.exit 1
      Right _ -> pure unit

-- | Write logs to the console and to the given logfile. Suitable for use in
-- | local runs of the registry tools.
runLogFile :: forall m. MonadAff m => FilePath -> LogLevel -> String -> m Unit
runLogFile logfile level message = do
  runLogConsole level message
  now <- liftEffect nowUTC
  let formatted = Array.fold [ "[", Formatter.DateTime.format Internal.Format.iso8601DateTime now, "] ", message, "\n" ]
  liftAff $ FS.Aff.appendTextFile UTF8 logfile formatted
