-- | An effect for notifying users of important events in the application, such
-- | as failures that prevent their package from being uploaded, or successful
-- | events that indicate progress.
-- |
-- | This is not a general logging effect. For that, you should use the Log
-- | effect. This effect should be used sparingly to notify registry users of
-- | events with formatted, human-readable messages providing context.
module Registry.App.Monad.Notify where

import Registry.App.Prelude

import Ansi.Codes (GraphicsParam)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Int as Int
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Registry.App.Monad.Log (class Loggable, class MonadLog)
import Registry.App.Monad.Log as Log
import Registry.Foreign.Octokit (Address, IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit

class Monad m <= MonadNotify m where
  send :: Doc GraphicsParam -> m Unit

instance MonadNotify m => MonadNotify (ExceptT e m) where
  send = lift <<< send

notify :: forall m a. Loggable a => MonadNotify m => a -> m Unit
notify = send <<< Log.toLog

-- | Handle a notification by logging it to the console.
handleNotifyLog :: forall m. MonadLog m => Doc GraphicsParam -> m Unit
handleNotifyLog message = Log.info $ Ansi.foreground Ansi.BrightBlue (Dodo.text "[NOTIFY] ") <> message

type NotifyGitHubEnv =
  { octokit :: Octokit
  , issue :: IssueNumber
  , registry :: Address
  }

type NOTIFY_GITHUB r = (notify :: NotifyGitHubEnv | r)

-- | Handle a notification by commenting on the relevant GitHub issue.
handleNotifyGitHub
  :: forall m r
   . MonadAsk { | NOTIFY_GITHUB + r } m
  => MonadLog m
  => MonadAff m
  => Doc GraphicsParam
  -> m Unit
handleNotifyGitHub message = do
  env <- asks _.notify
  let issueNumber = Int.toStringAs Int.decimal $ un IssueNumber env.issue
  Log.debug $ "Notifying via a GitHub comment on issue " <> issueNumber
  handleNotifyLog message
  let comment = Dodo.print Dodo.plainText Dodo.twoSpaces (Log.toLog message)
  let request = Octokit.createCommentRequest { address: env.registry, issue: env.issue, body: comment }
  Octokit.request env.octokit request >>= case _ of
    Left error -> do
      Log.error $ "Could not send comment to GitHub due to an unexpected error."
      Log.debug $ Octokit.printGitHubError error
    Right _ ->
      Log.debug $ "Created GitHub comment on issue " <> issueNumber
