-- | An effect for notifying users of important events in the application, such
-- | as failures that prevent their package from being uploaded, or successful
-- | events that indicate progress.
-- |
-- | This is not a general logging effect. For that, you should use the Log
-- | effect. This effect should be used sparingly to notify registry users of
-- | events with formatted, human-readable messages providing context.
module Registry.App.Effect.Notify where

import Registry.App.Prelude

import Ansi.Codes (GraphicsParam)
import Data.Int as Int
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Foreign.Octokit (Address, IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Run (AFF, Run)
import Run as Run

data Notify a = Notify (Doc GraphicsParam) a

derive instance Functor Notify

-- | An effect for notifying consumers of important events in the application
type NOTIFY r = (notify :: Notify | r)

_notify :: Proxy "notify"
_notify = Proxy

notify :: forall a r. Log.Loggable a => a -> Run (NOTIFY + r) Unit
notify message = Run.lift _notify (Notify (Log.toLog message) unit)

runNotify :: forall r a. (Notify ~> Run r) -> Run (NOTIFY + r) a -> Run r a
runNotify handler = Run.interpret (Run.on _notify handler Run.send)

-- | Handle a notification by logging it to the console.
handleNotifyLog :: forall a r. Notify a -> Run (LOG + r) a
handleNotifyLog = case _ of
  Notify message next -> do
    Log.info $ Ansi.foreground Ansi.BrightBlue (Dodo.text "[NOTIFY] ") <> message
    pure next

type NotifyGitHubEnv =
  { octokit :: Octokit
  , issue :: IssueNumber
  , registry :: Address
  }

-- | Handle a notification by commenting on the relevant GitHub issue.
handleNotifyGitHub :: forall a r. NotifyGitHubEnv -> Notify a -> Run (LOG + AFF + r) a
handleNotifyGitHub env = case _ of
  Notify message next -> do
    let issueNumber = Int.toStringAs Int.decimal $ un IssueNumber env.issue
    Log.debug $ "Notifying via a GitHub comment on issue " <> issueNumber
    handleNotifyLog (Notify message unit)
    let comment = Dodo.print Dodo.plainText Dodo.twoSpaces (Log.toLog message)
    let request = Octokit.createCommentRequest { address: env.registry, issue: env.issue, body: comment }
    Run.liftAff (Octokit.request env.octokit request) >>= case _ of
      Left error -> do
        Log.error $ "Could not send comment to GitHub due to an unexpected error."
        Log.debug $ Octokit.printGitHubError error
      Right _ ->
        Log.debug $ "Created GitHub comment on issue " <> issueNumber
    pure next
