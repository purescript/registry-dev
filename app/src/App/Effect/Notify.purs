module Registry.App.Effect.Notify where

import Registry.App.Prelude

import Ansi.Codes (GraphicsParam)
import Data.Int as Int
import Dodo (Doc)
import Dodo as Dodo
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Constants as Constants
import Registry.Foreign.Octokit (IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Run.Except

data Notify a = Notify (Doc GraphicsParam) a

derive instance Functor Notify

-- | An effect for notifying consumers of important events in the application
type NOTIFY r = (notify :: Notify | r)

_notify :: Proxy "notify"
_notify = Proxy

notify :: forall a r. Log.Loggable a => a -> Run (NOTIFY + r) Unit
notify message = Run.lift _notify (Notify (Log.toLog message) unit)

-- | Throw an exception after notifying consumers of an error.
exit :: forall a r void. Log.Loggable a => a -> Run (NOTIFY + EXCEPT Unit + r) void
exit message = notify message *> Run.Except.throw unit

handleNotifyGitHub :: forall a r. Octokit -> IssueNumber -> Notify a -> Run (LOG + EXCEPT Unit + AFF + r) a
handleNotifyGitHub octokit issue = case _ of
  Notify message next -> do
    let issueNumber = Int.toStringAs Int.decimal $ un IssueNumber issue
    Log.debug $ "Notifying via a GitHub comment on issue " <> issueNumber
    Log.info message
    let comment = Dodo.print Dodo.plainText Dodo.twoSpaces (Log.toLog message)
    let request = Octokit.createCommentRequest { address: Constants.registry, issue, body: comment }
    Run.liftAff (Octokit.request octokit request) >>= case _ of
      Left error -> do
        Log.error $ "Could not send comment to GitHub due to an unexpected error."
        Log.debug $ Octokit.printGitHubError error
        Run.Except.throw unit
      Right _ ->
        Log.debug $ "Created GitHub comment on issue " <> issueNumber
    pure next
