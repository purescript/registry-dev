-- | An effect for notifying users of important events in the application, such
-- | as failures that prevent their package from being uploaded, or successful
-- | events that indicate progress.
-- |
-- | This is not a general logging effect. For that, you should use the Log
-- | effect. This effect should be used sparingly to notify registry users of
-- | events with formatted, human-readable messages providing context.
module Registry.App.Effect.Comment where

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
import Run (AFF, EFFECT, Run)
import Run as Run

data Comment a = Comment (Doc GraphicsParam) a

derive instance Functor Comment

-- | An effect for notifying consumers of important events in the application
type COMMENT r = (comment :: Comment | r)

_comment :: Proxy "comment"
_comment = Proxy

comment :: forall a r. Log.Loggable a => a -> Run (COMMENT + r) Unit
comment message = Run.lift _comment (Comment (Log.toLog message) unit)

interpret :: forall r a. (Comment ~> Run r) -> Run (COMMENT + r) a -> Run r a
interpret handler = Run.interpret (Run.on _comment handler Run.send)

-- | Handle a notification by converting it to an info-level LOG
handleLog :: forall a r. Comment a -> Run (LOG + r) a
handleLog = case _ of
  Comment message next -> do
    Log.info $ Ansi.foreground Ansi.BrightBlue (Dodo.text "[NOTIFY] ") <> message
    pure next

type CommentGitHubEnv =
  { octokit :: Octokit
  , issue :: IssueNumber
  , registry :: Address
  }

-- | Handle a notification by commenting on the relevant GitHub issue.
handleGitHub :: forall a r. CommentGitHubEnv -> Comment a -> Run (LOG + AFF + EFFECT + r) a
handleGitHub env = case _ of
  Comment message next -> do
    let issueNumber = Int.toStringAs Int.decimal $ un IssueNumber env.issue
    Log.debug $ "Commenting via a GitHub comment on issue " <> issueNumber
    handleLog (Comment message unit)
    let body = Dodo.print Dodo.plainText Dodo.twoSpaces (Log.toLog message)
    let request = Octokit.createCommentRequest { address: env.registry, issue: env.issue, body }
    Octokit.request env.octokit request >>= case _ of
      Left error -> do
        Log.error $ "Could not send comment to GitHub due to an unexpected error."
        Log.debug $ Octokit.printGitHubError error
      Right _ ->
        Log.debug $ "Created GitHub comment on issue " <> issueNumber
    pure next
