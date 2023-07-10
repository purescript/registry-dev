-- | A general logging effect suitable for recording events as they happen in
-- | the application, including debugging logs. Should not be used to report
-- | important events to registry users; for that, use the Notify effect.
module Registry.App.Effect.Log where

import Registry.App.Prelude

import Ansi.Codes (GraphicsParam)
import Data.Array as Array
import Data.Formatter.DateTime as Formatters.DateTime
import Data.Int as Int
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Registry.App.Effect.Db (Db, JobId)
import Registry.App.Effect.Db as Db
import Registry.Foreign.Octokit (Address, IssueNumber(..), Octokit)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Type.Proxy (Proxy(..))

class Loggable a where
  toLog :: a -> Doc GraphicsParam

instance Loggable (Doc GraphicsParam) where
  toLog = identity

instance Loggable String where
  toLog = Dodo.text

instance Loggable PackageName where
  toLog = toLog <<< PackageName.print

instance Loggable Version where
  toLog = toLog <<< Version.print

instance Loggable Range where
  toLog = toLog <<< Range.print

data Log a = Log LogLevel (Doc GraphicsParam) a

derive instance Functor Log

-- | An effect for recording logs about events in the application
type LOG r = (log :: Log | r)

_log :: Proxy "log"
_log = Proxy

log :: forall a r. Loggable a => LogLevel -> a -> Run (LOG + r) Unit
log level message = Run.lift _log (Log level (toLog message) unit)

debug :: forall a r. Loggable a => a -> Run (LOG + r) Unit
debug = log Debug <<< toLog

info :: forall a r. Loggable a => a -> Run (LOG + r) Unit
info = log Info <<< toLog

warn :: forall a r. Loggable a => a -> Run (LOG + r) Unit
warn = log Warn <<< toLog

error :: forall a r. Loggable a => a -> Run (LOG + r) Unit
error = log Error <<< toLog

notify :: forall a r. Loggable a => a -> Run (LOG + r) Unit
notify = log Notify <<< toLog

interpret :: forall a r. (Log ~> Run r) -> Run (LOG + r) a -> Run r a
interpret handler = Run.interpret (Run.on _log handler Run.send)

-- | Write logs to the terminal only.
handleTerminal :: forall a r. LogVerbosity -> Log a -> Run (AFF + r) a
handleTerminal verbosity = case _ of
  Log level message next -> do
    let
      printed = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces $ case level of
        Debug -> Ansi.foreground Ansi.Blue message
        Info -> message
        Warn -> Ansi.foreground Ansi.Yellow (Dodo.text "[WARNING] ") <> message
        Error -> Ansi.foreground Ansi.Red (Dodo.text "[ERROR] ") <> message
        Notify -> Ansi.foreground Ansi.BrightBlue (Dodo.text "[NOTIFY] ") <> message

    Run.liftAff case verbosity of
      Quiet -> pure unit
      Normal -> when (level /= Debug) (Console.log printed)
      Verbose -> Console.log printed

    pure next

-- | Write logs to the specified logfile.
handleFs :: forall a r. LogVerbosity -> FilePath -> Log a -> Run (AFF + EFFECT + r) a
handleFs verbosity logfile action = case action of
  Log level message next -> do
    let
      attemptWrite = do
        now <- nowUTC
        let
          time = Formatters.DateTime.format Internal.Format.iso8601DateTime now

          formatted = Dodo.print Dodo.plainText Dodo.twoSpaces
            ( Array.fold
                [ Dodo.text "["
                , Dodo.text time
                , Dodo.space
                , Dodo.text (printLogLevel level)
                , Dodo.text "]"
                , Dodo.space
                , message
                , Dodo.break
                ]
            )

        Run.liftAff (Aff.attempt (FS.Aff.appendTextFile UTF8 logfile formatted)) >>= case _ of
          Left err -> Console.error $ "LOG ERROR: Failed to write to file " <> logfile <> ": " <> Aff.message err
          Right _ -> pure unit

    case verbosity of
      Quiet -> pure unit
      Normal -> when (level /= Debug) attemptWrite
      Verbose -> attemptWrite

    pure next

type LogGitHubEnv =
  { octokit :: Octokit
  , issue :: IssueNumber
  , registry :: Address
  }

-- | Handle a log by commenting on the relevant GitHub issue, if important for the user.
handleGitHub :: forall a r. LogGitHubEnv -> Log a -> Run (AFF + EFFECT + r) a
handleGitHub env = case _ of
  Log Notify message next -> do
    let _issueNumber = Int.toStringAs Int.decimal $ un IssueNumber env.issue
    let comment = Dodo.print Dodo.plainText Dodo.twoSpaces (toLog message)
    let request = Octokit.createCommentRequest { address: env.registry, issue: env.issue, body: comment }
    -- TODO: figure out how to log here
    -- debug $ "Notifying via a GitHub comment on issue " <> issueNumber
    Octokit.request env.octokit request >>= case _ of
      Left _err -> do
        -- error $ "Could not send comment to GitHub due to an unexpected error."
        -- debug $ Octokit.printGitHubError err
        pure unit
      Right _ ->
        -- debug $ "Created GitHub comment on issue " <> issueNumber
        pure unit
    pure next
  Log _ _ next -> pure next

type LogDbEnv =
  { db :: Db
  }

-- | Handle a log by recording it in the database.
handleDb :: forall a r. LogDbEnv -> JobId -> Log a -> Run (AFF + EFFECT + r) a
handleDb env jobId = case _ of
  Log level message next -> do
    timestamp <- nowUTC
    let
      row =
        { timestamp
        , level
        , jobId
        , message: Dodo.print Dodo.plainText Dodo.twoSpaces (toLog message)
        }
    liftEffect $ Db.insertLog env.db row
    pure next
