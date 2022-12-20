-- | A general logging effect suitable for recording events as they happen in
-- | the application, including debugging logs. Should not be used to report
-- | important events to registry users; for that, use the Notify effect.
module Registry.App.Effect.Log where

import Registry.App.Prelude

import Ansi.Codes (GraphicsParam)
import Data.Formatter.DateTime as Formatters.DateTime
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (Except)
import Run.Except as Run.Except
import Type.Proxy (Proxy(..))

data LogLevel = Debug | Info | Warn | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

data LogVerbosity = Quiet | Normal | Verbose

derive instance Eq LogVerbosity
derive instance Ord LogVerbosity

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

-- | Write logs to the terminal only.
handleLogTerminal :: forall a r. LogVerbosity -> Log a -> Run (AFF + r) a
handleLogTerminal verbosity = case _ of
  Log level message next -> do
    let printed = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces message
    Run.liftAff case verbosity of
      Quiet -> pure unit
      Normal -> when (level /= Debug) (Console.log printed)
      Verbose -> Console.log printed
    pure next

-- | Write logs to the specified logfile.
handleLogFs :: forall a r. LogVerbosity -> FilePath -> Log a -> Run (AFF + EFFECT + r) a
handleLogFs verbosity logfile action = case action of
  Log level message next -> do
    let
      attemptWrite = do
        now <- Run.liftEffect nowUTC
        let time = Formatters.DateTime.format Internal.Format.iso8601DateTime now
        let formatted = Dodo.print Dodo.plainText Dodo.twoSpaces (Dodo.text "[" <> Dodo.text time <> Dodo.text "]" <> Dodo.space <> message <> Dodo.break)
        Run.liftAff (Aff.attempt (FS.Aff.appendTextFile UTF8 logfile formatted)) >>= case _ of
          Left err -> Console.error $ "LOG ERROR: Failed to write to file " <> logfile <> ": " <> Aff.message err
          Right _ -> pure unit

    case verbosity of
      Quiet -> pure unit
      Normal -> when (level /= Debug) attemptWrite
      Verbose -> attemptWrite

    pure next

type LOG_EXCEPT :: Row (Type -> Type) -> Row (Type -> Type)
type LOG_EXCEPT r = (logExcept :: Except (Doc GraphicsParam) | r)

_logExcept :: Proxy "logExcept"
_logExcept = Proxy

-- | Terminate a computation with a formatted error suitable for display.
exit :: forall a r void. Loggable a => a -> Run (LOG_EXCEPT + r) void
exit message = Run.Except.throwAt _logExcept (toLog message)
