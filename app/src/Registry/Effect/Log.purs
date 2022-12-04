module Registry.Effect.Log where

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

formatPackageVersion :: PackageName -> Version -> String
formatPackageVersion name version = PackageName.print name <> "@" <> Version.print version

class MonadLog m where
  log :: LogLevel -> Doc GraphicsParam -> m Unit

debug :: forall m a. Loggable a => MonadLog m => a -> m Unit
debug = log Debug <<< toLog

info :: forall m a. Loggable a => MonadLog m => a -> m Unit
info = log Info <<< toLog

warn :: forall m a. Loggable a => MonadLog m => a -> m Unit
warn = log Warn <<< toLog

error :: forall m a. Loggable a => MonadLog m => a -> m Unit
error = log Error <<< toLog

type TerminalEnv = { verbosity :: LogVerbosity }

runLogTerminal :: forall m. MonadEffect m => TerminalEnv -> LogLevel -> Doc GraphicsParam -> m Unit
runLogTerminal { verbosity } level message = case verbosity of
  Quiet -> pure unit
  Normal -> when (level /= Debug) (Console.log printed)
  Verbose -> Console.log printed
  where
  printed = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces message

type FsEnv = { verbosity :: LogVerbosity, logfile :: FilePath }

runLogFs :: forall m. MonadAff m => FsEnv -> LogLevel -> Doc GraphicsParam -> m Unit
runLogFs { verbosity, logfile } level message = do
  now <- liftEffect nowUTC
  runLogTerminal { verbosity } level message

  let
    time = Formatters.DateTime.format Internal.Format.iso8601DateTime now
    formatted = Dodo.print Dodo.plainText Dodo.twoSpaces (Dodo.text time <> Dodo.space <> message)
    attemptWrite = liftAff do
      Aff.attempt (FS.Aff.appendTextFile UTF8 logfile formatted) >>= case _ of
        Left err -> do
          Console.log $ "Failed to write to file " <> logfile <> ": " <> Aff.message err
          Aff.throwError err
        Right _ -> pure unit

  case verbosity of
    Quiet -> pure unit
    Normal -> when (level /= Debug) attemptWrite
    Verbose -> attemptWrite
