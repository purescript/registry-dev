-- | A general logging effect suitable for recording events as they happen in
-- | the application, including debugging logs. Should not be used to report
-- | important events to registry users; for that, use the Notify effect.
module Registry.App.Monad.Log where

import Registry.App.Prelude

import Ansi.Codes (GraphicsParam)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
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

-- | An effect for logging events in the application
class Monad m <= MonadLog m where
  log :: LogLevel -> Doc GraphicsParam -> m Unit

instance MonadLog m => MonadLog (ExceptT e m) where
  log level = lift <<< log level

debug :: forall a m. MonadLog m => Loggable a => a -> m Unit
debug = log Debug <<< toLog

info :: forall a m. MonadLog m => Loggable a => a -> m Unit
info = log Info <<< toLog

warn :: forall a m. MonadLog m => Loggable a => a -> m Unit
warn = log Warn <<< toLog

error :: forall a m. MonadLog m => Loggable a => a -> m Unit
error = log Error <<< toLog

type LogTerminalEnv =
  { verbosity :: LogVerbosity
  }

-- | Write logs to the terminal only.
handleLogTerminal
  :: forall m r
   . MonadAsk { log :: LogTerminalEnv | r } m
  => MonadEffect m
  => LogLevel
  -> Doc GraphicsParam
  -> m Unit
handleLogTerminal level message = do
  env <- asks _.log
  let
    printed = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces $ case level of
      Debug -> Ansi.foreground Ansi.Blue message
      Info -> message
      Warn -> Ansi.foreground Ansi.Yellow (Dodo.text "[WARNING] ") <> message
      Error -> Ansi.foreground Ansi.Red (Dodo.text "[ERROR] ") <> message

  case env.verbosity of
    Quiet -> pure unit
    Normal -> when (level /= Debug) (Console.log printed)
    Verbose -> Console.log printed

type LogFsEnv =
  { verbosity :: LogVerbosity
  , logfile :: FilePath
  }

-- | Write logs to the specified logfile.
handleLogFs
  :: forall m r
   . MonadAsk { log :: LogFsEnv | r } m
  => MonadAff m
  => LogLevel
  -> Doc GraphicsParam
  -> m Unit
handleLogFs level message = do
  env <- asks _.log
  let
    attemptWrite = do
      now <- nowUTC
      let
        time = Formatters.DateTime.format Internal.Format.iso8601DateTime now

        level' = case level of
          Debug -> "DEBUG"
          Info -> "INFO"
          Warn -> "WARN"
          Error -> "ERROR"

        formatted = Dodo.print Dodo.plainText Dodo.twoSpaces
          ( Array.fold
              [ Dodo.text "["
              , Dodo.text time
              , Dodo.space
              , Dodo.text level'
              , Dodo.text "]"
              , Dodo.space
              , message
              , Dodo.break
              ]
          )

      liftAff (Aff.attempt (FS.Aff.appendTextFile UTF8 env.logfile formatted)) >>= case _ of
        Left err -> Console.error $ "LOG ERROR: Failed to write to file " <> env.logfile <> ": " <> Aff.message err
        Right _ -> pure unit

  case env.verbosity of
    Quiet -> pure unit
    Normal -> when (level /= Debug) attemptWrite
    Verbose -> attemptWrite
