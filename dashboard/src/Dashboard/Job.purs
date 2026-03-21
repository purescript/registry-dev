-- | Shared helpers for working with registry jobs. These are used by both
-- | the JobsList and JobDetail components to avoid duplicating logic for
-- | deriving job status, extracting job fields, and formatting durations.
module Dashboard.Job
  ( JobStatus(..)
  , JobSummary
  , deriveStatus
  , printStatus
  , toJobSummary
  , getJobType
  , getPackageName
  , getPackageVersion
  , getCompilerVersion
  , formatTimestamp
  , formatDateTimeLocal
  , parseDateTimeLocal
  , formatDurationSecs
  , formatDurationBetween
  , timerEmitter
  ) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds)
import Effect.Aff (Milliseconds)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Registry.API.V1 (Job(..), JobId, JobType(..))
import Registry.API.V1 as V1
import Registry.PackageName (PackageName)
import Registry.Version (Version)

-- | Client-side job status derived from the job's timestamps and success flag.
data JobStatus
  = Pending
  | Running
  | Succeeded
  | Failed

derive instance Eq JobStatus

-- | Print a job status as a lowercase string suitable for CSS class names.
printStatus :: JobStatus -> String
printStatus = case _ of
  Pending -> "pending"
  Running -> "running"
  Succeeded -> "succeeded"
  Failed -> "failed"

-- | Derive the status of a job from its fields.
deriveStatus :: forall r. { startedAt :: Maybe DateTime, finishedAt :: Maybe DateTime, success :: Boolean | r } -> JobStatus
deriveStatus job
  | isJust job.finishedAt && job.success = Succeeded
  | isJust job.finishedAt && not job.success = Failed
  | isJust job.startedAt && isNothing job.finishedAt = Running
  | otherwise = Pending

-- | A lightweight summary of a Job containing only the fields needed for
-- | list display. Stripping the payload (which can be large, e.g. a full
-- | resolution map for matrix jobs) and logs avoids holding unnecessary data
-- | in component state.
type JobSummary =
  { jobId :: JobId
  , jobType :: JobType
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , packageName :: Maybe PackageName
  , packageVersion :: Maybe Version
  , compilerVersion :: Maybe Version
  }

-- | Project a full Job into a lightweight JobSummary, discarding the payload
-- | and logs.
toJobSummary :: Job -> JobSummary
toJobSummary job = do
  let info = V1.jobInfo job
  { jobId: info.jobId
  , jobType: getJobType job
  , createdAt: info.createdAt
  , startedAt: info.startedAt
  , finishedAt: info.finishedAt
  , success: info.success
  , packageName: getPackageName job
  , packageVersion: getPackageVersion job
  , compilerVersion: getCompilerVersion job
  }

-- | Extract the job type from a Job.
getJobType :: Job -> JobType
getJobType = case _ of
  PublishJob _ -> PublishJobType
  UnpublishJob _ -> UnpublishJobType
  TransferJob _ -> TransferJobType
  MatrixJob _ -> MatrixJobType
  PackageSetJob _ -> PackageSetJobType

-- | Extract the package name from a Job, if present.
getPackageName :: Job -> Maybe PackageName
getPackageName = case _ of
  PublishJob r -> Just r.packageName
  UnpublishJob r -> Just r.packageName
  TransferJob r -> Just r.packageName
  MatrixJob r -> Just r.packageName
  PackageSetJob _ -> Nothing

-- | Extract the package version from a Job, if present.
getPackageVersion :: Job -> Maybe Version
getPackageVersion = case _ of
  PublishJob r -> Just r.packageVersion
  UnpublishJob r -> Just r.packageVersion
  TransferJob _ -> Nothing
  MatrixJob r -> Just r.packageVersion
  PackageSetJob _ -> Nothing

-- | Extract the compiler version from a Job, if present (matrix jobs only).
getCompilerVersion :: Job -> Maybe Version
getCompilerVersion = case _ of
  MatrixJob r -> Just r.compilerVersion
  _ -> Nothing

-- | Format a DateTime as "YYYY-MM-DD HH:MM:SS".
formatTimestamp :: DateTime -> String
formatTimestamp = Formatter.DateTime.format timestampFormat

-- "YYYY-MM-DD HH:MM:SS"
timestampFormat :: List FormatterCommand
timestampFormat = List.fromFoldable
  [ YearFull, Placeholder "-", MonthTwoDigits, Placeholder "-", DayOfMonthTwoDigits
  , Placeholder " ", Hours24, Placeholder ":", MinutesTwoDigits, Placeholder ":", SecondsTwoDigits
  ]

-- | Format a DateTime as an HTML datetime-local input value "YYYY-MM-DDTHH:MM".
formatDateTimeLocal :: DateTime -> String
formatDateTimeLocal = Formatter.DateTime.format dateTimeLocalFormat

-- | Parse an HTML datetime-local input value ("YYYY-MM-DDTHH:MM") into a DateTime.
parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal = hush <<< Formatter.DateTime.unformat dateTimeLocalFormat

-- "YYYY-MM-DDTHH:MM"
dateTimeLocalFormat :: List FormatterCommand
dateTimeLocalFormat = List.fromFoldable
  [ YearFull, Placeholder "-", MonthTwoDigits, Placeholder "-", DayOfMonthTwoDigits
  , Placeholder "T", Hours24, Placeholder ":", MinutesTwoDigits
  ]

-- | Format a duration in seconds as a human-readable string.
formatDurationSecs :: Int -> String
formatDurationSecs totalSecs
  | totalSecs < 60 = show totalSecs <> "s"
  | otherwise = do
      let mins = totalSecs / 60
      let remSecs = totalSecs `mod` 60
      show mins <> "m " <> show remSecs <> "s"

-- | Format the duration between two DateTimes as a human-readable string.
formatDurationBetween :: DateTime -> DateTime -> String
formatDurationBetween start end = do
  let diff = DateTime.diff end start :: Seconds
  formatDurationSecs (Int.floor (unwrap diff))

-- | Create a Halogen Emitter that fires the given action at a fixed interval.
timerEmitter :: forall action m. Applicative m => Milliseconds -> action -> m (Emitter action)
timerEmitter interval action = pure $ HS.makeEmitter \push -> do
  fiber <- Aff.launchAff $ forever do
    Aff.delay interval
    liftEffect (push action)
  pure (Aff.launchAff_ (Aff.killFiber (Aff.error "unsubscribe") fiber))
