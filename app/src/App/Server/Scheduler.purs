-- | Scheduler for admin jobs (PackageTransfer, LegacyImport, PackageSetUpdate).
module Registry.App.Server.Scheduler
  ( runScheduler
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime, Time(..))
import Data.DateTime as DateTime
import Data.Enum (fromEnum)
import Data.Time.Duration (Hours(..), fromDuration, negateDuration)
import Effect.Aff as Aff
import Registry.API.V1 (AdminJobType(..))
import Registry.API.V1 as V1
import Registry.App.Effect.Db as Db
import Registry.App.Effect.Log as Log
import Registry.App.SQLite (AdminJobDetails)
import Registry.App.Server.Env (ServerEnv, runEffects)

-- | The three admin job types that run on schedule.
-- | They are enqueued in this order: PackageTransfer -> LegacyImport -> PackageSetUpdate
scheduledAdminJobs :: Array AdminJobType
scheduledAdminJobs =
  [ AdminPackageTransfer
  , AdminLegacyImport V1.UpdateRegistry
  , AdminPackageSetUpdate V1.CommitPackageSet
  ]

-- | Run the scheduler loop. Checks every hour if jobs should be enqueued.
-- | We run things in a window instead of a precise time, so that restarts and/or
-- | delays don't prevent jobs from happening.
runScheduler :: ServerEnv -> Aff (Either Aff.Error Unit)
runScheduler env = runEffects env do
  Log.info "Starting Admin Job Scheduler"
  loop
  where
  loop = do
    liftAff $ Aff.delay $ fromDuration (Hours 1.0)
    now <- nowUTC

    when (inScheduleWindow now) do
      Log.info "In schedule window (00:00-04:00 UTC) - checking if admin jobs should be scheduled..."
      -- Get jobs from last 12h
      let twelveHoursAgo = fromMaybe now $ DateTime.adjust (negateDuration (Hours 12.0)) now
      recentJobs <- Db.selectRecentAdminJobs twelveHoursAgo
      for_ scheduledAdminJobs \jobType -> do
        when (shouldEnqueue jobType recentJobs) do
          Log.info $ "Scheduling admin job: " <> V1.adminJobTypeKey jobType
          void $ Db.insertAdminJob
            { adminJobType: jobType
            , rawPayload: Nothing
            , signature: Nothing
            }

    loop

-- | Check if current time is in the schedule window
inScheduleWindow :: DateTime -> Boolean
inScheduleWindow dt =
  let
    Time hour _ _ _ = DateTime.time dt
  in
    fromEnum hour >= 0 && fromEnum hour < 4

-- | Determine if we should enqueue a job of the given type.
-- | Returns true if:
-- | 1. No incomplete job of that type exists (prevents duplicates)
-- | 2. Either never run, or last completed job was >12 hours ago
shouldEnqueue :: AdminJobType -> Array AdminJobDetails -> Boolean
shouldEnqueue jobType recentJobs =
  let
    jobsOfType = Array.filter (\j -> V1.adminJobTypeKey j.adminJobType == V1.adminJobTypeKey jobType) recentJobs
    hasIncomplete = Array.any (\j -> isNothing j.finishedAt) jobsOfType
    lastCompleted = Array.last $ Array.sortBy (comparing _.createdAt) $
      Array.filter (\j -> isJust j.finishedAt) jobsOfType
  in
    not hasIncomplete && isJust lastCompleted
