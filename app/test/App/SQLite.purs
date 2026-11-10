module Test.Registry.App.SQLite (spec, withDatabaseAff) where

import Registry.App.Prelude

import Data.Array as Array
import Effect.Aff as Aff
import Effect.Exception as Exception
import Registry.API.V1 (SortOrder(..))
import Registry.API.V1 as V1
import Registry.App.SQLite (SQLite)
import Registry.App.SQLite as SQLite
import Registry.Location (Location(..))
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec as Spec

foreign import withDatabase :: (SQLite -> Effect Unit) -> Effect Unit

foreign import openDatabase :: Effect SQLite
foreign import closeDatabase :: SQLite -> Effect Unit

withDatabaseAff :: forall a. (SQLite -> Aff a) -> Aff a
withDatabaseAff = Aff.bracket (liftEffect openDatabase) (liftEffect <<< closeDatabase)

spec :: Spec.Spec Unit
spec = do
  logSpec
  publishSpec

logSpec :: Spec.Spec Unit
logSpec = Spec.it "filters job logs within a half-open window in either order, capped at 100" $ liftEffect $ withDatabase \db -> do
  let
    payload =
      { name: Utils.unsafePackageName "log-test"
      , version: Utils.unsafeVersion "1.0.0"
      , ref: "v1.0.0"
      , compiler: Nothing
      , resolutions: Nothing
      , location: Just $ GitHub { owner: "purescript", repo: "log-test", subdir: Nothing }
      }
    create name = SQLite.insertPublishJob db { payload: payload { name = Utils.unsafePackageName name } } >>= case _ of
      SQLite.PublishJobCreated jobId -> pure jobId
      _ -> Exception.throw "Expected a new publish job."
    before = Utils.unsafeDateTime "2026-11-10T00:00:00.000Z"
    since = Utils.unsafeDateTime "2026-11-10T00:00:01.000Z"
    middle = Utils.unsafeDateTime "2026-11-10T00:00:02.000Z"
    until = Utils.unsafeDateTime "2026-11-10T00:00:03.000Z"
  jobId <- create "log-test"
  otherJobId <- create "other-log-test"
  let
    insert job timestamp level message = SQLite.insertLogLine db { jobId: job, timestamp, level, message }
    select level order = SQLite.selectLogsByJob db jobId level since until order
  -- Insertion order differs from timestamp order; boundary and level checks
  -- distinguish >= since, < until, and >= level from their alternatives.
  insert jobId middle V1.Error "later"
  insert jobId until V1.Error "excluded upper boundary"
  insert otherJobId middle V1.Error "other job"
  insert jobId before V1.Error "before window"
  insert jobId since V1.Warn "lower boundary"
  insert jobId middle V1.Debug "below level"
  asc <- select V1.Warn ASC
  asc.fail `Assert.shouldEqual` []
  map _.message asc.success `Assert.shouldEqual` [ "lower boundary", "later" ]
  desc <- select V1.Warn DESC
  desc.fail `Assert.shouldEqual` []
  map _.message desc.success `Assert.shouldEqual` [ "later", "lower boundary" ]
  debug <- select V1.Debug ASC
  Array.length debug.success `Assert.shouldEqual` 3
  empty <- SQLite.selectLogsByJob db jobId V1.Debug since since ASC
  empty.success `Assert.shouldEqual` []
  for_ (Array.range 1 105) \_ -> insert jobId middle V1.Info "bulk"
  for_ [ ASC, DESC ] \order -> do
    capped <- select V1.Info order
    Array.length capped.success `Assert.shouldEqual` 100
    capped.fail `Assert.shouldEqual` []
    let expectedFirst = if order == ASC then since else middle
    map _.timestamp (Array.head capped.success) `Assert.shouldEqual` Just expectedFirst

publishSpec :: Spec.Spec Unit
publishSpec = Spec.it "allows failed publishes to be retried without losing deduplication or job history" $ liftEffect $ withDatabase \db -> do
  let
    payload =
      { name: Utils.unsafePackageName "puppy-runtime"
      , version: Utils.unsafeVersion "0.2.0"
      , ref: "v0.2.0"
      , compiler: Nothing
      , resolutions: Nothing
      , location: Just $ GitHub { owner: "katsujukou", repo: "puppy", subdir: Just "puppy-runtime" }
      }
    lookup = map (map (map _.jobId)) $ SQLite.selectNextPublishJob db
    submit payload' = SQLite.insertPublishJob db { payload: payload' } <#> case _ of
      SQLite.PublishJobCreated jobId -> Tuple V1.Created jobId
      SQLite.PublishJobDuplicateActive jobId -> Tuple V1.DuplicateActive jobId
      SQLite.PublishJobAlreadyPublished jobId -> Tuple V1.AlreadyPublishedSubmission jobId
    create payload' = SQLite.insertPublishJob db { payload: payload' } >>= case _ of
      SQLite.PublishJobCreated jobId -> pure jobId
      _ -> Exception.throw "Expected a new publish job."

  lookup >>= Assert.shouldEqual (Right Nothing)
  failedId <- create payload
  lookup >>= Assert.shouldEqual (Right $ Just failedId)
  submit payload >>= Assert.shouldEqual (Tuple V1.DuplicateActive failedId)
  now <- nowUTC
  SQLite.startJob db { jobId: failedId, startedAt: now }
  submit payload >>= Assert.shouldEqual (Tuple V1.DuplicateActive failedId)
  SQLite.finishJob db { jobId: failedId, finishedAt: now, success: false, disposition: Nothing, error: Nothing }
  lookup >>= Assert.shouldEqual (Right Nothing)

  -- An unchanged payload must also be retryable after a transient failure.
  retryId <- create payload
  lookup >>= Assert.shouldEqual (Right $ Just retryId)
  SQLite.finishJob db { jobId: retryId, finishedAt: now, success: false, disposition: Nothing, error: Nothing }
  lookup >>= Assert.shouldEqual (Right Nothing)

  let corrected = payload { location = Just $ GitHub { owner: "katsujukou", repo: "purescript-puppy-runtime", subdir: Nothing } }
  correctedId <- create corrected
  selected <- SQLite.selectNextPublishJob db
  map (map _.payload) selected `Assert.shouldEqual` Right (Just corrected)
  submit payload >>= Assert.shouldEqual (Tuple V1.DuplicateActive correctedId)
  SQLite.selectNextPublishJob db >>= \next ->
    map (map _.jobId) next `Assert.shouldEqual` Right (Just correctedId)
  SQLite.startJob db { jobId: correctedId, startedAt: now }
  submit corrected >>= Assert.shouldEqual (Tuple V1.DuplicateActive correctedId)
  SQLite.finishJob db { jobId: correctedId, finishedAt: now, success: true, disposition: Just V1.Published, error: Nothing }
  submit corrected >>= Assert.shouldEqual (Tuple V1.AlreadyPublishedSubmission correctedId)
  lookup >>= Assert.shouldEqual (Right Nothing)

  -- Looking up the old failure by ID must still work.
  result <- SQLite.selectJob db { jobId: failedId, level: Nothing, since: bottom, until: top, order: ASC }
  map (map (\job -> (V1.jobInfo job).success)) result.job `Assert.shouldEqual` Right (Just false)
