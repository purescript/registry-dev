module Test.Registry.App.SQLite (spec) where

import Registry.App.Prelude

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

spec :: Spec.Spec Unit
spec = Spec.it "allows failed publishes to be retried without losing deduplication or job history" $ liftEffect $ withDatabase \db -> do
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
