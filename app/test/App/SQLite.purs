module Test.Registry.App.SQLite (spec) where

import Registry.App.Prelude

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
    lookup = map (map (map _.jobId)) $ SQLite.selectPublishJob db payload.name payload.version

  lookup >>= Assert.shouldEqual (Right Nothing)
  failedId <- SQLite.insertPublishJob db { payload }
  lookup >>= Assert.shouldEqual (Right $ Just failedId)
  now <- nowUTC
  SQLite.startJob db { jobId: failedId, startedAt: now }
  lookup >>= Assert.shouldEqual (Right $ Just failedId)
  SQLite.finishJob db { jobId: failedId, finishedAt: now, success: false }
  lookup >>= Assert.shouldEqual (Right Nothing)

  -- An unchanged payload must also be retryable after a transient failure.
  retryId <- SQLite.insertPublishJob db { payload }
  lookup >>= Assert.shouldEqual (Right $ Just retryId)
  SQLite.finishJob db { jobId: retryId, finishedAt: now, success: false }
  lookup >>= Assert.shouldEqual (Right Nothing)

  let corrected = payload { location = Just $ GitHub { owner: "katsujukou", repo: "purescript-puppy-runtime", subdir: Nothing } }
  correctedId <- SQLite.insertPublishJob db { payload: corrected }
  selected <- SQLite.selectPublishJob db payload.name payload.version
  map (map _.payload) selected `Assert.shouldEqual` Right (Just corrected)
  SQLite.selectNextPublishJob db >>= \next ->
    map (map _.jobId) next `Assert.shouldEqual` Right (Just correctedId)
  SQLite.startJob db { jobId: correctedId, startedAt: now }
  lookup >>= Assert.shouldEqual (Right $ Just correctedId)
  SQLite.finishJob db { jobId: correctedId, finishedAt: now, success: true }
  lookup >>= Assert.shouldEqual (Right $ Just correctedId)

  -- Looking up the old failure by ID must still work.
  result <- SQLite.selectJob db { jobId: failedId, level: Nothing, since: bottom, until: top, order: ASC }
  map (map (\job -> (V1.jobInfo job).success)) result.job `Assert.shouldEqual` Right (Just false)
