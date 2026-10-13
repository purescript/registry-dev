module Test.Registry.App.SQLite (spec) where

import Registry.App.Prelude

import Effect.Exception as Exception
import Registry.API.V1 (JobId(..), SortOrder(..))
import Registry.API.V1 as V1
import Registry.App.SQLite (SQLite)
import Registry.App.SQLite as SQLite
import Registry.Location (Location(..))
import Registry.Operation as Operation
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec as Spec

foreign import withDatabase :: (SQLite -> Effect Unit) -> Effect Unit

foreign import insertHistoricalPublishJob :: SQLite -> String -> Effect String

spec :: Spec.Spec Unit
spec = Spec.it "caches failed publishes while preserving active and successful historical attempts" $ liftEffect $ withDatabase \db -> do
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
    submit payload' = SQLite.insertPublishJob db { payload: payload' }
    create payload' = do
      result <- submit payload'
      unless result.created $ Exception.throw "Expected a new publish job."
      pure result.jobId

  lookup >>= Assert.shouldEqual (Right Nothing)
  failedId <- create payload
  lookup >>= Assert.shouldEqual (Right $ Just failedId)
  submit payload >>= Assert.shouldEqual { jobId: failedId, created: false }
  now <- nowUTC
  SQLite.startJob db { jobId: failedId, startedAt: now }
  submit payload >>= Assert.shouldEqual { jobId: failedId, created: false }
  SQLite.finishJob db { jobId: failedId, finishedAt: now, success: false, disposition: Nothing, error: Nothing }
  lookup >>= Assert.shouldEqual (Right Nothing)

  submit payload >>= Assert.shouldEqual { jobId: failedId, created: false }
  let corrected = payload { location = Just $ GitHub { owner: "katsujukou", repo: "purescript-puppy-runtime", subdir: Nothing } }
  submit corrected >>= Assert.shouldEqual { jobId: failedId, created: false }
  submit (corrected { ref = "different-ref" }) >>= Assert.shouldEqual { jobId: failedId, created: false }
  lookup >>= Assert.shouldEqual (Right Nothing)

  -- Production may already have a newer attempt following an older failure.
  correctedId <- JobId <$> insertHistoricalPublishJob db (stringifyJson Operation.publishCodec corrected)
  selected <- SQLite.selectNextPublishJob db
  map (map _.payload) selected `Assert.shouldEqual` Right (Just corrected)
  submit payload >>= Assert.shouldEqual { jobId: correctedId, created: false }
  SQLite.selectNextPublishJob db >>= \next ->
    map (map _.jobId) next `Assert.shouldEqual` Right (Just correctedId)
  SQLite.startJob db { jobId: correctedId, startedAt: now }
  submit corrected >>= Assert.shouldEqual { jobId: correctedId, created: false }
  SQLite.finishJob db { jobId: correctedId, finishedAt: now, success: true, disposition: Just V1.Published, error: Nothing }
  submit corrected >>= Assert.shouldEqual { jobId: correctedId, created: false }
  submit payload >>= Assert.shouldEqual { jobId: failedId, created: false }
  submit (corrected { ref = "different-ref" }) >>= Assert.shouldEqual { jobId: failedId, created: false }
  lookup >>= Assert.shouldEqual (Right Nothing)

  -- A new version is independent, but a changed payload cannot rerun its success.
  let nextVersion = corrected { version = Utils.unsafeVersion "0.2.1", ref = "v0.2.1" }
  nextId <- create nextVersion
  SQLite.finishJob db { jobId: nextId, finishedAt: now, success: true, disposition: Just V1.Published, error: Nothing }
  submit (nextVersion { ref = "different-ref" }) >>= Assert.shouldEqual { jobId: nextId, created: false }
  lookup >>= Assert.shouldEqual (Right Nothing)

  -- Looking up the old failure by ID must still work.
  result <- SQLite.selectJob db { jobId: failedId, level: Nothing, since: bottom, until: top, order: ASC }
  map (map (\job -> (V1.jobInfo job).success)) result.job `Assert.shouldEqual` Right (Just false)
