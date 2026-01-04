-- | End-to-end tests for the Publish API endpoint.
-- | These tests exercise the actual registry server via HTTP requests.
module Test.E2E.Publish (spec) where

import Prelude

import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JSON as JSON
import Registry.API.V1 (Job(..))
import Registry.API.V1 as V1
import Registry.Internal.Codec as Internal.Codec
import Registry.Operation as Operation
import Registry.PackageName (PackageName)
import Registry.Test.Assert as Assert
import Registry.Test.E2E.Client as Client
import Registry.Test.E2E.Fixtures as Fixtures
import Registry.Test.Utils (unsafePackageName, unsafeVersion)
import Registry.Version (Version)
import Registry.Version as Version
import Test.Spec (Spec)
import Test.Spec as Spec

-- | Get client config from environment
getConfig :: Aff Client.Config
getConfig = liftEffect Client.configFromEnv

spec :: Spec Unit
spec = do
  Spec.describe "Server connectivity" do
    Spec.it "can reach the status endpoint" do
      config <- getConfig
      result <- Client.getStatus config
      case result of
        Left err -> Assert.fail $ "Failed to reach status endpoint: " <> Client.printClientError err
        Right _ -> pure unit

    Spec.it "can list jobs (initially only compiler-upgrade matrix jobs)" do
      config <- getConfig
      result <- Client.getJobs config
      case result of
        Left err -> Assert.fail $ "Failed to list jobs: " <> Client.printClientError err
        Right jobs -> Assert.shouldEqual initialJobs (map deterministicJob jobs)

  Spec.describe "Publish workflow" do
    Spec.it "can publish effect@4.0.0 and filter logs" do
      config <- getConfig

      -- Submit publish request
      publishResult <- Client.publish config Fixtures.effectPublishData
      case publishResult of
        Left err -> Assert.fail $ "Failed to submit publish request: " <> Client.printClientError err
        Right { jobId } -> do
          -- Poll until job completes
          job <- Client.pollJob config jobId

          -- If job failed, print logs for debugging
          unless (V1.jobInfo job).success do
            Console.log "Job failed! Logs:"
            let logMessages = map (\l -> "[" <> V1.printLogLevel l.level <> "] " <> l.message) (V1.jobInfo job).logs
            Console.log $ String.joinWith "\n" logMessages

          -- Verify job completed successfully
          when (not (V1.jobInfo job).success) do
            let errorLogs = Array.filter (\l -> l.level == V1.Error) (V1.jobInfo job).logs
            let errorMessages = map _.message errorLogs
            Assert.fail $ "Job failed with errors:\n" <> String.joinWith "\n" errorMessages

          Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust

          -- Test log level filtering
          allLogsResult <- Client.getJob config jobId (Just V1.Debug) Nothing
          case allLogsResult of
            Left err -> Assert.fail $ "Failed to get job with DEBUG level: " <> Client.printClientError err
            Right allLogsJob -> do
              let allLogs = (V1.jobInfo allLogsJob).logs

              infoLogsResult <- Client.getJob config jobId (Just V1.Info) Nothing
              case infoLogsResult of
                Left err -> Assert.fail $ "Failed to get job with INFO level: " <> Client.printClientError err
                Right infoLogsJob -> do
                  let infoLogs = (V1.jobInfo infoLogsJob).logs
                  let debugOnlyLogs = Array.filter (\l -> l.level == V1.Debug) allLogs

                  -- INFO logs should not contain any DEBUG logs
                  let infoContainsDebug = Array.any (\l -> l.level == V1.Debug) infoLogs
                  when infoContainsDebug do
                    Assert.fail "INFO level filter returned DEBUG logs"

                  -- If there were DEBUG logs, INFO result should be smaller
                  when (Array.length debugOnlyLogs > 0) do
                    Assert.shouldSatisfy (Array.length infoLogs) (_ < Array.length allLogs)

          -- Test timestamp filtering
          let logs = (V1.jobInfo job).logs
          when (Array.length logs >= 2) do
            case Array.index logs 0 of
              Nothing -> pure unit
              Just firstLog -> do
                sinceResult <- Client.getJob config jobId (Just V1.Debug) (Just firstLog.timestamp)
                case sinceResult of
                  Left err -> Assert.fail $ "Failed to get job with since filter: " <> Client.printClientError err
                  Right sinceJob -> do
                    let sinceLogs = (V1.jobInfo sinceJob).logs
                    for_ sinceLogs \l ->
                      Assert.shouldSatisfy l.timestamp (_ >= firstLog.timestamp)

    Spec.it "kicks off matrix jobs for effect@4.0.0 once the package is published" do
      config <- getConfig
      maybeJobs <- Client.getJobs config
      case maybeJobs of
        Left err -> Assert.fail $ "Failed to get jobs: " <> Client.printClientError err
        Right jobs -> do
          let
            expectedJobs = initialJobs <>
              [ { jobType: "publish"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Nothing
                , payload: """{"compiler":"0.15.9","location":{"githubOwner":"purescript","githubRepo":"purescript-effect"},"name":"effect","ref":"v4.0.0","version":"4.0.0"}"""
                , success: true
                }
              , { jobType: "matrix"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Just $ unsafeVersion "0.15.10"
                , payload: """{"prelude":"6.0.1"}"""
                , success: true
                }
              , { jobType: "matrix"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Just $ unsafeVersion "0.15.11"
                , payload: """{"prelude":"6.0.1"}"""
                , success: false
                }
              , { jobType: "matrix"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Just $ unsafeVersion "0.15.12"
                , payload: """{"prelude":"6.0.1"}"""
                , success: false
                }
              , { jobType: "matrix"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Just $ unsafeVersion "0.15.13"
                , payload: """{"prelude":"6.0.1"}"""
                , success: false
                }
              , { jobType: "matrix"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Just $ unsafeVersion "0.15.14"
                , payload: """{"prelude":"6.0.1"}"""
                , success: false
                }
              , { jobType: "matrix"
                , packageName: Just $ unsafePackageName "effect"
                , packageVersion: Just $ unsafeVersion "4.0.0"
                , compilerVersion: Just $ unsafeVersion "0.15.15"
                , payload: """{"prelude":"6.0.1"}"""
                , success: false
                }
              ]
          Assert.shouldEqual expectedJobs (map deterministicJob jobs)

type DeterministicJob =
  { jobType :: String
  , packageName :: Maybe PackageName
  , packageVersion :: Maybe Version
  , compilerVersion :: Maybe Version
  , payload :: String
  , success :: Boolean
  }

deterministicJob :: Job -> DeterministicJob
deterministicJob = case _ of
  PublishJob { success, packageName, packageVersion, payload } ->
    { jobType: "publish"
    , packageName: Just packageName
    , packageVersion: Just packageVersion
    , compilerVersion: Nothing
    , success
    , payload: JSON.print $ CJ.encode Operation.publishCodec payload
    }
  UnpublishJob { success, packageName, packageVersion, payload } ->
    { jobType: "unpublish"
    , packageName: Just packageName
    , packageVersion: Just packageVersion
    , compilerVersion: Nothing
    , success
    , payload: JSON.print $ CJ.encode Operation.authenticatedCodec payload
    }
  TransferJob { success, packageName, payload } ->
    { jobType: "transfer"
    , packageName: Just packageName
    , packageVersion: Nothing
    , compilerVersion: Nothing
    , success
    , payload: JSON.print $ CJ.encode Operation.authenticatedCodec payload
    }
  MatrixJob { success, packageName, packageVersion, compilerVersion, payload } ->
    { jobType: "matrix"
    , packageName: Just packageName
    , packageVersion: Just packageVersion
    , compilerVersion: Just compilerVersion
    , success
    , payload: JSON.print $ CJ.encode (Internal.Codec.packageMap Version.codec) payload
    }
  PackageSetJob { success, payload } ->
    { jobType: "packageset"
    , packageName: Nothing
    , packageVersion: Nothing
    , compilerVersion: Nothing
    , success
    , payload: JSON.print $ CJ.encode Operation.packageSetOperationCodec payload
    }

initialJobs :: Array DeterministicJob
initialJobs =
  [ { jobType: "matrix"
    , packageName: Just $ unsafePackageName "prelude"
    , packageVersion: Just $ unsafeVersion "6.0.1"
    , compilerVersion: Just $ unsafeVersion "0.15.15"
    , payload: """{}"""
    , success: true
    }
  , { jobType: "matrix"
    , packageName: Just $ unsafePackageName "type-equality"
    , packageVersion: Just $ unsafeVersion "4.0.1"
    , compilerVersion: Just $ unsafeVersion "0.15.15"
    , payload: """{}"""
    , success: true
    }
  ]
