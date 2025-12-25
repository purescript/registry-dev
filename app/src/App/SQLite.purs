-- | Bindings for the specific SQL queries we emit to the SQLite database. Use the
-- | Registry.App.Effect.Db module in production code instead of this module;
-- | the bindings here are still quite low-level and simply exist to provide a
-- | nicer interface with PureScript types for higher-level modules to use.

module Registry.App.SQLite
  ( ConnectOptions
  , FinishJob
  , InsertMatrixJob
  , InsertPackageSetJob
  , InsertPublishJob
  , InsertTransferJob
  , InsertUnpublishJob
  , JobInfo
  , MatrixJobDetails
  , PackageSetJobDetails
  , PublishJobDetails
  , SQLite
  , SelectJobRequest
  , SelectJobsRequest
  , StartJob
  , TransferJobDetails
  , UnpublishJobDetails
  , connect
  , finishJob
  , insertLogLine
  , insertMatrixJob
  , insertPackageSetJob
  , insertPublishJob
  , insertTransferJob
  , insertUnpublishJob
  , resetIncompleteJobs
  , selectJob
  , selectJobs
  , selectLogsByJob
  , selectNextMatrixJob
  , selectNextPackageSetJob
  , selectNextPublishJob
  , selectNextTransferJob
  , selectNextUnpublishJob
  , selectPublishJob
  , selectTransferJob
  , selectUnpublishJob
  , startJob
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as JSON.DecodeError
import Control.Monad.Except (runExceptT)
import Data.Array (sortBy, take)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTime
import Data.Function (on)
import Data.Nullable (notNull, null)
import Data.Nullable as Nullable
import Data.String as String
import Data.UUID.Random as UUID
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Effect.Uncurried as Uncurried
import Record as Record
import Registry.API.V1 (Job(..), JobId(..), LogLevel(..), LogLine)
import Registry.API.V1 as API.V1
import Registry.API.V1 as V1
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Operation (AuthenticatedData, PackageSetOperation, PublishData, TransferData, UnpublishData)
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.SSH (Signature)
import Registry.Version as Version

-- | An active database connection acquired with `connect`
data SQLite

foreign import connectImpl :: EffectFn2 FilePath (EffectFn1 String Unit) SQLite

type ConnectOptions =
  { database :: FilePath
  , logger :: String -> Effect Unit
  }

-- Connect to the indicated SQLite database
connect :: ConnectOptions -> Effect SQLite
connect { database, logger } = Uncurried.runEffectFn2 connectImpl database (Uncurried.mkEffectFn1 logger)

--------------------------------------------------------------------------------
-- job_info table

-- | Metadata about a particular package, package set, or matrix job.
type JobInfo =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  }

type JSJobInfo =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  }

-- jobInfoFromJSRep :: JSJobInfo -> Either String JobInfo
-- jobInfoFromJSRep { jobId, createdAt, startedAt, finishedAt, success } = do
--   created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
--   started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
--   finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
--   isSuccess <- toSuccess success
--   pure
--     { jobId: JobId jobId
--     , createdAt: created
--     , startedAt: started
--     , finishedAt: finished
--     , success: isSuccess
--     }

foreign import selectJobInfoImpl :: EffectFn2 SQLite String (Nullable JSJobInfo)

-- selectJobInfo :: SQLite -> JobId -> Effect (Either String (Maybe JobInfo))
-- selectJobInfo db (JobId jobId) = do
--   maybeJobInfo <- map toMaybe $ Uncurried.runEffectFn2 selectJobInfoImpl db jobId
--   pure $ traverse jobInfoFromJSRep maybeJobInfo

finishJob :: SQLite -> FinishJob -> Effect Unit
finishJob db = Uncurried.runEffectFn2 finishJobImpl db <<< finishJobToJSRep

type StartJob =
  { jobId :: JobId
  , startedAt :: DateTime
  }

type JSStartJob =
  { jobId :: String
  , startedAt :: String
  }

startJobToJSRep :: StartJob -> JSStartJob
startJobToJSRep { jobId, startedAt } =
  { jobId: un JobId jobId
  , startedAt: DateTime.format Internal.Format.iso8601DateTime startedAt
  }

foreign import startJobImpl :: EffectFn2 SQLite JSStartJob Unit

startJob :: SQLite -> StartJob -> Effect Unit
startJob db = Uncurried.runEffectFn2 startJobImpl db <<< startJobToJSRep

type FinishJob =
  { jobId :: JobId
  , success :: Boolean
  , finishedAt :: DateTime
  }

type JSFinishJob =
  { jobId :: String
  , success :: Int
  , finishedAt :: String
  }

finishJobToJSRep :: FinishJob -> JSFinishJob
finishJobToJSRep { jobId, success, finishedAt } =
  { jobId: un JobId jobId
  , success: fromSuccess success
  , finishedAt: DateTime.format Internal.Format.iso8601DateTime finishedAt
  }

foreign import finishJobImpl :: EffectFn2 SQLite JSFinishJob Unit

foreign import resetIncompleteJobsImpl :: EffectFn1 SQLite Unit

resetIncompleteJobs :: SQLite -> Effect Unit
resetIncompleteJobs = Uncurried.runEffectFn1 resetIncompleteJobsImpl

newJobId :: forall m. MonadEffect m => m JobId
newJobId = do
  id <- UUID.make
  pure $ JobId $ UUID.toString id

fromSuccess :: Boolean -> Int
fromSuccess success = if success then 1 else 0

toSuccess :: Int -> Either String Boolean
toSuccess success = case success of
  0 -> Right false
  1 -> Right true
  _ -> Left $ "Invalid success value " <> show success

type SelectJobRequest =
  { level :: Maybe LogLevel
  , since :: DateTime
  , jobId :: JobId
  }

selectJob :: SQLite -> SelectJobRequest -> Effect (Either String (Maybe Job))
selectJob db { level: maybeLogLevel, since, jobId: JobId jobId } = do
  let logLevel = fromMaybe Error maybeLogLevel
  { fail, success: logs } <- selectLogsByJob db (JobId jobId) logLevel since
  case fail of
    [] -> runExceptT $ firstJust
      [ selectPublishJobById logs
      , selectMatrixJobById logs
      , selectTransferJobById logs
      , selectPackageSetJobById logs
      , selectUnpublishJobById logs
      ]
    _ -> pure $ Left $ "Some logs are not readable: " <> String.joinWith "\n" fail
  where
  firstJust :: Array (ExceptT String Effect (Maybe Job)) -> ExceptT String Effect (Maybe Job)
  firstJust = Array.foldl go (pure Nothing)
    where
    go acc next = acc >>= case _ of
      Just job -> pure (Just job)
      Nothing -> next

  selectPublishJobById logs = ExceptT do
    maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectPublishJobImpl db
      { jobId: notNull jobId, packageName: null, packageVersion: null }
    pure $ traverse
      ( map (PublishJob <<< Record.merge { logs, jobType: Proxy :: _ "publish" })
          <<< publishJobDetailsFromJSRep
      )
      maybeJobDetails

  selectUnpublishJobById logs = ExceptT do
    maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectUnpublishJobImpl db
      { jobId: notNull jobId, packageName: null, packageVersion: null }
    pure $ traverse
      ( map (UnpublishJob <<< Record.merge { logs, jobType: Proxy :: _ "unpublish" })
          <<< unpublishJobDetailsFromJSRep
      )
      maybeJobDetails

  selectTransferJobById logs = ExceptT do
    maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectTransferJobImpl db
      { jobId: notNull jobId, packageName: null }
    pure $ traverse
      ( map (TransferJob <<< Record.merge { logs, jobType: Proxy :: _ "transfer" })
          <<< transferJobDetailsFromJSRep
      )
      maybeJobDetails

  selectMatrixJobById logs = ExceptT do
    maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectMatrixJobImpl db (Nullable.notNull jobId)
    pure $ traverse
      ( map (MatrixJob <<< Record.merge { logs, jobType: Proxy :: _ "matrix" })
          <<< matrixJobDetailsFromJSRep
      )
      maybeJobDetails

  selectPackageSetJobById logs = ExceptT do
    maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectPackageSetJobImpl db (Nullable.notNull jobId)
    pure $ traverse
      ( map (PackageSetJob <<< Record.merge { logs, jobType: Proxy :: _ "packageset" })
          <<< packageSetJobDetailsFromJSRep
      )
      maybeJobDetails

type SelectJobsRequest =
  { since :: DateTime
  , includeCompleted :: Boolean
  }

selectJobs :: SQLite -> SelectJobsRequest -> Effect { failed :: Array String, jobs :: Array Job }
selectJobs db { since, includeCompleted } = do
  publishJobs <- selectPublishJobs
  unpublishJobs <- selectUnpublishJobs
  transferJobs <- selectTransferJobs
  matrixJobs <- selectMatrixJobs
  packageSetJobs <- selectPackageSetJobs
  let
    { fail: failedJobs, success: allJobs } = partitionEithers
      (publishJobs <> unpublishJobs <> transferJobs <> matrixJobs <> packageSetJobs)
  pure { failed: failedJobs, jobs: take 100 $ sortBy (compare `on` (V1.jobInfo >>> _.createdAt)) allJobs }

  where
  selectPublishJobs = do
    jobs <- Uncurried.runEffectFn3 selectPublishJobsImpl db (DateTime.format Internal.Format.iso8601DateTime since) includeCompleted
    pure $ map (map (PublishJob <<< Record.merge { logs: [], jobType: Proxy :: _ "publish" }) <<< publishJobDetailsFromJSRep) jobs

  selectUnpublishJobs = do
    jobs <- Uncurried.runEffectFn3 selectUnpublishJobsImpl db (DateTime.format Internal.Format.iso8601DateTime since) includeCompleted
    pure $ map (map (UnpublishJob <<< Record.merge { logs: [], jobType: Proxy :: _ "unpublish" }) <<< unpublishJobDetailsFromJSRep) jobs

  selectTransferJobs = do
    jobs <- Uncurried.runEffectFn3 selectTransferJobsImpl db (DateTime.format Internal.Format.iso8601DateTime since) includeCompleted
    pure $ map (map (TransferJob <<< Record.merge { logs: [], jobType: Proxy :: _ "transfer" }) <<< transferJobDetailsFromJSRep) jobs

  selectMatrixJobs = do
    jobs <- Uncurried.runEffectFn3 selectMatrixJobsImpl db (DateTime.format Internal.Format.iso8601DateTime since) includeCompleted
    pure $ map (map (MatrixJob <<< Record.merge { logs: [], jobType: Proxy :: _ "matrix" }) <<< matrixJobDetailsFromJSRep) jobs

  selectPackageSetJobs = do
    jobs <- Uncurried.runEffectFn3 selectPackageSetJobsImpl db (DateTime.format Internal.Format.iso8601DateTime since) includeCompleted
    pure $ map (map (PackageSetJob <<< Record.merge { logs: [], jobType: Proxy :: _ "packageset" }) <<< packageSetJobDetailsFromJSRep) jobs

--------------------------------------------------------------------------------
-- publish_jobs table

type PublishJobDetails =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , packageName :: PackageName
  , packageVersion :: Version
  , payload :: PublishData
  }

type JSPublishJobDetails =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  , packageName :: String
  , packageVersion :: String
  , payload :: String
  }

publishJobDetailsFromJSRep :: JSPublishJobDetails -> Either String PublishJobDetails
publishJobDetailsFromJSRep { jobId, packageName, packageVersion, payload, createdAt, startedAt, finishedAt, success } = do
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
  s <- toSuccess success
  name <- PackageName.parse packageName
  version <- Version.parse packageVersion
  parsed <- lmap JSON.DecodeError.print $ parseJson Operation.publishCodec payload
  pure
    { jobId: JobId jobId
    , createdAt: created
    , startedAt: started
    , finishedAt: finished
    , success: s
    , packageName: name
    , packageVersion: version
    , payload: parsed
    }

type SelectPublishParams =
  { jobId :: Nullable String
  , packageName :: Nullable String
  , packageVersion :: Nullable String
  }

foreign import selectPublishJobImpl :: EffectFn2 SQLite SelectPublishParams (Nullable JSPublishJobDetails)

foreign import selectPublishJobsImpl :: EffectFn3 SQLite String Boolean (Array JSPublishJobDetails)

selectNextPublishJob :: SQLite -> Effect (Either String (Maybe PublishJobDetails))
selectNextPublishJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectPublishJobImpl db { jobId: null, packageName: null, packageVersion: null }
  pure $ traverse publishJobDetailsFromJSRep maybeJobDetails

selectPublishJob :: SQLite -> PackageName -> Version -> Effect (Either String (Maybe PublishJobDetails))
selectPublishJob db packageName packageVersion = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectPublishJobImpl db
    { jobId: null
    , packageName: notNull $ PackageName.print packageName
    , packageVersion: notNull $ Version.print packageVersion
    }
  pure $ traverse publishJobDetailsFromJSRep maybeJobDetails

type InsertPublishJob =
  { payload :: PublishData
  }

type JSInsertPublishJob =
  { jobId :: String
  , packageName :: String
  , packageVersion :: String
  , payload :: String
  , createdAt :: String
  }

insertPublishJobToJSRep :: JobId -> DateTime -> InsertPublishJob -> JSInsertPublishJob
insertPublishJobToJSRep jobId now { payload } =
  { jobId: un JobId jobId
  , packageName: PackageName.print payload.name
  , packageVersion: Version.print payload.version
  , payload: stringifyJson Operation.publishCodec payload
  , createdAt: DateTime.format Internal.Format.iso8601DateTime now
  }

foreign import insertPublishJobImpl :: EffectFn2 SQLite JSInsertPublishJob Unit

-- | Insert a new package job, ie. a publish, unpublish, or transfer.
insertPublishJob :: SQLite -> InsertPublishJob -> Effect JobId
insertPublishJob db job = do
  jobId <- newJobId
  now <- nowUTC
  Uncurried.runEffectFn2 insertPublishJobImpl db $ insertPublishJobToJSRep jobId now job
  pure jobId

--------------------------------------------------------------------------------
-- unpublish_jobs table

type UnpublishJobDetails =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , packageName :: PackageName
  , packageVersion :: Version
  , payload :: AuthenticatedData
  }

type JSUnpublishJobDetails =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  , packageName :: String
  , packageVersion :: String
  , payload :: String
  }

unpublishJobDetailsFromJSRep :: JSUnpublishJobDetails -> Either String UnpublishJobDetails
unpublishJobDetailsFromJSRep { jobId, packageName, packageVersion, payload, createdAt, startedAt, finishedAt, success } = do
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
  s <- toSuccess success
  name <- PackageName.parse packageName
  version <- Version.parse packageVersion
  parsed <- lmap JSON.DecodeError.print $ parseJson Operation.authenticatedCodec payload
  pure
    { jobId: JobId jobId
    , createdAt: created
    , startedAt: started
    , finishedAt: finished
    , success: s
    , packageName: name
    , packageVersion: version
    , payload: parsed
    }

type SelectUnpublishParams =
  { jobId :: Nullable String
  , packageName :: Nullable String
  , packageVersion :: Nullable String
  }

foreign import selectUnpublishJobImpl :: EffectFn2 SQLite SelectUnpublishParams (Nullable JSUnpublishJobDetails)

foreign import selectUnpublishJobsImpl :: EffectFn3 SQLite String Boolean (Array JSUnpublishJobDetails)

selectNextUnpublishJob :: SQLite -> Effect (Either String (Maybe UnpublishJobDetails))
selectNextUnpublishJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectUnpublishJobImpl db { jobId: null, packageName: null, packageVersion: null }
  pure $ traverse unpublishJobDetailsFromJSRep maybeJobDetails

selectUnpublishJob :: SQLite -> PackageName -> Version -> Effect (Either String (Maybe UnpublishJobDetails))
selectUnpublishJob db packageName packageVersion = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectUnpublishJobImpl db
    { jobId: null
    , packageName: notNull $ PackageName.print packageName
    , packageVersion: notNull $ Version.print packageVersion
    }
  pure $ traverse unpublishJobDetailsFromJSRep maybeJobDetails

type InsertUnpublishJob =
  { payload :: UnpublishData
  , rawPayload :: String
  , signature :: Signature
  }

type JSInsertUnpublishJob =
  { jobId :: String
  , packageName :: String
  , packageVersion :: String
  , payload :: String
  , createdAt :: String
  }

insertUnpublishJobToJSRep :: JobId -> DateTime -> InsertUnpublishJob -> JSInsertUnpublishJob
insertUnpublishJobToJSRep jobId now { payload, rawPayload, signature } =
  { jobId: un JobId jobId
  , packageName: PackageName.print payload.name
  , packageVersion: Version.print payload.version
  , payload: stringifyJson Operation.authenticatedCodec
      { payload: Operation.Unpublish payload
      , rawPayload
      , signature
      }
  , createdAt: DateTime.format Internal.Format.iso8601DateTime now
  }

foreign import insertUnpublishJobImpl :: EffectFn2 SQLite JSInsertUnpublishJob Unit

-- | Insert a new package job, ie. a publish, unpublish, or transfer.
insertUnpublishJob :: SQLite -> InsertUnpublishJob -> Effect JobId
insertUnpublishJob db job = do
  jobId <- newJobId
  now <- nowUTC
  Uncurried.runEffectFn2 insertUnpublishJobImpl db $ insertUnpublishJobToJSRep jobId now job
  pure jobId

--------------------------------------------------------------------------------
-- transfer_jobs table

type TransferJobDetails =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , packageName :: PackageName
  , payload :: AuthenticatedData
  }

type JSTransferJobDetails =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  , packageName :: String
  , payload :: String
  }

transferJobDetailsFromJSRep :: JSTransferJobDetails -> Either String TransferJobDetails
transferJobDetailsFromJSRep { jobId, packageName, payload, createdAt, startedAt, finishedAt, success } = do
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
  s <- toSuccess success
  name <- PackageName.parse packageName
  parsed <- lmap JSON.DecodeError.print $ parseJson Operation.authenticatedCodec payload
  pure
    { jobId: JobId jobId
    , createdAt: created
    , startedAt: started
    , finishedAt: finished
    , success: s
    , packageName: name
    , payload: parsed
    }

type SelectTransferParams = { jobId :: Nullable String, packageName :: Nullable String }

foreign import selectTransferJobImpl :: EffectFn2 SQLite SelectTransferParams (Nullable JSTransferJobDetails)

foreign import selectTransferJobsImpl :: EffectFn3 SQLite String Boolean (Array JSTransferJobDetails)

selectNextTransferJob :: SQLite -> Effect (Either String (Maybe TransferJobDetails))
selectNextTransferJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectTransferJobImpl db { jobId: null, packageName: null }
  pure $ traverse transferJobDetailsFromJSRep maybeJobDetails

selectTransferJob :: SQLite -> PackageName -> Effect (Either String (Maybe TransferJobDetails))
selectTransferJob db packageName = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectTransferJobImpl db
    { jobId: null
    , packageName: notNull $ PackageName.print packageName
    }
  pure $ traverse transferJobDetailsFromJSRep maybeJobDetails

type InsertTransferJob =
  { payload :: TransferData
  , rawPayload :: String
  , signature :: Signature
  }

type JSInsertTransferJob =
  { jobId :: String
  , packageName :: String
  , payload :: String
  , createdAt :: String
  }

insertTransferJobToJSRep :: JobId -> DateTime -> InsertTransferJob -> JSInsertTransferJob
insertTransferJobToJSRep jobId now { payload, rawPayload, signature } =
  { jobId: un JobId jobId
  , packageName: PackageName.print payload.name
  , payload: stringifyJson Operation.authenticatedCodec
      { payload: Operation.Transfer payload, rawPayload, signature }
  , createdAt: DateTime.format Internal.Format.iso8601DateTime now
  }

foreign import insertTransferJobImpl :: EffectFn2 SQLite JSInsertTransferJob Unit

-- | Insert a new package job, ie. a publish, unpublish, or transfer.
insertTransferJob :: SQLite -> InsertTransferJob -> Effect JobId
insertTransferJob db job = do
  jobId <- newJobId
  now <- nowUTC
  Uncurried.runEffectFn2 insertTransferJobImpl db $ insertTransferJobToJSRep jobId now job
  pure jobId

--------------------------------------------------------------------------------
-- matrix_jobs table

type InsertMatrixJob =
  { packageName :: PackageName
  , packageVersion :: Version
  , compilerVersion :: Version
  , payload :: Map PackageName Version
  }

type JSInsertMatrixJob =
  { jobId :: String
  , createdAt :: String
  , packageName :: String
  , packageVersion :: String
  , compilerVersion :: String
  , payload :: String
  }

insertMatrixJobToJSRep :: JobId -> DateTime -> InsertMatrixJob -> JSInsertMatrixJob
insertMatrixJobToJSRep jobId now { packageName, packageVersion, compilerVersion, payload } =
  { jobId: un JobId jobId
  , createdAt: DateTime.format Internal.Format.iso8601DateTime now
  , packageName: PackageName.print packageName
  , packageVersion: Version.print packageVersion
  , compilerVersion: Version.print compilerVersion
  , payload: stringifyJson (Internal.Codec.packageMap Version.codec) payload
  }

foreign import insertMatrixJobImpl :: EffectFn2 SQLite JSInsertMatrixJob Unit

insertMatrixJob :: SQLite -> InsertMatrixJob -> Effect JobId
insertMatrixJob db job = do
  jobId <- newJobId
  now <- nowUTC
  Uncurried.runEffectFn2 insertMatrixJobImpl db $ insertMatrixJobToJSRep jobId now job
  pure jobId

type MatrixJobDetails =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , packageName :: PackageName
  , packageVersion :: Version
  , compilerVersion :: Version
  , payload :: Map PackageName Version
  }

type JSMatrixJobDetails =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  , packageName :: String
  , packageVersion :: String
  , compilerVersion :: String
  , payload :: String
  }

matrixJobDetailsFromJSRep :: JSMatrixJobDetails -> Either String MatrixJobDetails
matrixJobDetailsFromJSRep { jobId, packageName, packageVersion, compilerVersion, payload, createdAt, startedAt, finishedAt, success } = do
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
  s <- toSuccess success
  name <- PackageName.parse packageName
  version <- Version.parse packageVersion
  compiler <- Version.parse compilerVersion
  parsed <- lmap JSON.DecodeError.print $ parseJson (Internal.Codec.packageMap Version.codec) payload
  pure
    { jobId: JobId jobId
    , createdAt: created
    , startedAt: started
    , finishedAt: finished
    , success: s
    , packageName: name
    , packageVersion: version
    , compilerVersion: compiler
    , payload: parsed
    }

foreign import selectMatrixJobImpl :: EffectFn2 SQLite (Nullable String) (Nullable JSMatrixJobDetails)

foreign import selectMatrixJobsImpl :: EffectFn3 SQLite String Boolean (Array JSMatrixJobDetails)

selectNextMatrixJob :: SQLite -> Effect (Either String (Maybe MatrixJobDetails))
selectNextMatrixJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectMatrixJobImpl db Nullable.null
  pure $ traverse matrixJobDetailsFromJSRep maybeJobDetails

--------------------------------------------------------------------------------
-- package_set_jobs table

type PackageSetJobDetails =
  { jobId :: JobId
  , createdAt :: DateTime
  , startedAt :: Maybe DateTime
  , finishedAt :: Maybe DateTime
  , success :: Boolean
  , payload :: PackageSetOperation
  }

type JSPackageSetJobDetails =
  { jobId :: String
  , createdAt :: String
  , startedAt :: Nullable String
  , finishedAt :: Nullable String
  , success :: Int
  , payload :: String
  }

packageSetJobDetailsFromJSRep :: JSPackageSetJobDetails -> Either String PackageSetJobDetails
packageSetJobDetailsFromJSRep { jobId, payload, createdAt, startedAt, finishedAt, success } = do
  created <- DateTime.unformat Internal.Format.iso8601DateTime createdAt
  started <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe startedAt)
  finished <- traverse (DateTime.unformat Internal.Format.iso8601DateTime) (toMaybe finishedAt)
  s <- toSuccess success
  parsed <- lmap JSON.DecodeError.print $ parseJson Operation.packageSetOperationCodec payload
  pure
    { jobId: JobId jobId
    , createdAt: created
    , startedAt: started
    , finishedAt: finished
    , success: s
    , payload: parsed
    }

foreign import selectPackageSetJobImpl :: EffectFn2 SQLite (Nullable String) (Nullable JSPackageSetJobDetails)

foreign import selectPackageSetJobsImpl :: EffectFn3 SQLite String Boolean (Array JSPackageSetJobDetails)

selectNextPackageSetJob :: SQLite -> Effect (Either String (Maybe PackageSetJobDetails))
selectNextPackageSetJob db = do
  maybeJobDetails <- map toMaybe $ Uncurried.runEffectFn2 selectPackageSetJobImpl db Nullable.null
  pure $ traverse packageSetJobDetailsFromJSRep maybeJobDetails

type InsertPackageSetJob =
  { payload :: PackageSetOperation
  }

type JSInsertPackageSetJob =
  { jobId :: String
  , createdAt :: String
  , payload :: String
  }

insertPackageSetJobToJSRep :: JobId -> DateTime -> InsertPackageSetJob -> JSInsertPackageSetJob
insertPackageSetJobToJSRep jobId now { payload } =
  { jobId: un JobId jobId
  , createdAt: DateTime.format Internal.Format.iso8601DateTime now
  , payload: stringifyJson Operation.packageSetOperationCodec payload
  }

foreign import insertPackageSetJobImpl :: EffectFn2 SQLite JSInsertPackageSetJob Unit

insertPackageSetJob :: SQLite -> InsertPackageSetJob -> Effect JobId
insertPackageSetJob db job = do
  jobId <- newJobId
  now <- nowUTC
  Uncurried.runEffectFn2 insertPackageSetJobImpl db $ insertPackageSetJobToJSRep jobId now job
  pure jobId

--------------------------------------------------------------------------------
-- logs table

type JSLogLine =
  { level :: Int
  , message :: String
  , jobId :: String
  , timestamp :: String
  }

logLineToJSRep :: LogLine -> JSLogLine
logLineToJSRep { level, message, jobId, timestamp } =
  { level: API.V1.logLevelToPriority level
  , message
  , jobId: un JobId jobId
  , timestamp: DateTime.format Internal.Format.iso8601DateTime timestamp
  }

logLineFromJSRep :: JSLogLine -> Either String LogLine
logLineFromJSRep { level, message, jobId, timestamp } = do
  logLevel <- API.V1.logLevelFromPriority level
  time <- DateTime.unformat Internal.Format.iso8601DateTime timestamp
  pure
    { level: logLevel
    , message
    , jobId: JobId jobId
    , timestamp: time
    }

foreign import insertLogLineImpl :: EffectFn2 SQLite JSLogLine Unit

insertLogLine :: SQLite -> LogLine -> Effect Unit
insertLogLine db = Uncurried.runEffectFn2 insertLogLineImpl db <<< logLineToJSRep

foreign import selectLogsByJobImpl :: EffectFn4 SQLite String Int String (Array JSLogLine)

-- | Select all logs for a given job at or above the indicated log level. To get all
-- | logs, pass the DEBUG log level.
selectLogsByJob :: SQLite -> JobId -> LogLevel -> DateTime -> Effect { fail :: Array String, success :: Array LogLine }
selectLogsByJob db jobId level since = do
  let timestamp = DateTime.format Internal.Format.iso8601DateTime since
  jsLogLines <-
    Uncurried.runEffectFn4
      selectLogsByJobImpl
      db
      (un JobId jobId)
      (API.V1.logLevelToPriority level)
      timestamp
  pure $ partitionEithers $ map logLineFromJSRep jsLogLines
