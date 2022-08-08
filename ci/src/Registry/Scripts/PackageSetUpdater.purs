module Registry.Scripts.PackageSetUpdater where

import Registry.Prelude

import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Map as Map
import Data.PreciseDateTime as PDT
import Data.String as String
import Data.Time.Duration (Hours(..))
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.Process as Node.Process
import Node.Process as Process
import Registry.API as API
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.RegistryM (Env, RegistryM, commitPackageSetFile, readPackagesMetadata, runRegistryM, throwWithComment)
import Registry.Schema (PackageSet(..))
import Registry.Version (Version)
import Registry.Version as Version

data PublishMode = GeneratePackageSet | CommitPackageSet

derive instance Eq PublishMode

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  log "Parsing CLI args..."
  mode <- liftEffect do
    args <- Array.drop 2 <$> Node.Process.argv
    case Array.uncons args of
      Nothing -> Exception.throw "Expected 'generate' or 'commit', but received no arguments."
      Just { head, tail: [] } -> case head of
        "generate" -> pure GeneratePackageSet
        "commit" -> pure CommitPackageSet
        other -> Exception.throw $ "Expected 'generate' or 'commit' but received: " <> other
      Just _ -> Exception.throw $ String.joinWith "\n"
        [ "Expected 'generate' or 'commit', but received multiple arguments:"
        , String.joinWith " " args
        ]

  log "Starting package set publishing..."

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (Exception.throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken

  tmpDir <- liftEffect $ Tmp.mkTmpDir
  liftEffect $ Node.Process.chdir tmpDir

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env :: Env
    env =
      { comment: \comment -> log ("[COMMENT] " <> comment)
      , closeIssue: mempty
      , commitMetadataFile: \_ _ -> pure (Right unit)
      , commitIndexFile: \_ _ -> pure (Right unit)
      , commitPackageSetFile: API.pacchettiBottiPushToRegistryPackageSets
      , uploadPackage: mempty
      , deletePackage: mempty
      , octokit
      , cache: { write: mempty, read: \_ -> pure (Left mempty), remove: mempty }
      , username: mempty
      , packagesMetadata: metadataRef
      , registry: "registry"
      , registryIndex: "registry-index"
      }

  runRegistryM env do
    API.fetchRegistryIndex
    API.fetchRegistry
    API.fillMetadataRef

    registryIndexPath <- asks _.registryIndex
    registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath

    prevPackageSet <- PackageSet.readLatestPackageSet
    recentUploads <- findRecentUploads (Hours 24.0)

    let candidates = PackageSet.validatePackageSetCandidates registryIndex prevPackageSet (map Just recentUploads.accepted)
    log $ PackageSet.printRejections candidates.rejected

    if Map.isEmpty candidates.accepted then do
      log "No eligible additions, updates, or removals to produce a new package set."
    else do
      let
        logPackage name maybeVersion = case maybeVersion of
          -- There are no removals in the automated package sets. This should be
          -- an unreachable case.
          Nothing -> throwWithComment "Package removals are not accepted in automatic package sets."
          Just version -> log (PackageName.print name <> "@" <> Version.printVersion version)

      log "Found the following package versions eligible for inclusion in package set:"
      forWithIndex_ candidates.accepted logPackage
      PackageSet.processBatchSequential registryIndex prevPackageSet Nothing candidates.accepted >>= case _ of
        Nothing -> do
          log "\n----------\nNo packages could be added to the set. All packages failed:"
          forWithIndex_ candidates.accepted logPackage
        Just { success, fail, packageSet } -> do
          unless (Map.isEmpty fail) do
            log "\n----------\nSome packages could not be added to the set:"
            forWithIndex_ fail logPackage
          log "\n----------\nNew packages were added to the set!"
          forWithIndex_ success logPackage
          newPath <- PackageSet.getPackageSetPath (un PackageSet packageSet).version
          liftAff $ Json.writeJsonFile newPath packageSet
          case mode of
            GeneratePackageSet -> pure unit
            CommitPackageSet -> do
              let commitMessage = PackageSet.commitMessage prevPackageSet success
              commitPackageSetFile (un PackageSet packageSet).version commitMessage >>= case _ of
                Left err -> throwWithComment $ "Failed to commit package set file: " <> err
                Right _ -> pure unit

findRecentUploads :: Hours -> RegistryM { accepted :: Map PackageName Version, rejected :: Map PackageName (NonEmptyArray Version) }
findRecentUploads limit = do
  metadata <- readPackagesMetadata
  now <- liftEffect Now.nowDateTime

  let
    packageUploads = Map.fromFoldable do
      Tuple packageName packageMetadata <- Map.toUnfoldable metadata
      versions <- Array.fromFoldable $ NonEmptyArray.fromArray do
        Tuple version { publishedTime } <- Map.toUnfoldable packageMetadata.published
        published <- maybe [] (pure <<< PDT.toDateTimeLossy) (PDT.fromRFC3339String publishedTime)
        let diff = DateTime.diff now published
        guardA (diff <= limit)
        pure version
      pure (Tuple packageName versions)

    deduplicated = packageUploads # flip foldlWithIndex { rejected: Map.empty, accepted: Map.empty } \name acc versions -> do
      let { init, last } = NonEmptyArray.unsnoc versions
      case NonEmptyArray.fromArray init of
        Nothing -> acc { accepted = Map.insert name last acc.accepted }
        Just entries -> acc { accepted = Map.insert name last acc.accepted, rejected = Map.insert name entries acc.rejected }

  pure deduplicated
