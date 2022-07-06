module Registry.Scripts.PublishPackageSet where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Int as Int
import Data.Map as Map
import Data.PreciseDateTime as PDT
import Data.Time.Duration (Hours(..))
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Console as Console
import Effect.Exception (throw)
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.Process as Node.Process
import Node.Process as Process
import Registry.API as API
import Registry.Cache as Cache
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.RegistryM (runRegistryM)
import Registry.Schema (LegacyPackageSet(..), LegacyPackageSetEntry(..))
import Registry.Version (Version)
import Registry.Version as Version

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  liftEffect $ Console.log "Starting package set publishing..."

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken
  cache <- Cache.useCache

  tmpDir <- liftEffect $ Tmp.mkTmpDir
  liftEffect $ Node.Process.chdir tmpDir

  API.fetchRegistryIndex "registry-index"
  _ <- Index.readRegistryIndex "registry-index"

  API.fetchRegistry "registry"
  packagesMetadataRef <- API.mkMetadataRef "registry"

  metadata <- liftEffect $ Ref.read packagesMetadataRef

  now <- liftEffect $ Now.nowDateTime

  let
    -- TODO: Use latest package's `publishedTime` to find new uploads.
    recentUploads :: Array (Tuple PackageName (NonEmptyArray Version))
    recentUploads = do
      Tuple packageName packageMetadata <- Map.toUnfoldable metadata
      let
        versions' :: Array Version
        versions' = do
          Tuple version { publishedTime } <- Map.toUnfoldable packageMetadata.published
          published <- maybe [] (pure <<< PDT.toDateTimeLossy) (PDT.fromRFC3339String publishedTime)
          let diff = DateTime.diff now published
          -- NOTE: Change this line for configurable lookback.
          guardA (diff <= Hours (Int.toNumber 240))
          pure version

      versions <- Array.fromFoldable (NonEmptyArray.fromArray versions')
      pure (Tuple packageName versions)

  liftEffect $ Console.log "Fetching latest package set..."

  runRegistryM (API.mkLocalEnv octokit cache packagesMetadataRef) do
    API.wget "https://raw.githubusercontent.com/purescript/package-sets/master/packages.json" "packages.json"

  packageSetResult :: Either String LegacyPackageSet <- Json.readJsonFile "packages.json"

  LegacyPackageSet packageSet :: LegacyPackageSet <- case packageSetResult of
    Left err -> unsafeCrashWith err
    Right packageSet -> pure packageSet

  liftEffect $ Console.log "Computing candidates for inclusion in package set..."

  let
    uploads :: Array (Tuple PackageName Version)
    uploads = do
      Tuple packageName versions' <- recentUploads
      -- We only care about the latest version
      let version = NonEmptyArray.last (NonEmptyArray.sort versions')
      -- Ensure package is not in package set, or latest version is newer than that in package set
      checkedVersion <- case Map.lookup packageName packageSet of
        Nothing -> pure version
        Just (LegacyPackageSetEntry { version: RawVersion v' })
          | Right v <- Version.parseVersion Version.Lenient v'
          , v < version -> pure version
        _ -> []
      pure (Tuple packageName checkedVersion)

  liftEffect $ Console.log "Found the following uploads eligible for inclusion in package set:"
  liftEffect $ Console.log (show uploads)

  pure unit
