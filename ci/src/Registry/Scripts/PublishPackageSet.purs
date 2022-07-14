module Registry.Scripts.PublishPackageSet where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Int as Int
import Data.Map as Map
import Data.PreciseDateTime as PDT
import Data.String as String
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
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Registry.API as API
import Registry.Cache as Cache
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.RegistryM (Env, readPackagesMetadata, runRegistryM)
import Registry.Schema (PackageSet(..))
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

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env :: Env
    env =
      -- TODO: Do we need to comment?
      { comment: mempty
      , closeIssue: mempty
      , commitMetadataFile: \_ _ -> pure (Right unit)
      , commitIndexFile: \_ _ -> pure (Right unit)
      , uploadPackage: mempty
      , deletePackage: mempty
      , octokit
      , cache
      , packagesMetadata: metadataRef
      , registry: "registry"
      , registryIndex: "registry-index"
      }

  runRegistryM env do
    API.fetchRegistryIndex
    API.fetchRegistry
    API.fillMetadataRef

    metadata <- readPackagesMetadata

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

    -- Read package set directory from "registry" - FS.Aff.readDir
    -- Trim JSON extension, parse versions, sort, take highest version
    -- Check purescript-formatters for formatDateTime
    -- Read that file as `PackageSet` - Date (YYYY-MM-DD, add newtype around DateTime, format as YYYY-MM-DD), not RFC3339String - to compare: date, then version
    --    encode :: turn string to datetime
    --    decode :: turn string to datetime, then print

    packageSets <- liftAff $ FS.Aff.readdir (Path.concat [ "registry", "package-sets" ])

    let
      packageSetVersions :: Array Version
      packageSetVersions = packageSets # Array.mapMaybe \s -> do 
        let versionString = String.take (String.length s - 5) s
        hush $ Version.parseVersion Version.Lenient versionString

      latestPackageSet :: Maybe FilePath
      latestPackageSet = do
        latestVersion <- Array.last (Array.sort packageSetVersions)
        pure $ Path.concat
          [ "registry"
          , "package-sets"
          , Version.printVersion latestVersion <> ".json"
          ]

    packageSetPath :: FilePath <- case latestPackageSet of
      Nothing -> unsafeCrashWith "ERROR: No existing package set."
      Just packageSetPath -> pure packageSetPath
    
    packageSetResult :: Either String PackageSet <- liftAff $ Json.readJsonFile packageSetPath

    PackageSet { packages } :: PackageSet <- case packageSetResult of
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
        checkedVersion <- case Map.lookup packageName packages of
          Nothing -> pure version
          Just v | v < version -> pure version
          _ -> []
        pure (Tuple packageName checkedVersion)

    liftEffect $ Console.log "Found the following uploads eligible for inclusion in package set:"
    liftEffect $ Console.log (show uploads)

    -- NOTE:
    --    - Compute eligible uploads
    --    - Compute batches via dependencies
    --    - Make sure dependencies are in the package set unioned with the batch


    -- Determine semver 

    pure unit
