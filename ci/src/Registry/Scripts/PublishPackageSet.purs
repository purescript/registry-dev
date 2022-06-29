module Registry.Scripts.PublishPackageSet where

import Registry.Prelude

import Data.Array as Array
import Data.DateTime as DateTime
import Data.Int as Int
import Data.Map as Map
import Data.PreciseDateTime as PDT
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Console as Console
import Effect.Now (nowDateTime)
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.Tmp as Tmp
import Node.Process as Node.Process
import Registry.API as API
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.RegistryM (runRegistryM)
import Registry.Schema (LegacyPackageSet(..))
import Registry.Utils (mkLocalEnv, wget)
import Registry.Version (Version)

{-
First, we read the contents of the latest package set release and gather all package versions that have been uploaded to the registry since that release. These package versions are the "batch" of packages that we are considering for automatic inclusion to the next package set.

Second, we filter out any packages where, based on their metadata and manifest files alone, we know they can't be added to the package set. This happens for one of three reasons: they have a dependency that isn't in the package sets, or they had multiple releases since the last package set, in which case we only take the highest version, or c) they already have a higher version number released  in the previous package set. In any of these cases, we remove the package from consideration.
-}

-- 

main :: Effect Unit
main = Aff.launchAff_ do
  liftEffect $ Console.log "Starting package set publishing..."

  packagesMetadataRef <- API.mkMetadataRef

  tmpDir <- liftEffect $ Tmp.mkTmpDir
  liftEffect $ Node.Process.chdir tmpDir

  API.checkIndexExists

  metadata <- liftEffect $ Ref.read packagesMetadataRef
  liftEffect $ Console.log (show metadata)

  now <- liftEffect $ Now.nowDateTime

  let
    newUploads :: Array (Tuple PackageName (Array Version))
    newUploads = do
      Tuple packageName packageMetadata <- Map.toUnfoldable metadata
      let
        versions :: Array Version
        versions = do
          Tuple version { publishedTime } <- Map.toUnfoldable packageMetadata.published
          published <- maybe [] (pure <<< PDT.toDateTimeLossy) (PDT.fromRFC3339String publishedTime)
          let diff = DateTime.diff now published
          guardA (diff <= Hours (Int.toNumber 100)) -- TODO - 24 or 25 hours lookback
          pure version

      guardA (not (Array.null versions))
      pure $ Tuple packageName versions

  liftEffect $ Console.log (show newUploads)

  liftEffect $ Console.log "Fetching latest package set..."

  runRegistryM (mkLocalEnv packagesMetadataRef) do
    wget "https://raw.githubusercontent.com/purescript/package-sets/master/packages.json" "packages.json"

  (packageSetResult :: Either String LegacyPackageSet) <- Json.readJsonFile "packages.json"

  packageSet :: LegacyPackageSet <- case packageSetResult of
    Left err -> unsafeCrashWith err
    Right packageSet -> pure packageSet

  liftEffect $ Console.log (show packageSet)


  pure unit
