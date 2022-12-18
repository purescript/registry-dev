module Registry.Scripts.Solver where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as J
import Data.Codec.Argonaut.Compat as JC
import Data.DateTime.Instant as Instant
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String as String
import Data.These (These(..), these)
import Data.Time.Duration (Milliseconds(..))
import Effect.Exception (throw)
import Effect.Exception as Exception
import Effect.Now (now)
import Effect.Ref as Ref
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.Json as Json
import Registry.App.PackageIndex (readManifestIndexFromDisk)
import Registry.App.RegistryM (runRegistryM)
import Registry.Internal.Codec as Codec
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (GitHubToken(..))
import Registry.Foreign.Octokit as GitHub
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Scripts.PackageDeleter (deletePackagesCodec)
import Registry.Solver as Solver
import Registry.Version as Version

unionWithMapThese :: forall k a b c. Ord k =>
  (k -> These a b -> Maybe c) ->
  Map k a -> Map k b -> Map k c
unionWithMapThese f ma mb =
  let
    combine = case _, _ of
      This a, That b -> Both a b
      That b, This a -> Both a b
      Both a b, _ -> Both a b
      This a, Both _ b -> Both a b
      That b, Both a _ -> Both a b
      That b, That _ -> That b
      This a, This _ -> This a
  in Map.mapMaybeWithKey f $ Map.unionWith combine (This <$> ma) (That <$> mb)

diff ::
  Map PackageName (Map Version (Map PackageName Range)) ->
  Map PackageName (Map Version (Map PackageName Range)) ->
  Map PackageName (Map Version (Map PackageName (These Range Range)))
diff = unionWithMapThese \_ -> filtered <<< these
  do map (map This)
  do map (map That)
  do
    unionWithMapThese \_ -> filtered <<< these
      do map This
      do map That
      do
        unionWithMapThese \_ -> these
          do Just <<< This
          do Just <<< That
          \l r ->
            if l == r then Nothing else Just (Both l r)
  where
  filtered :: forall k v. Ord k => Map k v -> Maybe (Map k v)
  filtered v | Map.isEmpty v = Nothing
  filtered v = Just v

fromThese :: forall a. These a a -> Array (Maybe a)
fromThese = these (\l -> [Just l, Nothing]) (\r -> [Nothing, Just r]) (\l r -> [Just l, Just r])

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ Array.drop 2 <$> Node.Process.argv
  let
    getAction = case args of
      ["--file", path] -> do
        packageversions <- liftAff (Json.readJsonFile deletePackagesCodec path) >>= case _ of
          Left err -> log err *> liftEffect (Process.exit 1)
          Right values -> pure values
        pure \registry -> do
          forWithIndex_ packageversions \package versionsofpackage -> do
            for_ versionsofpackage \version -> do
              let
                deps =
                  case Map.lookup package registry of
                    Nothing -> unsafeCrashWith $ "Missing package: " <> PackageName.print package
                    Just versions ->
                      case Map.lookup version versions of
                        Nothing -> unsafeCrashWith $ "Missing version: " <> PackageName.print package <> "@" <> Version.print version
                        Just d -> d
              test registry package version deps
      ["--all"] -> pure \registry -> do
        forWithIndex_ registry \package versions -> do
          forWithIndex_ versions \version deps -> do
            test registry package version deps
      [package_versionS] | [packageS,versionS] <- String.split (String.Pattern "@") package_versionS ->
        let
          package = unsafeFromRight $ PackageName.parse packageS
          version = unsafeFromRight $ Version.parse versionS
        in pure \registry -> do
          let versions = unsafeFromJust $ Map.lookup package registry
          let deps = unsafeFromJust $ Map.lookup version versions
          test registry package version deps
      _ -> do
        log "Either use --all or package@version"
        liftEffect $ throw $ "Invalid arguments: " <> show args

  let scratchDir = API.scratchDir

  action <- getAction

  log "Reading .env file..."
  _ <- API.loadEnv

  octokit <- liftEffect do
    token <- do
      result <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
      maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) result
    GitHub.newOctokit token

  FS.Extra.ensureDirectory scratchDir

  log $ "Loading cache " <> API.cacheDir <> " …"
  cache <- Cache.useCache API.cacheDir

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    -- copied from LegacyImporter DryRun
    env =
      { comment: \err -> error err
      , closeIssue: log "Skipping GitHub issue closing, this is a dry run..."
      , commitMetadataFile: \_ _ -> do
          log "Skipping committing to registry metadata, this is a dry run..."
          pure (Right unit)
      , commitIndexFile: \_ _ -> do
          log "Skipping committing to registry index, this is a dry run..."
          pure (Right unit)
      , commitPackageSetFile: \_ _ _ -> do
          log "Skipping committing to registry package sets, this is a dry run..."
          pure (Right unit)
      , uploadPackage: \_ _ -> log "Skipping upload, this is a dry run..."
      , deletePackage: \_ -> log "Skipping delete, this is a dry run..."
      , octokit
      , cache
      , username: "NO USERNAME"
      , packagesMetadata: metadataRef
      , registry: Path.concat [ scratchDir, "registry" ]
      , registryIndex: Path.concat [ scratchDir, "registry-index" ]
      }

  log $ "Reading registry index1 from " <> env.registryIndex <> " …"
  index1 <- runRegistryM env readManifestIndexFromDisk

  log $ "Reading registry index2 from tmp/registry-index …"
  index2 <- runRegistryM (env { registryIndex = Path.concat [ "tmp", "registry-index" ] }) readManifestIndexFromDisk

  let reg i = map (unwrap >>> _.dependencies) <$> i
  let index3 = Map.unionWith Map.union (ManifestIndex.toMap index1) (ManifestIndex.toMap index2)
  let registry = reg $ ManifestIndex.toMap index1

  Json.writeJsonFile (Codec.packageMap (Codec.versionMap (Codec.packageMap (J.array (JC.maybe Range.codec))))) "tmp/registry_index_diff.json" (map map map fromThese <$> diff (reg $ ManifestIndex.toMap index2) (reg index3))

  action registry
  where
  test registry package version deps = do
    log $ "%%% Solving " <> PackageName.print package <> "@" <> Version.print version <> " %%%"
    t0 <- liftEffect now
    let r = Solver.solve registry deps
    t1 <- liftEffect now
    let Milliseconds d = Instant.diff t1 t0
    log $ "Took: " <> show d <> "ms"
    case r of
      Right _ -> pure unit
      Left es -> do
        log $ "Failed: " <> PackageName.print package <> "@" <> Version.print version
        log $ String.take 5000 $ foldMap Solver.printSolverError es
