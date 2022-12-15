module Registry.Scripts.Solver where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Exception (throw)
import Effect.Exception as Exception
import Effect.Now (now)
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.Json as Json
import Registry.App.PackageIndex (readManifestIndexFromDisk)
import Registry.App.RegistryM (runRegistryM)
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Scripts.PackageDeleter (deletePackagesCodec)
import Registry.Solver as Solver
import Registry.Version as Version

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
    GitHub.mkOctokit token

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

  -- log $ "Reading registry index2 from tmp/registry-index …"
  -- index2 <- runRegistryM (env { registryIndex = Path.concat [ "tmp", "registry-index" ] }) readManifestIndexFromDisk

  --let registry = map (unwrap >>> _.dependencies) <$> Map.unionWith Map.union (ManifestIndex.toMap index1) (ManifestIndex.toMap index2)
  let registry = map (unwrap >>> _.dependencies) <$> ManifestIndex.toMap index1

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
