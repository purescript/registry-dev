-- | Script to run the version solver against the registry-index (in ./scratch/)
-- | in various ways:
-- |
-- | - `--manifest foo.json` specifies a manifest with custom bounds
-- |   of the form `{ "prelude": ">=1.0.0 <2.0.0", "console": "1.3.1" }`
-- | - Other modes solve one or more existing manifests from the registry:
-- |   - `--all` solves all manifests in the registry
-- |   - `--file packagesversions.json` solves a map `{ "package": ["1.0.0"] }`
-- |   - `package@version` solves a single package version from the registry
module Registry.Scripts.Solver where

import Registry.App.Prelude

import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Codec.Argonaut as J
import Data.Codec.Argonaut.Compat as JC
import Data.DateTime.Instant as Instant
import Data.Foldable (foldMap)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String as String
import Data.These (These(..), these)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class.Console as Aff
import Effect.Exception (throw)
import Effect.Now (now)
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Parsing as Parsing
import Registry.App.API as API
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Git (PullMode(..), WriteMode(..))
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LogVerbosity(..), debug, error, info, warn)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify as Notify
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (readManifestIndexFromDisk)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Codec
import Registry.Internal.Format as Internal.Format
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Scripts.LegacyImporter (IMPORT_CACHE, _importCache)
import Registry.Scripts.PackageDeleter (deletePackagesCodec)
import Registry.Solver as Solver
import Registry.Version as Version
import Run (Run)
import Run as Run
import Run.Except as Except

unionWithMapThese
  :: forall k a b c
   . Ord k
  => (k -> These a b -> Maybe c)
  -> Map k a
  -> Map k b
  -> Map k c
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
  in
    Map.mapMaybeWithKey f $ Map.unionWith combine (This <$> ma) (That <$> mb)

diff
  :: Map PackageName (Map Version (Map PackageName Range))
  -> Map PackageName (Map Version (Map PackageName Range))
  -> Map PackageName (Map Version (Map PackageName (These Range Range)))
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
fromThese = these (\l -> [ Just l, Nothing ]) (\r -> [ Nothing, Just r ]) (\l r -> [ Just l, Just r ])

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ Array.drop 2 <$> Node.Process.argv
  let
    getAction :: forall r. Aff (_ -> Run (API.PublishEffects + IMPORT_CACHE + r) Unit)
    getAction = case args of
      [ "--file", path ] -> do
        packageversions <- liftAff (readJsonFile deletePackagesCodec path) >>= case _ of
          Left err -> Aff.log err *> liftEffect (Process.exit 1)
          Right values -> pure values
        pure \registry -> do
          forWithIndex_ packageversions \package versionsofpackage -> do
            for_ versionsofpackage \version -> do
              deps <-
                case Map.lookup package registry of
                  Nothing -> Except.throw $ "Missing package: " <> PackageName.print package
                  Just versions ->
                    case Map.lookup version versions of
                      Nothing -> Except.throw $ "Missing version: " <> PackageName.print package <> "@" <> Version.print version
                      Just d -> pure d
              test registry package version deps
      [ "--manifest", path ] -> do
        let
          parser =
            Range.parser <|> (Version.parser <#> \v -> unsafeFromJust (Range.mk v (Version.bumpPatch v)))
          parsing = hush <<< flip Parsing.runParser parser
          codec = Codec.packageMap $ J.prismaticCodec "VersionOrRange" parsing Range.print J.string
          package = unsafeFromRight $ PackageName.parse "manifest"
          version = unsafeFromRight $ Version.parse "0.0.0"
        deps <- liftAff (readJsonFile codec path) >>= case _ of
          Left err -> Aff.log err *> liftEffect (Process.exit 1)
          Right values -> pure values
        pure \registry -> test registry package version deps
      [ "--all" ] -> pure \registry -> do
        forWithIndex_ registry \package versions -> do
          forWithIndex_ versions \version deps -> do
            test registry package version deps
      [ package_versionS ] | [ packageS, versionS ] <- String.split (String.Pattern "@") package_versionS ->
        let
          package = unsafeFromRight $ PackageName.parse packageS
          version = unsafeFromRight $ Version.parse versionS
        in
          pure \registry -> do
            let versions = unsafeFromJust $ Map.lookup package registry
            let deps = unsafeFromJust $ Map.lookup version versions
            test registry package version deps
      _ -> do
        Aff.log "Either use --all or package@version"
        liftEffect $ throw $ "Invalid arguments: " <> show args

  action <- getAction

  Env.loadEnvFile ".env"

  githubCacheRef <- Cache.newCacheRef
  legacyCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef
  importCacheRef <- Cache.newCacheRef
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache

  debouncer <- Git.newDebouncer
  let gitEnv pull write = { pull, write, repos: Git.defaultRepos { manifestIndex = Git.defaultRepos.manifestIndex { owner = "monoidmusician" }, registry = Git.defaultRepos.registry { owner = "monoidmusician" } }, workdir: scratchDir, debouncer }
  token <- Env.lookupRequired Env.githubToken
  octokit <- Octokit.newOctokit token
  let
    runAppEffects =
      Storage.interpret (Storage.handleReadOnly cache)
        >>> Pursuit.interpret Pursuit.handlePure
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Git.interpret (Git.handle (gitEnv Autostash ReadOnly))

  let
    doTheThing = do
      info $ "Reading registry index1 (modified) from " <> Path.concat [ scratchDir, "registry-index" ] <> " ..."
      index1 <- readManifestIndexFromDisk $ Path.concat [ scratchDir, "registry-index" ]

      info $ "Reading registry index0 (original) from " <> "tmp/registry-index" <> " ..."
      index0 <- readManifestIndexFromDisk $ "tmp/registry-index"

      let reg i = map (unwrap >>> _.dependencies) <$> ManifestIndex.toMap i
      let registry = reg $ index1

      liftAff $ writeJsonFile (Codec.packageMap (Codec.versionMap (Codec.packageMap (J.array (JC.maybe Range.codec))))) "tmp/registry_index_diff.json" (map map map fromThese <$> diff (reg index0) (reg index1))

      action registry

  -- Logging setup
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  now <- nowUTC
  let
    logFile = "solver-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
    logPath = Path.concat [ logDir, logFile ]

  doTheThing
    # Registry.interpret (Registry.handle registryCacheRef)
    # runAppEffects
    # Cache.interpret Legacy.Manifest._legacyCache (Cache.handleMemoryFs { cache, ref: legacyCacheRef })
    # Cache.interpret _importCache (Cache.handleMemoryFs { cache, ref: importCacheRef })
    # Notify.interpret Notify.handleLog
    # Except.catch (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
    # Run.runBaseAff'

  where
  test :: forall r. _ -> _ -> _ -> _ -> Run (API.PublishEffects + IMPORT_CACHE + r) Unit
  test registry package version deps = do
    info $ "%%% Solving " <> PackageName.print package <> "@" <> Version.print version <> " %%%"
    t0 <- liftEffect now
    let r = Solver.solve registry deps
    t1 <- liftEffect now
    let Milliseconds d = Instant.diff t1 t0
    debug $ "Took: " <> show d <> "ms"
    case r of
      Right vs ->
        debug $ Json.stringifyWithIndent 2 (J.encode (Codec.packageMap Version.codec) vs)
      Left es -> do
        error $ "Failed: " <> PackageName.print package <> "@" <> Version.print version
        warn $ String.take 5000 $ foldMap Solver.printSolverError es
