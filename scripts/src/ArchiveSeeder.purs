-- | This script populates the purescript/registry-archive repository with
-- | tarballs for packages whose GitHub sources have been deleted (404).
-- |
-- | The archive is a temporary measure for the legacy importer migration.
-- | Once packages are re-uploaded to the registry, the archive can be deleted.
-- |
-- | The script is designed to be re-run safely:
-- | - Caches 404 and accessible status to disk (scratch/.cache)
-- | - Skips tarballs that already exist in the archive
-- | - Reports transient errors (rate limits, network) separately
-- | - Exits with error code 1 if any packages had transient errors
module Registry.Scripts.ArchiveSeeder where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Apply (lift2)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Exists as Exists
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache (class FsEncodable, class MemoryEncodable, Cache, FsEncoding(..), MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

type Args =
  { archivePath :: FilePath
  , dryRun :: Boolean
  , package :: Maybe PackageName
  }

parser :: ArgParser Args
parser = Arg.fromRecord
  { archivePath:
      Arg.argument [ "--archive-path" ]
        "Path to local checkout of purescript/registry-archive"
        # Arg.default (Path.concat [ scratchDir, "registry-archive" ])
  , dryRun:
      Arg.flag [ "--dry-run" ]
        "Run without writing tarballs or committing to the registry-archive repo."
        # Arg.boolean
        # Arg.default false
  , package:
      Arg.argument [ "--package" ]
        "Only process the given package (by registry package name)."
        # Arg.unformat "PACKAGE" PackageName.parse
        # Arg.optional
  }

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "A script for seeding the registry archive with tarballs for deleted GitHub repos."
  parsedArgs <- case Arg.parseArgs "archive-seeder" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right a -> pure a

  Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv

  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef
  seederCacheRef <- Cache.newCacheRef
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache

  runAppEffects <- do
    debouncer <- Registry.newDebouncer
    repoLocks <- Registry.newRepoLocks
    let
      registryEnv =
        { pull: Git.Autostash
        , write: Registry.ReadOnly
        , repos: Registry.defaultRepos
        , workdir: scratchDir
        , debouncer
        , cacheRef: registryCacheRef
        , repoLocks
        , process: Registry.ScriptArchiveSeeder
        }

    token <- Env.lookupRequired Env.githubToken
    s3 <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)
    octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl
    pure do
      Registry.interpret (Registry.handle registryEnv)
        >>> Storage.interpret (Storage.handleS3 { s3, cache })
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Cache.interpret _seederCache (Cache.handleMemoryFs { cache, ref: seederCacheRef })

  -- Logging setup
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  now <- nowUTC

  let
    logFile = "archive-seeder-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
    logPath = Path.concat [ logDir, logFile ]

  hasErrors <- runArchiveSeeder parsedArgs logPath
    # runAppEffects
    # Except.catch (\msg -> Log.error msg *> Run.liftEffect (Process.exit' 1))
    # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'

  when hasErrors do
    liftEffect $ Process.exit' 1

-- | The status of a GitHub repo: either accessible or returns 404.
-- | We only cache these definitive states, not transient errors.
data RepoStatus = RepoAccessible | Repo404

derive instance Eq RepoStatus

repoStatusCodec :: CJ.Codec RepoStatus
repoStatusCodec = CJ.prismaticCodec "RepoStatus" decode encode CJ.string
  where
  decode = case _ of
    "accessible" -> Just RepoAccessible
    "404" -> Just Repo404
    _ -> Nothing
  encode = case _ of
    RepoAccessible -> "accessible"
    Repo404 -> "404"

type SEEDER_CACHE r = (seederCache :: Cache SeederCache | r)

_seederCache :: Proxy "seederCache"
_seederCache = Proxy

data SeederCache :: (Type -> Type -> Type) -> Type -> Type
data SeederCache c a = RepoStatusCache PackageName (c RepoStatus a)

instance Functor2 c => Functor (SeederCache c) where
  map k (RepoStatusCache name a) = RepoStatusCache name (map2 k a)

instance MemoryEncodable SeederCache where
  encodeMemory = case _ of
    RepoStatusCache name next ->
      Exists.mkExists $ Key ("RepoStatus__" <> PackageName.print name) next

instance FsEncodable SeederCache where
  encodeFs = case _ of
    RepoStatusCache name next ->
      Exists.mkExists $ AsJson ("RepoStatus__" <> PackageName.print name) repoStatusCodec next

type Stats =
  { packagesChecked :: Int
  , versionsChecked :: Int
  , packagesNeedingArchive :: Int
  , versionsNeedingArchive :: Int
  , tarballsWritten :: Int
  , tarballsSkipped :: Int
  , tarballsMissing :: Int
  , transientErrors :: Int
  }

emptyStats :: Stats
emptyStats =
  { packagesChecked: 0
  , versionsChecked: 0
  , packagesNeedingArchive: 0
  , versionsNeedingArchive: 0
  , tarballsWritten: 0
  , tarballsSkipped: 0
  , tarballsMissing: 0
  , transientErrors: 0
  }

type SeedEffects r = (SEEDER_CACHE + REGISTRY + STORAGE + GITHUB + LOG + EXCEPT String + AFF + EFFECT + r)

-- | Returns true if there were transient errors that require re-running
runArchiveSeeder :: forall r. Args -> FilePath -> Run (SeedEffects r) Boolean
runArchiveSeeder args logPath = do
  Log.info "Starting archive seeder!"
  Log.info $ "Logs available at " <> logPath
  Log.info $ "Archive path: " <> args.archivePath
  when args.dryRun do
    Log.info "Running in dry-run mode (no writes will be performed)"
  case args.package of
    Nothing -> Log.info "Processing all packages"
    Just name -> Log.info $ "Processing single package: " <> PackageName.print name

  -- Ensure archive directory exists (unless dry-run)
  unless args.dryRun do
    Run.liftAff $ FS.Extra.ensureDirectory args.archivePath

  statsRef <- liftEffect $ Ref.new emptyStats
  transientErrorsRef <- liftEffect $ Ref.new ([] :: Array String)

  let
    processPackage name (Metadata metadata) = do
      liftEffect $ Ref.modify_ (\s -> s { packagesChecked = s.packagesChecked + 1 }) statsRef

      let publishedVersions = Map.keys metadata.published
      let versionCount = Set.size publishedVersions
      liftEffect $ Ref.modify_ (\s -> s { versionsChecked = s.versionsChecked + versionCount }) statsRef

      -- Extract GitHub address from location
      case metadata.location of
        Git _ -> do
          Log.debug $ PackageName.print name <> ": Git location, skipping (only GitHub packages supported)"
        GitHub { owner, repo } -> do
          let address = { owner, repo }

          -- Check cache first for definitive status
          Cache.get _seederCache (RepoStatusCache name) >>= case _ of
            Just RepoAccessible -> do
              Log.debug $ PackageName.print name <> ": Cached as accessible, skipping"
            Just Repo404 -> do
              Log.debug $ PackageName.print name <> ": Cached as 404, processing..."
              processDeletedPackage args statsRef name publishedVersions versionCount
            Nothing -> do
              -- Probe GitHub to check if the repo is accessible
              GitHub.listTags address >>= case _ of
                Right _ -> do
                  Log.debug $ PackageName.print name <> ": GitHub repo accessible, caching and skipping"
                  Cache.put _seederCache (RepoStatusCache name) RepoAccessible
                Left (Octokit.APIError err) | err.statusCode == 404 -> do
                  Log.info $ PackageName.print name <> ": GitHub repo returns 404, caching and processing..."
                  Cache.put _seederCache (RepoStatusCache name) Repo404
                  processDeletedPackage args statsRef name publishedVersions versionCount
                Left otherErr -> do
                  -- Transient error - do NOT cache, log for re-run
                  let errMsg = PackageName.print name <> ": " <> Octokit.printGitHubError otherErr
                  Log.warn $ errMsg <> " (transient, will retry on next run)"
                  liftEffect $ Ref.modify_ (\s -> s { transientErrors = s.transientErrors + 1 }) statsRef
                  liftEffect $ Ref.modify_ (Array.snoc <@> errMsg) transientErrorsRef

  -- Process either single package or all packages
  case args.package of
    Just targetName -> Registry.readMetadata targetName >>= case _ of
      Nothing -> Except.throw $ "Package " <> PackageName.print targetName <> " not found in registry metadata."
      Just metadata -> processPackage targetName metadata
    Nothing -> do
      allMetadata <- Registry.readAllMetadata
      Log.info $ "Read metadata for " <> show (Map.size allMetadata) <> " packages."
      forWithIndex_ allMetadata processPackage

  -- Generate summary report
  stats <- liftEffect $ Ref.read statsRef
  transientErrors <- liftEffect $ Ref.read transientErrorsRef
  let report = formatReport stats transientErrors
  Log.info report

  let reportPath = Path.concat [ scratchDir, "archive-seeder-report.txt" ]
  Run.liftAff $ FS.Aff.writeTextFile UTF8 reportPath report
  Log.info $ "Report written to " <> reportPath

  let hadTransientErrors = stats.transientErrors > 0
  let wroteAnything = stats.tarballsWritten > 0

  if hadTransientErrors then do
    Log.warn $ "There were " <> show stats.transientErrors <> " transient errors. Re-run the script to retry."
    pure true
  else if args.dryRun then do
    Log.info $ String.joinWith "\n"
      [ ""
      , "Dry run complete!"
      , "Run without --dry-run to write tarballs and commit."
      ]
    pure false
  else if wroteAnything then do
    Log.warn "Make sure to commit and push archive changes!"
    pure false
  else do
    Log.info "Archive seeding complete! No new tarballs were written."
    pure false

processDeletedPackage
  :: forall r
   . Args
  -> Ref.Ref Stats
  -> PackageName
  -> Set Version
  -> Int
  -> Run (SeedEffects r) Unit
processDeletedPackage args statsRef name publishedVersions versionCount = do
  liftEffect $ Ref.modify_ (\s -> s { packagesNeedingArchive = s.packagesNeedingArchive + 1 }) statsRef
  liftEffect $ Ref.modify_ (\s -> s { versionsNeedingArchive = s.versionsNeedingArchive + versionCount }) statsRef

  Log.info $ PackageName.print name <> ": Checking S3 for tarballs..."

  -- Check S3 for available versions
  Except.runExcept (Storage.query name) >>= case _ of
    Left queryErr -> do
      Log.warn $ PackageName.print name <> ": Failed to query S3: " <> queryErr
    Right s3Versions -> do
      Log.debug $ PackageName.print name <> ": S3 has " <> show (Set.size s3Versions) <> " versions"

      -- For each published version, try to download and write to archive
      for_ publishedVersions \version -> do
        let formatted = formatPackageVersion name version
        let archiveSubdir = Path.concat [ args.archivePath, PackageName.print name ]
        let archiveFile = Path.concat [ archiveSubdir, Version.print version <> ".tar.gz" ]

        -- Check if already exists in archive (skip check in dry-run since we don't ensure dir exists)
        exists <- if args.dryRun then pure false else liftEffect $ FS.Sync.exists archiveFile
        if exists then do
          Log.debug $ formatted <> ": Already exists in archive, skipping"
          liftEffect $ Ref.modify_ (\s -> s { tarballsSkipped = s.tarballsSkipped + 1 }) statsRef
        else if Set.member version s3Versions then do
          if args.dryRun then do
            Log.info $ formatted <> ": Would download from S3 and write to archive (dry run)"
            liftEffect $ Ref.modify_ (\s -> s { tarballsWritten = s.tarballsWritten + 1 }) statsRef
          else do
            Log.info $ formatted <> ": Downloading from S3..."
            Run.liftAff $ FS.Extra.ensureDirectory archiveSubdir
            Except.runExcept (Storage.download name version archiveFile) >>= case _ of
              Left downloadErr -> do
                Log.warn $ formatted <> ": Failed to download: " <> downloadErr
                liftEffect $ Ref.modify_ (\s -> s { tarballsMissing = s.tarballsMissing + 1 }) statsRef
              Right _ -> do
                Log.info $ formatted <> ": Written to archive"
                liftEffect $ Ref.modify_ (\s -> s { tarballsWritten = s.tarballsWritten + 1 }) statsRef
        else do
          Log.warn $ formatted <> ": Not available in S3"
          liftEffect $ Ref.modify_ (\s -> s { tarballsMissing = s.tarballsMissing + 1 }) statsRef

formatReport :: Stats -> Array String -> String
formatReport stats transientErrors = String.joinWith "\n" (header <> transients)
  where
  header =
    [ "=== Archive Seeder Report ==="
    , ""
    , "Packages checked: " <> show stats.packagesChecked
    , "Versions checked: " <> show stats.versionsChecked
    , ""
    , "Packages needing archive (GitHub 404): " <> show stats.packagesNeedingArchive
    , "Versions needing archive: " <> show stats.versionsNeedingArchive
    , ""
    , "Tarballs written: " <> show stats.tarballsWritten
    , "Tarballs skipped (already exist): " <> show stats.tarballsSkipped
    , "Tarballs missing (not in S3): " <> show stats.tarballsMissing
    , ""
    , "Transient errors (re-run to retry): " <> show stats.transientErrors
    ]

  transients = do
    guard $ Array.null transientErrors
    [ "", "Packages with transient errors:" ] <> map ("  - " <> _) transientErrors
