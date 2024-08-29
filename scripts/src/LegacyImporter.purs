-- | This script attempts to import all package versions for packages listed in
-- | the legacy registry files (ie. bower-packages.json and new-packages.json).
-- |
-- | It can be run in different modes depending on whether you want to generate
-- | the registry from scratch, including uploading packages to the backend or
-- | you just want to iteratively pick up new releases.
module Registry.Scripts.LegacyImporter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Apply (lift2)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Codec.JSON.Variant as CJ.Variant
import Data.Compactable (separate)
import Data.DateTime (Date, Month(..))
import Data.DateTime as DateTime
import Data.Enum (toEnum)
import Data.Exists as Exists
import Data.Filterable (partition)
import Data.Foldable (foldMap)
import Data.Foldable as Foldable
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Ordering (invert)
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.These (These(..))
import Data.Tuple (uncurry)
import Data.Variant as Variant
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.App.API (COMPILER_CACHE)
import Registry.App.API as API
import Registry.App.CLI.Git as Git
import Registry.App.CLI.Purs (CompilerFailure, compilerFailureCodec)
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.Effect.Cache (class FsEncodable, class MemoryEncodable, Cache, FsEncoding(..), MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Source as Source
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion (LenientVersion)
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Manifest (LegacyManifestError(..), LegacyManifestValidationError)
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), rawPackageNameMapCodec, rawVersionMapCodec)
import Registry.App.Manifest.SpagoYaml as SpagoYaml
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (Address, Tag)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tmp as Tmp
import Registry.Internal.Codec (packageMap, versionMap)
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Solver (CompilerIndex(..))
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Run.Except
import Type.Proxy (Proxy(..))

data ImportMode = DryRun | GenerateRegistry | UpdateRegistry

derive instance Eq ImportMode

parser :: ArgParser ImportMode
parser = Arg.choose "command"
  [ Arg.flag [ "dry-run" ]
      "Run the registry importer without uploading packages or committing files."
      $> DryRun
  , Arg.flag [ "generate-registry" ]
      "Run the registry importer, uploading packages but not committing to metadata or the index."
      $> GenerateRegistry
  , Arg.flag [ "update-registry" ]
      "Run the registry importer, uploading packages and committing to metadata and the index."
      $> UpdateRegistry
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "A script for uploading legacy registry packages."
  mode <- case Arg.parseArgs "legacy-importer" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right command -> pure command

  Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv

  githubCacheRef <- Cache.newCacheRef
  legacyCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef
  importCacheRef <- Cache.newCacheRef
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache

  -- Set up interpreters according to the import mode. In dry-run mode we don't
  -- allow anyting to be committed or pushed, but data is still written to the
  -- local repository checkouts on disk. In generate-registry mode, tarballs are
  -- uploaded, but nothing is committed. In update-registry mode, tarballs are
  -- uploaded and manifests and metadata are written, committed, and pushed.
  runAppEffects <- do
    debouncer <- Registry.newDebouncer
    let registryEnv pull write = { pull, write, repos: Registry.defaultRepos, workdir: scratchDir, debouncer, cacheRef: registryCacheRef }
    case mode of
      DryRun -> do
        token <- Env.lookupRequired Env.githubToken
        octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl
        pure do
          Registry.interpret (Registry.handle (registryEnv Git.Autostash Registry.ReadOnly))
            >>> Storage.interpret (Storage.handleReadOnly cache)
            >>> Pursuit.interpret Pursuit.handlePure
            >>> Source.interpret (Source.handle Source.Old)
            >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })

      GenerateRegistry -> do
        token <- Env.lookupRequired Env.githubToken
        s3 <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)
        octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl
        pure do
          Registry.interpret (Registry.handle (registryEnv Git.Autostash (Registry.CommitAs (Git.pacchettibottiCommitter token))))
            >>> Storage.interpret (Storage.handleS3 { s3, cache })
            >>> Pursuit.interpret Pursuit.handlePure
            >>> Source.interpret (Source.handle Source.Old)
            >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })

      UpdateRegistry -> do
        token <- Env.lookupRequired Env.pacchettibottiToken
        s3 <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)
        octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl
        pure do
          Registry.interpret (Registry.handle (registryEnv Git.ForceClean (Registry.CommitAs (Git.pacchettibottiCommitter token))))
            >>> Storage.interpret (Storage.handleS3 { s3, cache })
            >>> Pursuit.interpret (Pursuit.handleAff token)
            >>> Source.interpret (Source.handle Source.Recent)
            >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })

  -- Logging setup
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  now <- nowUTC

  let
    logFile = "legacy-importer-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
    logPath = Path.concat [ logDir, logFile ]

  runLegacyImport logPath
    # runAppEffects
    # Cache.interpret Legacy.Manifest._legacyCache (Cache.handleMemoryFs { cache, ref: legacyCacheRef })
    # Cache.interpret _importCache (Cache.handleMemoryFs { cache, ref: importCacheRef })
    # Cache.interpret API._compilerCache (Cache.handleFs cache)
    # Run.Except.catch (\msg -> Log.error msg *> Run.liftEffect (Process.exit' 1))
    # Comment.interpret Comment.handleLog
    # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'

runLegacyImport :: forall r. FilePath -> Run (API.PublishEffects + IMPORT_CACHE + r) Unit
runLegacyImport logs = do
  Log.info "Starting legacy import!"
  Log.info $ "Logs available at " <> logs

  Log.info "Ensuring the registry is well-formed..."

  let
    hasMetadata allMetadata package version = case Map.lookup package allMetadata of
      Nothing -> false
      Just (Metadata m) -> isJust (Map.lookup version m.published) || isJust (Map.lookup version m.unpublished)

  _ <- do
    allManifests <- Registry.readAllManifests
    allMetadata <- Registry.readAllMetadata
    -- To ensure the metadata and registry index are always in sync, we remove
    -- any entries from the registry index that don't have accompanying metadata
    let mismatched = mapWithIndex (Map.filterKeys <<< not <<< hasMetadata allMetadata) $ ManifestIndex.toMap allManifests
    unless (Map.isEmpty mismatched) do
      Log.info "Removing entries from the manifest index that don't have accompanying metadata..."
      void $ forWithIndex mismatched \package versions ->
        forWithIndex versions \version _ -> do
          Log.debug $ "Found mismatch: " <> formatPackageVersion package version
          Registry.deleteManifest package version

  Log.info "Reading legacy registry..."
  legacyRegistry <- do
    { bower, new } <- Registry.readLegacyRegistry
    let allPackages = Map.union bower new
    let fixupNames = mapKeys (RawPackageName <<< stripPureScriptPrefix)
    pure $ fixupNames allPackages

  Log.info $ "Read " <> show (Set.size (Map.keys legacyRegistry)) <> " package names from the legacy registry."

  Log.info "Reading reserved 0.13 packages..."
  reserved0_13 <- readPackagesMetadata >>= case _ of
    Left err -> do
      Log.warn $ "Could not read reserved packages: " <> err
      Log.warn $ "Determining reserved packages..."
      metadata <- getPackagesMetadata legacyRegistry
      let cutoff = filterPackages_0_13 metadata
      writePackagesMetadata cutoff
      pure cutoff
    Right cutoff -> pure cutoff

  Log.info $ "Reserving metadata files for 0.13 and purs/metadata packages"
  forWithIndex_ reserved0_13 \package { address } -> Registry.readMetadata package >>= case _ of
    Nothing -> do
      Log.info $ "Writing empty metadata file for reserved 0.13 package " <> PackageName.print package
      let location = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
      let entry = Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
      Registry.writeMetadata package entry
    Just _ -> Log.debug $ PackageName.print package <> " already reserved."

  let metadataPackage = unsafeFromRight (PackageName.parse "metadata")
  let pursPackage = unsafeFromRight (PackageName.parse "purs")
  for_ [ metadataPackage, pursPackage ] \package ->
    Registry.readMetadata package >>= case _ of
      Nothing -> do
        Log.info $ "Writing empty metadata file for " <> PackageName.print package
        let location = GitHub { owner: "purescript", repo: "purescript-" <> PackageName.print package, subdir: Nothing }
        let entry = Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }
        Registry.writeMetadata package entry
      Just _ -> pure unit

  importedIndex <- importLegacyRegistry legacyRegistry

  Log.info "Writing package and version failures to disk..."
  Run.liftAff $ writePackageFailures importedIndex.failedPackages
  Run.liftAff $ writeVersionFailures importedIndex.failedVersions

  Log.info "Ready for upload!"
  let importStats = calculateImportStats legacyRegistry importedIndex
  let formattedStats = formatImportStats importStats
  Log.info formattedStats
  Run.liftAff $ FS.Aff.writeTextFile UTF8 (Path.concat [ scratchDir, "import-stats.txt" ]) formattedStats

  Log.info "Sorting packages for upload..."
  let allIndexPackages = ManifestIndex.toSortedArray ManifestIndex.ConsiderRanges importedIndex.registryIndex
  Run.liftAff $ FS.Aff.writeTextFile UTF8 (Path.concat [ scratchDir, "sorted-packages.txt" ]) $ String.joinWith "\n" $ map (\(Manifest { name, version }) -> PackageName.print name <> "@" <> Version.print version) allIndexPackages

  Log.info "Removing packages that previously failed publish or have been published"
  publishable <- do
    allMetadata <- Registry.readAllMetadata
    allIndexPackages # Array.filterA \(Manifest { name, version }) -> do
      Cache.get _importCache (PublishFailure name version) >>= case _ of
        Nothing -> pure $ not $ hasMetadata allMetadata name version
        Just _ -> pure false

  allCompilers <- PursVersions.pursVersions
  allCompilersRange <- case Range.mk (NonEmptyArray.head allCompilers) (Version.bumpPatch (NonEmptyArray.last allCompilers)) of
    Nothing -> Run.Except.throw $ "Failed to construct a compiler range from " <> Version.print (NonEmptyArray.head allCompilers) <> " and " <> Version.print (NonEmptyArray.last allCompilers)
    Just range -> do
      Log.info $ "All available compilers range: " <> Range.print range
      pure range

  let
    publishLegacyPackage :: Solver.TransitivizedRegistry -> Manifest -> Run _ Unit
    publishLegacyPackage legacyIndex (Manifest manifest) = do
      let formatted = formatPackageVersion manifest.name manifest.version
      Log.info $ "\n----------\nPUBLISHING: " <> formatted <> "\n----------\n"
      RawVersion ref <- case Map.lookup manifest.version =<< Map.lookup manifest.name importedIndex.packageRefs of
        Nothing -> Run.Except.throw $ "Unable to recover package ref for " <> formatted
        Just ref -> pure ref

      Log.debug "Building dependency index with compiler versions..."
      compilerIndex <- API.readCompilerIndex

      Log.debug $ "Solving dependencies for " <> formatted
      eitherResolutions <- do
        let toErrors = map Solver.printSolverError <<< NonEmptyList.toUnfoldable
        let isCompilerSolveError = String.contains (String.Pattern "Conflict in version ranges for purs:")
        let partitionIsCompiler = partitionEithers <<< map (\error -> if isCompilerSolveError error then Right error else Left error)

        legacySolution <- case Solver.solveFull { registry: legacyIndex, required: Solver.initializeRequired manifest.dependencies } of
          Left unsolvable -> do
            let errors = toErrors unsolvable
            let joined = String.joinWith " " errors
            let { fail: nonCompiler } = partitionIsCompiler errors
            Log.warn $ "Could not solve with legacy index " <> formatted <> Array.foldMap (append "\n") errors
            pure $ Left $ if Array.null nonCompiler then SolveFailedCompiler joined else SolveFailedDependencies joined
          Right resolutions -> do
            Log.debug $ "Solved " <> formatted <> " with legacy index."
            -- The solutions do us no good if the dependencies don't exist. Note
            -- the compiler index is updated on every publish.
            let lookupInRegistry res = maybe (Left res) (\_ -> Right res) (Map.lookup (fst res) (un CompilerIndex compilerIndex) >>= Map.lookup (snd res))
            let { fail: notRegistered } = partitionEithers $ map lookupInRegistry $ Map.toUnfoldable resolutions
            if (Array.null notRegistered) then
              pure $ Right resolutions
            else do
              let missing = "Some resolutions from legacy index are not registered: " <> String.joinWith ", " (map (uncurry formatPackageVersion) notRegistered)
              Log.warn missing
              Log.warn "Not using legacy index resolutions for this package."
              pure $ Left $ SolveFailedDependencies missing

        currentSolution <- case Solver.solveWithCompiler allCompilersRange compilerIndex manifest.dependencies of
          Left unsolvable -> do
            let errors = toErrors unsolvable
            let joined = String.joinWith " " errors
            let { fail: nonCompiler } = partitionIsCompiler errors
            Log.warn $ "Could not solve with current index " <> formatted <> Array.foldMap (append "\n") errors
            pure $ Left $ if Array.null nonCompiler then SolveFailedCompiler joined else SolveFailedDependencies joined
          Right (Tuple _ resolutions) -> do
            Log.debug $ "Solved " <> formatted <> " with contemporary index."
            pure $ Right resolutions

        pure $ case legacySolution, currentSolution of
          Left err, Left _ -> Left err
          Right resolutions, Left _ -> Right $ This resolutions
          Left _, Right resolutions -> Right $ That resolutions
          Right legacyResolutions, Right currentResolutions -> Right $ Both legacyResolutions currentResolutions

      case eitherResolutions of
        -- We skip if we couldn't solve (but we write the error to cache).
        Left err ->
          Cache.put _importCache (PublishFailure manifest.name manifest.version) err
        Right resolutionOptions -> do
          Log.info "Selecting usable compiler from resolutions..."

          let
            findFirstFromResolutions :: Map PackageName Version -> Run _ (Either (Map Version CompilerFailure) Version)
            findFirstFromResolutions resolutions = do
              Log.debug $ "Finding compiler for " <> formatted <> " with resolutions " <> printJson (Internal.Codec.packageMap Version.codec) resolutions <> "\nfrom dependency list\n" <> printJson (Internal.Codec.packageMap Range.codec) manifest.dependencies
              possibleCompilers <-
                if Map.isEmpty manifest.dependencies then do
                  Log.debug "No dependencies to determine ranges, so all compilers are potentially compatible."
                  pure $ NonEmptySet.fromFoldable1 allCompilers
                else do
                  Log.debug "No compiler version was produced by the solver, so all compilers are potentially compatible."
                  allMetadata <- Registry.readAllMetadata
                  case compatibleCompilers allMetadata resolutions of
                    Left [] -> do
                      Log.debug "No dependencies to determine ranges, so all compilers are potentially compatible."
                      pure $ NonEmptySet.fromFoldable1 allCompilers
                    Left errors -> do
                      let
                        printError { packages, compilers } = do
                          let key = String.joinWith ", " $ foldlWithIndex (\name prev version -> Array.cons (formatPackageVersion name version) prev) [] packages
                          let val = String.joinWith ", " $ map Version.print $ NonEmptySet.toUnfoldable compilers
                          key <> " support compilers " <> val
                      Log.warn $ Array.fold
                        [ "Resolutions admit no overlapping compiler versions:\n"
                        , Array.foldMap (append "\n  - " <<< printError) errors
                        ]
                      pure $ NonEmptySet.fromFoldable1 allCompilers
                    Right compilers -> do
                      Log.debug $ "Compatible compilers for resolutions of " <> formatted <> ": " <> stringifyJson (CJ.array Version.codec) (NonEmptySet.toUnfoldable compilers)
                      pure compilers

              cached <- do
                cached <- for (NonEmptySet.toUnfoldable possibleCompilers) \compiler ->
                  Cache.get API._compilerCache (API.Compilation (Manifest manifest) resolutions compiler) >>= case _ of
                    Nothing -> pure Nothing
                    Just { result: Left _ } -> pure Nothing
                    Just { target, result: Right _ } -> pure $ Just target
                pure $ NonEmptyArray.fromArray $ Array.catMaybes cached

              case cached of
                Just prev -> do
                  let selected = NonEmptyArray.last prev
                  Log.debug $ "Found successful cached compilation for " <> formatted <> " and chose " <> Version.print selected
                  pure $ Right selected
                Nothing -> do
                  Log.debug $ "No cached compilation for " <> formatted <> ", so compiling with all compilers to find first working one."
                  Log.debug "Fetching source and installing dependencies to test compilers"
                  tmp <- Tmp.mkTmpDir
                  { path } <- Source.fetch tmp manifest.location ref
                  Log.debug $ "Downloaded source to " <> path
                  Log.debug "Downloading dependencies..."
                  let installDir = Path.concat [ tmp, ".registry" ]
                  FS.Extra.ensureDirectory installDir
                  API.installBuildPlan resolutions installDir
                  Log.debug $ "Installed to " <> installDir
                  Log.debug "Trying compilers one-by-one..."
                  selected <- findFirstCompiler
                    { source: path
                    , installed: installDir
                    , compilers: NonEmptySet.toUnfoldable possibleCompilers
                    , resolutions
                    , manifest: Manifest manifest
                    }
                  FS.Extra.remove tmp
                  pure selected

          let
            collectCompilerErrors :: Map Version CompilerFailure -> Map (NonEmptyArray Version) CompilerFailure
            collectCompilerErrors failures = do
              let
                foldFn prev xs = do
                  let Tuple _ failure = NonEmptyArray.head xs
                  let key = map fst xs
                  Map.insert key failure prev
              Array.foldl foldFn Map.empty $ Array.groupAllBy (compare `on` snd) (Map.toUnfoldable failures)

            reportFailures :: forall a. _ -> Run _ (Either PublishError a)
            reportFailures failures = do
              let collected = collectCompilerErrors failures
              Log.error $ "Failed to find any valid compilers for publishing:\n" <> printJson compilerFailureMapCodec collected
              pure $ Left $ NoCompilersFound collected

          -- Here, we finally attempt to find a suitable compiler. If we only
          -- got one set of working resolutions that's what we use. If we got
          -- solutions with both the legacy and adjusted-manifest indices, then
          -- we try the adjusted index first since that's what is used in the
          -- publish pipeline.
          eitherCompiler <- case resolutionOptions of
            This legacyResolutions -> do
              selected <- findFirstFromResolutions legacyResolutions
              case selected of
                Left failures -> reportFailures failures
                Right compiler -> pure $ Right $ Tuple compiler legacyResolutions
            That currentResolutions -> do
              selected <- findFirstFromResolutions currentResolutions
              case selected of
                Left failures -> reportFailures failures
                Right compiler -> pure $ Right $ Tuple compiler currentResolutions
            Both legacyResolutions currentResolutions -> do
              selectedCurrent <- findFirstFromResolutions currentResolutions
              case selectedCurrent of
                Right compiler -> pure $ Right $ Tuple compiler currentResolutions
                Left currentFailures | legacyResolutions == currentResolutions -> reportFailures currentFailures
                Left _ -> do
                  Log.info $ "Could not find suitable compiler from current index, trying legacy solution..."
                  selectedLegacy <- findFirstFromResolutions legacyResolutions
                  case selectedLegacy of
                    Left failures -> reportFailures failures
                    Right compiler -> pure $ Right $ Tuple compiler legacyResolutions

          case eitherCompiler of
            Left err -> Cache.put _importCache (PublishFailure manifest.name manifest.version) err
            Right (Tuple compiler resolutions) -> do
              Log.debug $ "Selected " <> Version.print compiler <> " for publishing."
              let
                payload =
                  { name: manifest.name
                  , location: Just manifest.location
                  , ref
                  , compiler
                  , resolutions: Just resolutions
                  }
              Run.Except.runExcept (API.publish (Just legacyIndex) payload) >>= case _ of
                Left error -> do
                  Log.error $ "Failed to publish " <> formatted <> ": " <> error
                  Cache.put _importCache (PublishFailure manifest.name manifest.version) (PublishError error)
                Right _ -> do
                  Log.info $ "Published " <> formatted

  case publishable of
    [] -> Log.info "No packages to publish."
    manifests -> do
      Log.info $ Array.foldMap (append "\n")
        [ "----------"
        , "AVAILABLE TO PUBLISH"
        , Array.foldMap (\(Manifest { name, version }) -> "\n  - " <> formatPackageVersion name version) manifests
        , "----------"
        ]

      legacyIndex <- do
        Log.info "Transitivizing legacy registry..."
        pure
          $ Solver.exploreAllTransitiveDependencies
          $ Solver.initializeRegistry
          $ map (map (un Manifest >>> _.dependencies)) (ManifestIndex.toMap importedIndex.registryIndex)

      void $ for manifests (publishLegacyPackage legacyIndex)

  Log.info "Finished publishing! Collecting all publish failures and writing to disk."
  let
    collectError prev (Manifest { name, version }) = do
      Cache.get _importCache (PublishFailure name version) >>= case _ of
        Nothing -> pure prev
        Just error -> pure $ Map.insertWith Map.union name (Map.singleton version error) prev
  failures <- Array.foldM collectError Map.empty allIndexPackages
  Run.liftAff $ writePublishFailures failures

  let publishStats = collectPublishFailureStats importStats (map _.address reserved0_13) importedIndex.registryIndex failures
  let publishStatsMessage = formatPublishFailureStats publishStats
  Log.info publishStatsMessage
  Run.liftAff $ FS.Aff.writeTextFile UTF8 (Path.concat [ scratchDir, "publish-stats.txt" ]) publishStatsMessage
  Run.liftAff $ FS.Aff.writeTextFile UTF8 (Path.concat [ scratchDir, "reserved-packages.txt" ]) (String.joinWith "\n" (map PackageName.print (Set.toUnfoldable publishStats.packages.reserved)))
  Run.liftAff $ FS.Aff.writeTextFile UTF8 (Path.concat [ scratchDir, "removed-packages.txt" ]) (String.joinWith "\n" (map PackageName.print (Set.toUnfoldable (Set.difference publishStats.packages.failed publishStats.packages.reserved))))

-- | Record all package failures to the 'package-failures.json' file.
writePublishFailures :: Map PackageName (Map Version PublishError) -> Aff Unit
writePublishFailures =
  writeJsonFile (packageMap (versionMap jsonValidationErrorCodec)) (Path.concat [ scratchDir, "publish-failures.json" ])
    <<< map (map formatPublishError)

-- | Record all package failures to the 'package-failures.json' file.
writePackageFailures :: Map RawPackageName PackageValidationError -> Aff Unit
writePackageFailures =
  writeJsonFile (rawPackageNameMapCodec jsonValidationErrorCodec) (Path.concat [ scratchDir, "package-failures.json" ])
    <<< map formatPackageValidationError

-- | Record all version failures to the 'version-failures.json' file.
writeVersionFailures :: Map RawPackageName (Map RawVersion VersionValidationError) -> Aff Unit
writeVersionFailures =
  writeJsonFile (rawPackageNameMapCodec (rawVersionMapCodec jsonValidationErrorCodec)) (Path.concat [ scratchDir, "version-failures.json" ])
    <<< map (map formatVersionValidationError)

type LegacyRegistry = Map RawPackageName String

type ImportedIndex =
  { failedPackages :: Map RawPackageName PackageValidationError
  , failedVersions :: Map RawPackageName (Map RawVersion VersionValidationError)
  , removedPackages :: Map PackageName Location
  , registryIndex :: ManifestIndex
  , packageRefs :: Map PackageName (Map Version RawVersion)
  }

-- | Construct a valid registry index containing manifests for all packages from
-- | the legacy registry files. This function also collects import errors for
-- | packages and package versions and reports packages that are present in the
-- | legacy registry but not in the resulting registry.
importLegacyRegistry :: forall r. LegacyRegistry -> Run (API.PublishEffects + IMPORT_CACHE + r) ImportedIndex
importLegacyRegistry legacyRegistry = do
  Log.info "Importing legacy registry manifests (this will take a while if you do not have a cache)"
  manifests <- forWithIndex legacyRegistry buildLegacyPackageManifests

  let
    separatedPackages :: { left :: Map RawPackageName PackageValidationError, right :: Map RawPackageName (Map RawVersion _) }
    separatedPackages = separate manifests

    separatedVersions :: { left :: Map RawPackageName (Map RawVersion VersionValidationError), right :: Map RawPackageName (Map RawVersion Manifest) }
    separatedVersions =
      separatedPackages.right # flip foldlWithIndex { left: Map.empty, right: Map.empty } \key acc next -> do
        let { left, right } = separate next
        { left: if Map.isEmpty left then acc.left else Map.insert key left acc.left
        , right: if Map.isEmpty right then acc.right else Map.insert key right acc.right
        }

    validLegacyManifests :: Set Manifest
    validLegacyManifests = Set.fromFoldable $ foldMap Map.values $ Map.values separatedVersions.right

    -- The raw ref strings associated with the input package names and versions
    packageRefs :: Map PackageName (Map Version RawVersion)
    packageRefs = Map.fromFoldableWith Map.union do
      Tuple _ rawVersions <- Map.toUnfoldable separatedVersions.right
      Tuple rawVersion (Manifest manifest) <- Map.toUnfoldable rawVersions
      [ Tuple manifest.name (Map.singleton manifest.version rawVersion) ]

    -- A 'checked' index is one where we have verified that all dependencies
    -- are self-contained within the registry.
    Tuple unsatisfied validIndex = ManifestIndex.maximalIndex ManifestIndex.ConsiderRanges validLegacyManifests

    -- The list of all packages that were present in the legacy registry files,
    -- but which have no versions present in the fully-imported registry.
    removedPackages :: Map PackageName Location
    removedPackages =
      Map.fromFoldable $ Array.mapMaybe removed $ Map.toUnfoldable legacyRegistry
      where
      removed (Tuple (RawPackageName name) address) = do
        packageName <- hush $ PackageName.parse name
        guard $ isNothing $ Map.lookup packageName $ ManifestIndex.toMap validIndex
        { owner, repo } <- hush $ Parsing.runParser address legacyRepoParser
        pure (Tuple packageName (GitHub { owner, repo, subdir: Nothing }))

    -- The list of all packages that could not be included because of an error
    -- with the overall package, prior to fetching any versions.
    packageFailures :: Map RawPackageName PackageValidationError
    packageFailures = separatedPackages.left

    -- The list of all package versions that could not be included because of
    -- an error with the specific version. Includes failures to fetch or parse
    -- manifest files as well as valid manifests that contain dependencies that
    -- are not in the registry.
    versionFailures :: Map RawPackageName (Map RawVersion VersionValidationError)
    versionFailures = do
      let
        foldFn acc fail = do
          let error = { error: UnregisteredDependencies fail.dependencies, reason: "Contains dependencies that are not registered." }
          Map.insertWith Map.union fail.package (Map.singleton fail.version error) acc
        dependencyFailures =
          Array.foldl foldFn Map.empty do
            Tuple name versions <- Map.toUnfoldable unsatisfied
            Tuple version deps <- Map.toUnfoldable versions
            let ref = unsafeFromJust (Map.lookup name packageRefs >>= Map.lookup version)
            [ { package: RawPackageName (PackageName.print name), version: ref, dependencies: Array.fromFoldable $ Map.keys deps } ]
      Map.unionWith Map.union separatedVersions.left dependencyFailures

  pure
    { failedPackages: packageFailures
    , failedVersions: versionFailures
    , removedPackages: removedPackages
    , registryIndex: validIndex
    , packageRefs
    }

-- | Attempt to build valid manifests for all releases associated with the given
-- | legacy package. This will result in a package error if versions could not
-- | be fetched in the first place. Otherwise, it will produce errors for all
-- | versions that don't produce valid manifests, and manifests for all that do.
buildLegacyPackageManifests
  :: forall r
   . RawPackageName
  -> String
  -> Run (API.PublishEffects + IMPORT_CACHE + r) (Either PackageValidationError (Map RawVersion (Either VersionValidationError Manifest)))
buildLegacyPackageManifests rawPackage rawUrl = Run.Except.runExceptAt _exceptPackage do
  Log.info $ "Processing " <> un RawPackageName rawPackage
  package <- validatePackage rawPackage rawUrl

  let
    location :: Location
    location = GitHub { owner: package.address.owner, repo: package.address.repo, subdir: Nothing }

    buildManifestForVersion :: Tag -> Run _ (Either VersionValidationError Manifest)
    buildManifestForVersion tag = Run.Except.runExceptAt _exceptVersion do
      version <- exceptVersion $ validateVersion tag
      Cache.get _importCache (ImportManifest package.name (RawVersion tag.name)) >>= case _ of
        Just cached -> exceptVersion cached
        Nothing -> do
          -- While technically not 'legacy', we do need to handle packages with
          -- spago.yaml files because they've begun to pop up since the registry
          -- alpha began and we don't want to drop them when doing a re-import.
          fetchSpagoYaml package.address (RawVersion tag.name) >>= case _ of
            Just manifest -> do
              Log.debug $ "Built manifest from discovered spago.yaml file."
              Cache.put _importCache (ImportManifest package.name (RawVersion tag.name)) (Right manifest)
              pure manifest
            Nothing -> do
              Log.debug $ "Building manifest in legacy import because there is no registry entry, spago.yaml, or cached result: " <> formatPackageVersion package.name (LenientVersion.version version)
              manifest <- Run.Except.runExceptAt _exceptVersion do
                exceptVersion $ validateVersionDisabled package.name version
                legacyManifest <- do
                  Legacy.Manifest.fetchLegacyManifest package.name package.address (RawVersion tag.name) >>= case _ of
                    Left error -> throwVersion { error: InvalidManifest error, reason: "Legacy manifest could not be parsed." }
                    Right result -> pure result
                pure $ Legacy.Manifest.toManifest package.name (LenientVersion.version version) location legacyManifest
              case manifest of
                Left err -> Log.info $ "Failed to build manifest for " <> PackageName.print package.name <> "@" <> tag.name <> ": " <> printJson versionValidationErrorCodec err
                Right val -> Log.info $ "Built manifest for " <> PackageName.print package.name <> "@" <> tag.name <> ":\n" <> printJson Manifest.codec val
              Cache.put _importCache (ImportManifest package.name (RawVersion tag.name)) manifest
              exceptVersion manifest

  manifests <- for package.tags \tag -> do
    manifest <- buildManifestForVersion tag
    pure (Tuple (RawVersion tag.name) manifest)

  pure $ Map.fromFoldable manifests

data PublishError
  = SolveFailedDependencies String
  | SolveFailedCompiler String
  | NoCompilersFound (Map (NonEmptyArray Version) CompilerFailure)
  | UnsolvableDependencyCompilers (Array GroupedByCompilers)
  | PublishError String

derive instance Eq PublishError

publishErrorCodec :: CJ.Codec PublishError
publishErrorCodec = Profunctor.dimap toVariant fromVariant $ CJ.Variant.variantMatch
  { solveFailedCompiler: Right CJ.string
  , solveFailedDependencies: Right CJ.string
  , noCompilersFound: Right compilerFailureMapCodec
  , unsolvableDependencyCompilers: Right (CJ.array groupedByCompilersCodec)
  , publishError: Right CJ.string
  }
  where
  toVariant = case _ of
    SolveFailedDependencies error -> Variant.inj (Proxy :: _ "solveFailedDependencies") error
    SolveFailedCompiler error -> Variant.inj (Proxy :: _ "solveFailedCompiler") error
    NoCompilersFound failed -> Variant.inj (Proxy :: _ "noCompilersFound") failed
    UnsolvableDependencyCompilers group -> Variant.inj (Proxy :: _ "unsolvableDependencyCompilers") group
    PublishError error -> Variant.inj (Proxy :: _ "publishError") error

  fromVariant = Variant.match
    { solveFailedDependencies: SolveFailedDependencies
    , solveFailedCompiler: SolveFailedCompiler
    , noCompilersFound: NoCompilersFound
    , unsolvableDependencyCompilers: UnsolvableDependencyCompilers
    , publishError: PublishError
    }

type PublishFailureStats =
  { packages :: { total :: Int, considered :: Int, partial :: Int, failed :: Set PackageName, reserved :: Set PackageName }
  , versions :: { total :: Int, considered :: Int, failed :: Int, reason :: Map String Int }
  }

collectPublishFailureStats :: ImportStats -> Map PackageName Address -> ManifestIndex -> Map PackageName (Map Version PublishError) -> PublishFailureStats
collectPublishFailureStats importStats reserved0_13 importedIndex failures = do
  let
    index :: Map PackageName (Map Version Manifest)
    index = ManifestIndex.toMap importedIndex

    countVersions :: forall a. Map PackageName (Map Version a) -> Int
    countVersions = Array.foldl (\prev (Tuple _ versions) -> prev + Map.size versions) 0 <<< Map.toUnfoldable

    startPackages :: Int
    startPackages = importStats.packagesProcessed

    consideredPackages :: Int
    consideredPackages = Map.size index

    startVersions :: Int
    startVersions = importStats.versionsProcessed

    consideredVersions :: Int
    consideredVersions = countVersions index

    failedPackages :: Int
    failedPackages = Map.size failures

    failedVersions :: Int
    failedVersions = countVersions failures

    removedPackages :: Set PackageName
    removedPackages = do
      let
        foldFn package prev versions = fromMaybe prev do
          allVersions <- Map.lookup package index
          guard (Map.keys allVersions == Map.keys versions)
          pure $ Set.insert package prev

      foldlWithIndex foldFn Set.empty failures

    -- Packages that are eligible for removal — but are reserved due to 0.13 or
    -- organization status — are the 'reserved packages'.
    reservedPackages :: Set PackageName
    reservedPackages = Set.intersection removedPackages (Map.keys reserved0_13)

    countByFailure :: Map String Int
    countByFailure = do
      let
        toKey = case _ of
          SolveFailedDependencies _ -> "Solving failed (dependencies)"
          SolveFailedCompiler _ -> "Solving failed (compiler)"
          NoCompilersFound _ -> "No compilers usable for publishing"
          UnsolvableDependencyCompilers _ -> "Dependency compiler conflict"
          PublishError _ -> "Publishing failed"

        foldFn prev (Tuple _ versions) =
          Array.foldl (\prevCounts (Tuple _ error) -> Map.insertWith (+) (toKey error) 1 prevCounts) prev (Map.toUnfoldable versions)

      Array.foldl foldFn Map.empty (Map.toUnfoldable failures)

  { packages:
      { total: startPackages
      , considered: consideredPackages
      , partial: failedPackages
      , reserved: reservedPackages
      , failed: removedPackages
      }
  , versions:
      { total: startVersions
      , considered: consideredVersions
      , failed: failedVersions
      , reason: countByFailure
      }
  }

formatPublishFailureStats :: PublishFailureStats -> String
formatPublishFailureStats { packages, versions } = String.joinWith "\n"
  [ "--------------------"
  , "PUBLISH FAILURES"
  , "--------------------"
  , ""
  , show packages.considered <> " of " <> show packages.total <> " total packages were considered for publishing (others had no manifests imported.)"
  , "  - " <> show (packages.total - packages.partial - (Set.size packages.failed)) <> " out of " <> show packages.considered <> " packages fully succeeded."
  , "  - " <> show packages.partial <> " packages partially succeeded."
  , "  - " <> show (Set.size packages.reserved) <> " packages fully failed, but are reserved due to 0.13 or organization status."
  , "  - " <> show (Set.size packages.failed - Set.size packages.reserved) <> " packages had all versions fail and will be removed."
  , ""
  , show versions.considered <> " of " <> show versions.total <> " total versions were considered for publishing.\n  - " <> show versions.failed <> " out of " <> show versions.total <> " versions failed."
  , Array.foldMap (\(Tuple key val) -> "\n    - " <> key <> ": " <> show val) (Array.sortBy (comparing snd) (Map.toUnfoldable versions.reason))
  ]

compilerFailureMapCodec :: CJ.Codec (Map (NonEmptyArray Version) CompilerFailure)
compilerFailureMapCodec = do
  let
    print = NonEmptyArray.intercalate "," <<< map Version.print
    parse input = do
      let versions = String.split (String.Pattern ",") input
      let { fail, success } = partitionEithers $ map Version.parse versions
      case NonEmptyArray.fromArray success of
        Nothing | Array.null fail -> Left "No versions"
        Nothing -> Left $ "No versions parsed, some failed: " <> String.joinWith ", " fail
        Just result -> pure result
  Internal.Codec.strMap "CompilerFailureMap" parse print compilerFailureCodec

type EXCEPT_VERSION :: Row (Type -> Type) -> Row (Type -> Type)
type EXCEPT_VERSION r = (exceptVersion :: Run.Except.Except VersionValidationError | r)

_exceptVersion = Proxy :: Proxy "exceptVersion"

throwVersion :: forall r a. VersionValidationError -> Run (EXCEPT_VERSION + r) a
throwVersion = Run.Except.throwAt _exceptVersion

exceptVersion :: forall r a. Either VersionValidationError a -> Run (EXCEPT_VERSION + r) a
exceptVersion = Run.Except.rethrowAt _exceptVersion

type VersionValidationError = { error :: VersionError, reason :: String }

versionValidationErrorCodec :: CJ.Codec VersionValidationError
versionValidationErrorCodec = CJ.named "VersionValidationError" $ CJ.Record.object
  { error: versionErrorCodec
  , reason: CJ.string
  }

-- | An error that affects a specific package version
data VersionError
  = InvalidTag Tag
  | DisabledVersion
  | InvalidManifest LegacyManifestValidationError
  | UnregisteredDependencies (Array PackageName)

versionErrorCodec :: CJ.Codec VersionError
versionErrorCodec = Profunctor.dimap toVariant fromVariant $ CJ.Variant.variantMatch
  { invalidTag: Right $ CJ.named "Tag" $ CJ.Record.object
      { name: CJ.string
      , sha: CJ.string
      , url: CJ.string
      }
  , disabledVersion: Left unit
  , invalidManifest: Right $ CJ.named "LegacyManifestValidationError" $ CJ.Record.object
      { error: Legacy.Manifest.legacyManifestErrorCodec
      , reason: CJ.string
      }
  , unregisteredDependencies: Right (CJ.array PackageName.codec)
  }
  where
  toVariant = case _ of
    InvalidTag tag -> Variant.inj (Proxy :: _ "invalidTag") tag
    DisabledVersion -> Variant.inj (Proxy :: _ "disabledVersion") unit
    InvalidManifest inner -> Variant.inj (Proxy :: _ "invalidManifest") inner
    UnregisteredDependencies inner -> Variant.inj (Proxy :: _ "unregisteredDependencies") inner

  fromVariant = Variant.match
    { invalidTag: InvalidTag
    , disabledVersion: \_ -> DisabledVersion
    , invalidManifest: InvalidManifest
    , unregisteredDependencies: UnregisteredDependencies
    }

validateVersionDisabled :: PackageName -> LenientVersion -> Either VersionValidationError Unit
validateVersionDisabled package version =
  case Map.lookup (Tuple package (LenientVersion.raw version)) disabledPackageVersions of
    Nothing -> pure unit
    Just reason -> Left { error: DisabledVersion, reason }
  where
  disabledPackageVersions :: Map (Tuple PackageName String) String
  disabledPackageVersions = Map.fromFoldable
    [ Tuple (disabled "concur-core" "v0.3.9") noSrcDirectory
    , Tuple (disabled "concur-react" "v0.3.9") noSrcDirectory
    , Tuple (disabled "pux-devtool" "v5.0.0") noSrcDirectory
    , Tuple (disabled "endpoints-express" "0.0.1") noSrcDirectory
    , Tuple (disabled "argonaut-aeson-generic" "0.4.0") "Does not compile."
    , Tuple (disabled "batteries-core" "0.3.0") "Does not solve."
    ]
    where
    noSrcDirectory = "Does not contain a 'src' directory."
    disabled name = Tuple (unsafeFromRight $ PackageName.parse name)

validateVersion :: Tag -> Either VersionValidationError LenientVersion
validateVersion tag =
  LenientVersion.parse tag.name # lmap \parseError ->
    { error: InvalidTag tag
    , reason: parseError
    }

type EXCEPT_PACKAGE :: Row (Type -> Type) -> Row (Type -> Type)
type EXCEPT_PACKAGE r = (exceptPackage :: Run.Except.Except PackageValidationError | r)

_exceptPackage = Proxy :: Proxy "exceptPackage"

throwPackage :: forall r a. PackageValidationError -> Run (EXCEPT_PACKAGE + r) a
throwPackage = Run.Except.throwAt _exceptPackage

exceptPackage :: forall r a. Either PackageValidationError a -> Run (EXCEPT_PACKAGE + r) a
exceptPackage = Run.Except.rethrowAt _exceptPackage

type PackageValidationError = { error :: PackageError, reason :: String }

-- | An error that affects an entire package
data PackageError
  = InvalidPackageName
  | InvalidPackageURL String
  | PackageURLRedirects { registered :: Address, received :: Address }
  | CannotAccessRepo Address
  | DisabledPackage

derive instance Eq PackageError

type PackageResult =
  { name :: PackageName
  , address :: Address
  , tags :: Array Tag
  }

type PackagesMetadata = { address :: Address, lastPublished :: Date }

packagesMetadataCodec :: CJ.Codec PackagesMetadata
packagesMetadataCodec = CJ.named "PackagesMetadata" $ CJ.Record.object
  { address: CJ.named "Address" $ CJ.Record.object { owner: CJ.string, repo: CJ.string }
  , lastPublished: Internal.Codec.iso8601Date
  }

getPackagesMetadata :: forall r. Map RawPackageName String -> Run (EXCEPT String + GITHUB + r) (Map PackageName PackagesMetadata)
getPackagesMetadata legacyRegistry = do
  associated <- for (Map.toUnfoldableUnordered legacyRegistry) \(Tuple rawName rawUrl) -> do
    Run.Except.runExceptAt (Proxy :: _ "exceptPackage") (validatePackage rawName rawUrl) >>= case _ of
      Left _ -> pure Nothing
      Right { name, address, tags } -> case Array.head tags of
        Nothing -> pure Nothing
        Just tag -> do
          result <- GitHub.getCommitDate address tag.sha
          case result of
            Left error -> unsafeCrashWith ("Failed to get commit date for " <> PackageName.print name <> "@" <> tag.name <> ": " <> Octokit.printGitHubError error)
            Right date -> pure $ Just $ Tuple name { address, lastPublished: DateTime.date date }
  pure $ Map.fromFoldable $ Array.catMaybes associated

filterPackages_0_13 :: Map PackageName PackagesMetadata -> Map PackageName PackagesMetadata
filterPackages_0_13 = do
  let
    -- 0.13 release date
    cutoff = DateTime.canonicalDate (unsafeFromJust (toEnum 2019)) May (unsafeFromJust (toEnum 29))
    organizations =
      [ "purescript"
      , "purescript-contrib"
      , "purescript-node"
      , "purescript-web"
      , "rowtype-yoga"
      , "purescript-halogen"
      , "purescript-deprecated"
      ]

  Map.filterWithKey \_ metadata -> do
    let { owner } = metadata.address
    owner `Array.elem` organizations || metadata.lastPublished >= cutoff

writePackagesMetadata :: forall r. Map PackageName PackagesMetadata -> Run (LOG + AFF + r) Unit
writePackagesMetadata pkgs = do
  let path = Path.concat [ scratchDir, "packages-metadata.json" ]
  Log.info $ "Writing packages metadata to " <> path
  Run.liftAff $ writeJsonFile (packageMap packagesMetadataCodec) path pkgs

readPackagesMetadata :: forall r. Run (AFF + r) (Either String (Map PackageName PackagesMetadata))
readPackagesMetadata = Run.liftAff $ readJsonFile (packageMap packagesMetadataCodec) (Path.concat [ scratchDir, "packages-metadata.json" ])

validatePackage :: forall r. RawPackageName -> String -> Run (GITHUB + EXCEPT_PACKAGE + EXCEPT String + r) PackageResult
validatePackage rawPackage rawUrl = do
  name <- exceptPackage $ validatePackageName rawPackage
  exceptPackage $ validatePackageDisabled name
  address <- exceptPackage $ validatePackageAddress rawUrl
  tags <- fetchPackageTags address
  -- We do not allow packages that redirect from their registered location elsewhere. The package
  -- transferrer will handle automatically transferring these packages.
  case Array.head tags of
    Nothing -> pure { name, address, tags }
    Just tag -> do
      tagAddress <- exceptPackage case tagUrlToRepoUrl tag.url of
        Nothing -> Left { error: InvalidPackageURL tag.url, reason: "Failed to format redirected " <> tag.url <> " as a GitHub.Address." }
        Just formatted -> Right formatted
      exceptPackage $ validatePackageLocation { registered: address, received: tagAddress }
      pure { name, address, tags }

fetchPackageTags :: forall r. Address -> Run (GITHUB + EXCEPT_PACKAGE + EXCEPT String + r) (Array Tag)
fetchPackageTags address = GitHub.listTags address >>= case _ of
  Left err -> case err of
    Octokit.APIError apiError | apiError.statusCode >= 400 -> do
      let error = CannotAccessRepo address
      let reason = "GitHub API error with status code " <> show apiError.statusCode
      throwPackage { error, reason }
    _ ->
      Run.Except.throw $ String.joinWith "\n"
        [ "Unexpected GitHub error with a status <= 400"
        , Octokit.printGitHubError err
        ]
  Right tags ->
    pure tags

validatePackageLocation :: { registered :: Address, received :: Address } -> Either PackageValidationError Unit
validatePackageLocation addresses = do
  let lower { owner, repo } = String.toLower owner <> "/" <> String.toLower repo
  if lower addresses.registered /= lower addresses.received then
    Left
      { error: PackageURLRedirects addresses
      , reason: "Registered address " <> show addresses.registered <> " redirects to another location " <> show addresses.received
      }
  else
    Right unit

validatePackageAddress :: String -> Either PackageValidationError Address
validatePackageAddress packageUrl =
  Parsing.runParser packageUrl legacyRepoParser # lmap \parserError ->
    { error: InvalidPackageURL packageUrl
    , reason: Parsing.parseErrorMessage parserError
    }

-- Example tag url:
-- https://api.github.com/repos/octocat/Hello-World/commits/c5b97d5ae6c19d5c5df71a34c7fbeeda2479ccbc
tagUrlToRepoUrl :: String -> Maybe Address
tagUrlToRepoUrl url = do
  noPrefix <- String.stripPrefix (String.Pattern "https://api.github.com/repos/") url
  let getOwnerRepoArray = Array.take 2 <<< String.split (String.Pattern "/")
  case getOwnerRepoArray noPrefix of
    [ owner, repo ] -> Just { owner, repo: String.toLower repo }
    _ -> Nothing

validatePackageDisabled :: PackageName -> Either PackageValidationError Unit
validatePackageDisabled package =
  case Map.lookup (PackageName.print package) disabledPackages of
    Nothing -> pure unit
    Just reason -> Left { error: DisabledPackage, reason }
  where
  -- These packages have no usable versions, but we've discovered by running the
  -- pipeline that they produce at least one manifest. To avoid processing these
  -- packages we manually disable them.
  disabledPackages :: Map String String
  disabledPackages = Map.fromFoldable
    [ Tuple "metadata" reservedPackage
    , Tuple "purs" reservedPackage
    , Tuple "bitstrings" noSrcDirectory
    , Tuple "purveyor" noSrcDirectory
    , Tuple "styled-components" noSrcDirectory
    , Tuple "styled-system" noSrcDirectory
    ]
    where
    reservedPackage = "Reserved package which cannot be uploaded."
    noSrcDirectory = "No version contains a 'src' directory."

-- | Validate that a package name parses. Expects the package to already have
-- | had its 'purescript-' prefix removed.
validatePackageName :: RawPackageName -> Either PackageValidationError PackageName
validatePackageName (RawPackageName name) =
  PackageName.parse name # lmap \parserError ->
    { error: InvalidPackageName
    , reason: parserError
    }

type JsonValidationError =
  { tag :: String
  , value :: Maybe JSON
  , reason :: String
  }

jsonValidationErrorCodec :: CJ.Codec JsonValidationError
jsonValidationErrorCodec = CJ.named "JsonValidationError" $ CJ.Record.object
  { tag: CJ.string
  , value: CJ.Record.optional CJ.json
  , reason: CJ.string
  }

formatPackageValidationError :: PackageValidationError -> JsonValidationError
formatPackageValidationError { error, reason } = case error of
  InvalidPackageName ->
    { tag: "InvalidPackageName", value: Nothing, reason }
  InvalidPackageURL url ->
    { tag: "InvalidPackageURL", value: Just (CJ.encode CJ.string url), reason }
  PackageURLRedirects { registered } ->
    { tag: "PackageURLRedirects", value: Just (CJ.encode CJ.string (registered.owner <> "/" <> registered.repo)), reason }
  CannotAccessRepo address ->
    { tag: "CannotAccessRepo", value: Just (CJ.encode CJ.string (address.owner <> "/" <> address.repo)), reason }
  DisabledPackage ->
    { tag: "DisabledPackage", value: Nothing, reason }

formatVersionValidationError :: VersionValidationError -> JsonValidationError
formatVersionValidationError { error, reason } = case error of
  InvalidTag tag ->
    { tag: "InvalidTag", value: Just (CJ.encode CJ.string tag.name), reason }
  DisabledVersion ->
    { tag: "DisabledVersion", value: Nothing, reason }
  InvalidManifest err -> do
    let errorValue = Legacy.Manifest.printLegacyManifestError err.error
    { tag: "InvalidManifest", value: Just (CJ.encode CJ.string errorValue), reason }
  UnregisteredDependencies names ->
    { tag: "UnregisteredDependencies", value: Just (CJ.encode (CJ.array PackageName.codec) names), reason }

formatPublishError :: PublishError -> JsonValidationError
formatPublishError = case _ of
  SolveFailedCompiler error ->
    { tag: "SolveFailedCompiler", value: Nothing, reason: error }
  SolveFailedDependencies error ->
    { tag: "SolveFailedDependencies", value: Nothing, reason: error }
  NoCompilersFound versions ->
    { tag: "NoCompilersFound", value: Just (CJ.encode compilerFailureMapCodec versions), reason: "No valid compilers found for publishing." }
  UnsolvableDependencyCompilers failed ->
    { tag: "UnsolvableDependencyCompilers", value: Just (CJ.encode (CJ.array groupedByCompilersCodec) failed), reason: "Resolved dependencies cannot compile together" }
  PublishError error ->
    { tag: "PublishError", value: Nothing, reason: error }

type ImportStats =
  { packagesProcessed :: Int
  , versionsProcessed :: Int
  , packageNamesRemoved :: Int
  , packageResults :: { success :: Int, partial :: Int, fail :: Int }
  , versionResults :: { success :: Int, fail :: Int }
  , packageErrors :: Map String Int
  , versionErrors :: Map String Int
  }

formatImportStats :: ImportStats -> String
formatImportStats stats = String.joinWith "\n"
  [ "\n----------\nIMPORT STATS\n----------\n"
  , show stats.packagesProcessed <> " packages processed:"
  , indent $ show stats.packageResults.success <> " fully successful"
  , indent $ show stats.packageResults.partial <> " partially successful"
  , indent $ show (stats.packageNamesRemoved - stats.packageResults.fail) <> " omitted (no usable versions)"
  , indent $ show stats.packageResults.fail <> " fully failed"
  , indent "---"
  , formatErrors stats.packageErrors
  , ""
  , show stats.versionsProcessed <> " versions processed:"
  , indent $ show stats.versionResults.success <> " successful"
  , indent $ show stats.versionResults.fail <> " failed"
  , indent "---"
  , formatErrors stats.versionErrors
  , ""
  ]
  where
  indent contents = "  " <> contents
  formatErrors =
    String.joinWith "\n"
      <<< map (\(Tuple error count) -> indent (show count <> " " <> error))
      <<< Array.sortBy (\a b -> invert (compare (snd a) (snd b)))
      <<< Map.toUnfoldableUnordered

calculateImportStats :: LegacyRegistry -> ImportedIndex -> ImportStats
calculateImportStats legacyRegistry imported = do
  let
    registryIndex :: Map RawPackageName (Map RawVersion Manifest)
    registryIndex = Map.fromFoldableWith Map.union do
      Tuple name versions <- Map.toUnfoldable $ ManifestIndex.toMap imported.registryIndex
      Tuple version manifest <- Map.toUnfoldable versions
      let ref = unsafeFromJust (Map.lookup name imported.packageRefs >>= Map.lookup version)
      [ Tuple (RawPackageName (PackageName.print name)) (Map.singleton ref manifest) ]

    packagesProcessed =
      Map.size legacyRegistry

    packageNamesRemoved =
      Map.size imported.removedPackages

    packageResults = do
      let succeeded = Map.keys registryIndex
      let failedPackages = Map.keys imported.failedPackages
      let failedPackageVersions = Map.keys imported.failedVersions
      let both = partition (_ `Set.member` failedPackageVersions) (Array.fromFoldable succeeded)
      { success: Array.length both.no
      , partial: Array.length both.yes
      , fail: Set.size failedPackages
      }

    versionResults =
      { success: Foldable.sum (map Map.size (Map.values registryIndex))
      , fail: Foldable.sum (map Map.size (Map.values imported.failedVersions))
      }

    versionsProcessed =
      versionResults.success + versionResults.fail

    packageErrors =
      Array.foldl (\m error -> Map.insertWith (+) error 1 m) Map.empty
        $ map toKey
        $ Array.fromFoldable
        $ Map.values imported.failedPackages
      where
      toKey = _.error >>> case _ of
        InvalidPackageName -> "Invalid Package Name"
        InvalidPackageURL _ -> "Invalid Package URL"
        PackageURLRedirects _ -> "Package URL Redirects"
        CannotAccessRepo _ -> "Cannot Access Repo"
        DisabledPackage -> "Disabled Package"

    versionErrors =
      Array.foldl (\m error -> Map.insertWith (+) error 1 m) Map.empty
        $ Array.fromFoldable
        $ List.concatMap (map toKey <<< Map.values)
        $ Map.values imported.failedVersions
      where
      toKey = _.error >>> case _ of
        InvalidTag _ -> "Invalid Tag"
        DisabledVersion -> "Disabled Version"
        InvalidManifest err -> "Invalid Manifest (" <> innerKey err <> ")"
        UnregisteredDependencies _ -> "Unregistered Dependencies"

      innerKey = _.error >>> case _ of
        NoManifests -> "No Manifests"
        MissingLicense -> "Missing License"
        InvalidLicense _ -> "Invalid License"
        InvalidDependencies _ -> "Invalid Dependencies"

  { packagesProcessed
  , versionsProcessed
  , packageNamesRemoved
  , packageResults
  , versionResults
  , packageErrors
  , versionErrors
  }

legacyRepoParser :: Parser String Address
legacyRepoParser = do
  _ <- Parsing.Combinators.choice
    [ Parsing.String.string "https://github.com/"
    , Parsing.String.string "git://github.com/"
    , Parsing.String.string "git@github.com/"
    ]

  owner <- do
    let
      ownerChoice = Parsing.Combinators.choice
        [ Parsing.String.Basic.alphaNum
        , Parsing.String.char '-'
        ]
    Tuple chars _ <- Parsing.Combinators.Array.manyTill_ ownerChoice (Parsing.String.char '/')
    pure $ String.CodeUnits.fromCharArray chars

  repoWithSuffix <- String.CodeUnits.fromCharArray <$> Array.many Parsing.String.anyChar
  let repo = fromMaybe repoWithSuffix (String.stripSuffix (String.Pattern ".git") repoWithSuffix)

  pure { owner, repo }

fetchSpagoYaml :: forall r. Address -> RawVersion -> Run (GITHUB + LOG + EXCEPT String + r) (Maybe Manifest)
fetchSpagoYaml address ref = do
  eitherSpagoYaml <- GitHub.getContent address ref "spago.yaml"
  case eitherSpagoYaml of
    Left err -> do
      Log.debug $ "No spago.yaml found: " <> Octokit.printGitHubError err
      pure Nothing
    Right contents -> do
      Log.debug $ "Found spago.yaml file\n" <> contents
      case parseYaml SpagoYaml.spagoYamlCodec contents of
        Left error -> do
          Log.warn $ "Failed to parse spago.yaml file:\n" <> contents <> "\nwith errors:\n" <> error
          pure Nothing
        Right { package: Just { publish: Just { location: Just location } } }
          | location /= GitHub { owner: address.owner, repo: address.repo, subdir: Nothing } -> do
              Log.warn "spago.yaml file does not use the same location it was fetched from, this is disallowed..."
              pure Nothing
        Right config -> case SpagoYaml.spagoYamlToManifest config of
          Left err -> do
            Log.warn $ "Failed to convert parsed spago.yaml file to purs.json " <> contents <> "\nwith errors:\n" <> err
            pure Nothing
          Right manifest -> do
            Log.debug "Successfully converted a spago.yaml into a purs.json manifest"
            pure $ Just manifest

-- | Find the first compiler that can compile the package source code and
-- | installed resolutions from the given array of compilers. Begins with the
-- | latest compiler and works backwards to older compilers.
findFirstCompiler
  :: forall r
   . { compilers :: Array Version
     , manifest :: Manifest
     , resolutions :: Map PackageName Version
     , source :: FilePath
     , installed :: FilePath
     }
  -> Run (COMPILER_CACHE + STORAGE + LOG + AFF + EFFECT + r) (Either (Map Version CompilerFailure) Version)
findFirstCompiler { source, manifest, resolutions, compilers, installed } = do
  search <- Run.Except.runExcept $ for (Array.reverse (Array.sort compilers)) \target -> do
    result <- Cache.get API._compilerCache (API.Compilation manifest resolutions target) >>= case _ of
      Nothing -> do
        Log.info $ "Not cached, trying compiler " <> Version.print target
        workdir <- Tmp.mkTmpDir
        result <- Run.liftAff $ Purs.callCompiler
          { command: Purs.Compile { globs: [ Path.concat [ source, "src/**/*.purs" ], Path.concat [ installed, "*/src/**/*.purs" ] ] }
          , version: Just target
          , cwd: Just workdir
          }
        FS.Extra.remove workdir
        let cache = { result: map (const unit) result, target }
        Cache.put API._compilerCache (API.Compilation manifest resolutions target) cache
        pure cache.result
      Just cached ->
        pure cached.result

    case result of
      Left error -> pure $ Tuple target error
      Right _ -> Run.Except.throw target

  case search of
    Left worked -> pure $ Right worked
    Right others -> pure $ Left $ Map.fromFoldable others

type GroupedByCompilers =
  { packages :: Map PackageName Version
  , compilers :: NonEmptySet Version
  }

groupedByCompilersCodec :: CJ.Codec GroupedByCompilers
groupedByCompilersCodec = CJ.named "GroupedByCompilers" $ CJ.Record.object
  { compilers: CJ.Common.nonEmptySet Version.codec
  , packages: Internal.Codec.packageMap Version.codec
  }

-- | Given a set of package versions, determine the set of compilers that can be
-- | used for all packages.
compatibleCompilers :: Map PackageName Metadata -> Map PackageName Version -> Either (Array GroupedByCompilers) (NonEmptySet Version)
compatibleCompilers allMetadata resolutions = do
  let
    associated :: Array { name :: PackageName, version :: Version, compilers :: NonEmptyArray Version }
    associated = Map.toUnfoldableUnordered resolutions # Array.mapMaybe \(Tuple name version) -> do
      Metadata metadata <- Map.lookup name allMetadata
      published <- Map.lookup version metadata.published
      case published.compilers of
        Left _ -> Nothing
        Right compilers -> Just { name, version, compilers: compilers }

  case Array.uncons associated of
    Nothing ->
      Left []
    Just { head, tail: [] } ->
      Right $ NonEmptySet.fromFoldable1 head.compilers
    Just { head, tail } -> do
      let foldFn prev = Set.intersection prev <<< Set.fromFoldable <<< _.compilers
      case NonEmptySet.fromFoldable $ Array.foldl foldFn (Set.fromFoldable head.compilers) tail of
        -- An empty intersection means there are no shared compilers among the
        -- resolved dependencies.
        Nothing -> do
          let
            grouped :: Array (NonEmptyArray { name :: PackageName, version :: Version, compilers :: NonEmptyArray Version })
            grouped = Array.groupAllBy (compare `on` _.compilers) (Array.cons head tail)

            collect :: NonEmptyArray { name :: PackageName, version :: Version, compilers :: NonEmptyArray Version } -> GroupedByCompilers
            collect vals =
              { packages: Map.fromFoldable (map (\{ name, version } -> Tuple name version) vals)
              -- We've already grouped by compilers, so those must all be equal
              -- and we can take just the first value.
              , compilers: NonEmptySet.fromFoldable1 (NonEmptyArray.head vals).compilers
              }
          Left $ Array.foldl (\prev -> Array.snoc prev <<< collect) [] grouped

        Just set ->
          Right set

type IMPORT_CACHE r = (importCache :: Cache ImportCache | r)

_importCache :: Proxy "importCache"
_importCache = Proxy

-- | A key type for the storage cache. Only supports packages identified by
-- | their name and version.
data ImportCache :: (Type -> Type -> Type) -> Type -> Type
data ImportCache c a
  = ImportManifest PackageName RawVersion (c (Either VersionValidationError Manifest) a)
  | PublishFailure PackageName Version (c PublishError a)

instance Functor2 c => Functor (ImportCache c) where
  map k (ImportManifest name version a) = ImportManifest name version (map2 k a)
  map k (PublishFailure name version a) = PublishFailure name version (map2 k a)

instance MemoryEncodable ImportCache where
  encodeMemory = case _ of
    ImportManifest name (RawVersion version) next ->
      Exists.mkExists $ Key ("ImportManifest__" <> PackageName.print name <> "__" <> version) next
    PublishFailure name version next -> do
      Exists.mkExists $ Key ("PublishFailure__" <> PackageName.print name <> "__" <> Version.print version) next

instance FsEncodable ImportCache where
  encodeFs = case _ of
    ImportManifest name (RawVersion version) next -> do
      let codec = CJ.Common.either versionValidationErrorCodec Manifest.codec
      Exists.mkExists $ AsJson ("ImportManifest__" <> PackageName.print name <> "__" <> version) codec next
    PublishFailure name version next -> do
      let codec = publishErrorCodec
      Exists.mkExists $ AsJson ("PublishFailure__" <> PackageName.print name <> "__" <> Version.print version) codec next
