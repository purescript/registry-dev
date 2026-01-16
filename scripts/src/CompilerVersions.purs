module Registry.Scripts.CompilerVersions where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Codec.JSON.Variant as CJ.Variant
import Data.Exists as Exists
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Semigroup.Foldable as Semigroup.Foldable
import Data.Set as Set
import Data.String as String
import Data.Variant as Variant
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.Cache (class FsEncodable, class MemoryEncodable, Cache, FsEncoding(..), MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tmp as Tmp
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Manifest (Manifest(..))
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Solver (DependencyIndex)
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

data TargetCompiler
  = AllCompilers
  | OneCompiler Version

derive instance Eq TargetCompiler

type Arguments =
  { package :: Maybe (Tuple PackageName Version)
  , compiler :: TargetCompiler
  }

parser :: ArgParser Arguments
parser = Arg.fromRecord
  { package: Arg.choose "input (--all-packages or --package)"
      [ Arg.flag [ "--all-packages" ] "Check compiler versions for all packages" $> Nothing
      , Arg.argument [ "--package" ]
          "Check compiler versions for specific package"
          # Arg.unformat "NAME@VERSION" parsePackage
          # map Just
      ]
  , compiler: Arg.choose "input (--all-compilers or --compiler)"
      [ Arg.flag [ "--all-compilers" ] "Check all compiler versions" $> AllCompilers
      , Arg.argument [ "--compiler" ]
          "Check compiler versions for specific package"
          # Arg.unformat "VERSION" Version.parse
          # map OneCompiler
      ]
  }
  where
  parsePackage :: String -> Either String (Tuple PackageName Version)
  parsePackage input = do
    let split = String.split (String.Pattern "@") input
    case Array.length split of
      0 -> Left "Expected package@version but received nothing."
      2 -> do
        rawPackage <- note "Unexpected error" (Array.index split 0)
        package <- lmap (append ("Failed to parse package name '" <> rawPackage <> "': ")) (PackageName.parse rawPackage)
        rawVersion <- note "Unexpected error" (Array.index split 1)
        version <- lmap (append ("Failed to parse version '" <> rawVersion <> "': ")) (Version.parse rawVersion)
        pure $ Tuple package version
      _ -> Left $ "Expected package@version but received an invalid format: " <> input

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "A script for determining the supported compiler versions for packages."
  arguments <- case Arg.parseArgs "compiler-versions" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.githubToken
  resourceEnv <- Env.lookupResourceEnv

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  -- GitHub
  octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl

  -- Registry
  debouncer <- Registry.newDebouncer
  repoLocks <- Registry.newRepoLocks
  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { write: Registry.ReadOnly
      , pull: Git.Autostash
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      , repoLocks
      , process: Registry.ScriptCompilerVersions
      }

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "compiler-versions-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]
  Console.log $ "Logs available at " <> logPath

  let
    interpret :: Run _ ~> Aff
    interpret =
      Except.catch (\error -> Run.liftEffect (Console.log error *> Process.exit' 1))
        >>> Registry.interpret (Registry.handle registryEnv)
        >>> Storage.interpret (Storage.handleReadOnly cache)
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Cache.interpret _compilationCache (Cache.handleFs cache :: Cache CompilationCache ~> _)
        >>> Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
        >>> Env.runResourceEnv resourceEnv
        >>> Run.runBaseAff'

  case arguments.package of
    Just (Tuple package version) ->
      interpret $ compilersForPackageVersion package version arguments.compiler
    Nothing -> do
      { failures, results } <- interpret $ compilersForAllPackages arguments.compiler
      let resultsDir = Path.concat [ scratchDir, "results" ]
      FS.Extra.ensureDirectory resultsDir
      let
        resultsFile = "compiler-versions-results-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".json"
        failuresFile = "compiler-versions-failures-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".json"
      writeJsonFile (Internal.Codec.packageMap (Internal.Codec.versionMap (CJ.array Version.codec))) (Path.concat [ resultsDir, resultsFile ]) results
      writeJsonFile (Internal.Codec.versionMap (CJ.array failureCodec)) (Path.concat [ resultsDir, failuresFile ]) failures

compilersForPackageVersion
  :: forall r
   . PackageName
  -> Version
  -> TargetCompiler
  -> Run (REGISTRY + STORAGE + LOG + EXCEPT String + AFF + EFFECT + r) Unit
compilersForPackageVersion package version target = do
  allManifests <- Registry.readAllManifests
  supportedCompilers <- PursVersions.pursVersions
  Log.debug $ "Checking manifest index for " <> formatPackageVersion package version
  Manifest { dependencies } <- Except.rethrow (note "No entry found in manifest index." (ManifestIndex.lookup package version allManifests))
  -- FIXME: Support packages with dependencies once we have compilers versions
  -- in metadata.
  unless (Map.isEmpty dependencies) do
    Log.error "Cannot check package that has dependencies."
    Except.throw "Cannot check package that has dependencies."

  tmp <- Run.liftEffect Tmp.mkTmpDir
  let formattedName = formatPackageVersion package version
  let extractedName = PackageName.print package <> "-" <> Version.print version
  let tarballName = extractedName <> ".tar.gz"
  let tarballPath = Path.concat [ tmp, tarballName ]
  let extractedPath = Path.concat [ tmp, extractedName ]
  let installPath = Path.concat [ tmp, formattedName ]

  Log.debug $ "Installing " <> formattedName
  Storage.download package version tarballPath
  Run.liftEffect $ Tar.extract { cwd: tmp, archive: tarballName }
  Run.liftAff do
    FS.Extra.remove tarballPath
    FS.Aff.rename extractedPath installPath

  Log.debug $ "Installed " <> formatPackageVersion package version
  Log.debug $ "Finding supported compiler versions for " <> formatPackageVersion package version

  let
    checkCompiler compiler = do
      Log.debug $ "Trying to compile " <> formatPackageVersion package version <> " with purs@" <> Version.print compiler
      result <- Run.liftAff $ Purs.callCompiler
        { command: Purs.Compile { globs: [ Path.concat [ formattedName, "src/**/*.purs" ] ] }
        , version: Just compiler
        , cwd: Just tmp
        }
      case result of
        Left _ -> do
          Log.debug $ "Failed to compile " <> formatPackageVersion package version <> " with purs@" <> Version.print compiler
          pure false
        Right _ -> do
          Log.debug $ "Compiled " <> formatPackageVersion package version <> " with purs@" <> Version.print compiler
          pure true

    goCompilerVersions supported compilers = case Array.uncons compilers of
      Nothing -> pure supported
      Just { head, tail } -> do
        success <- checkCompiler head
        if success then
          goCompilerVersions (supported <> [ head ]) tail
        else
          goCompilerVersions supported tail

  supported <- goCompilerVersions [] $ case target of
    AllCompilers -> NEA.toArray supportedCompilers
    OneCompiler compiler -> [ compiler ]

  if Array.null supported then do
    Log.error $ "Could not find supported compiler versions for " <> formatPackageVersion package version
    Run.liftEffect $ Process.exit' 1
  else
    Log.info $ "Found supported compiler versions for " <> formatPackageVersion package version <> ": " <> Array.intercalate ", " (map Version.print supported)

compilersForAllPackages :: forall r. TargetCompiler -> Run (COMPILATION_CACHE + REGISTRY + STORAGE + LOG + EXCEPT String + AFF + EFFECT + r) CompilerVersionResults
compilersForAllPackages target = do
  index <- Registry.readAllManifests
  let sortedManifests = Array.mapWithIndex Tuple (ManifestIndex.toSortedArray ManifestIndex.ConsiderRanges index)
  let manifestCount = Array.length sortedManifests
  compilersToCheck <- case target of
    AllCompilers -> PursVersions.pursVersions
    OneCompiler version -> pure (NEA.singleton version)
  supportedForVersion <- map Map.fromFoldable $ for compilersToCheck \compiler -> do
    Log.info $ "Starting checks for compiler " <> Version.print compiler
    Tuple compiler <$> Array.foldM (checkCompilation compiler manifestCount) { failures: [], results: Map.empty } sortedManifests

  let
    results = Map.fromFoldableWith (Map.unionWith append) do
      Tuple compiler supported <- Map.toUnfoldable (map _.results supportedForVersion)
      Tuple package versions <- Map.toUnfoldable supported
      Tuple version _ <- Map.toUnfoldable versions
      [ Tuple package (Map.singleton version [ compiler ]) ]

    failures = map _.failures supportedForVersion

  pure { results, failures }
  where
  -- Adds packages which compile with `version` to the `DependencyIndex`
  checkCompilation :: Version -> Int -> { failures :: Array Failure, results :: DependencyIndex } -> Tuple Int Manifest -> Run _ { failures :: Array Failure, results :: DependencyIndex }
  checkCompilation compiler total { failures: prevFailures, results: prevResults } (Tuple index manifest@(Manifest { name, version, dependencies })) = do
    let progress = fold [ "[", Version.print compiler, " ", show (1 + index), "/", show total, "]" ]
    Log.info $ progress <> " Checking " <> formatPackageVersion name version

    let
      successResult = { failures: prevFailures, results: Map.insertWith Map.union name (Map.singleton version dependencies) prevResults }
      failResult reason = { failures: prevFailures <> [ { name, version, reason } ], results: prevResults }
      runCheckWithCache prevCache = do
        Log.debug $ "Solving " <> PackageName.print name <> "@" <> Version.print version
        case Solver.solve prevResults dependencies of
          Left unsolvable -> do
            Log.debug $ "Could not solve " <> formatPackageVersion name version <> " with manifest " <> printJson Manifest.codec manifest
            Log.debug $ Semigroup.Foldable.foldMap1 (append "\n" <<< Solver.printSolverError) unsolvable
            Cache.put _compilationCache (CompileResult name version) $ case prevCache of
              Nothing -> { failed: Map.singleton compiler CannotSolve, succeeded: Set.empty }
              Just prev -> prev { failed = Map.insert compiler CannotSolve prev.failed }
            pure $ failResult CannotSolve
          Right resolutions -> do
            supported <- installAndBuildWithVersion compiler (Map.insert name version resolutions)
            case supported of
              Nothing -> do
                Log.debug $ "Including package version " <> formatPackageVersion name version
                Cache.put _compilationCache (CompileResult name version) $ case prevCache of
                  Nothing -> { failed: Map.empty, succeeded: Set.singleton compiler }
                  Just prev -> prev { succeeded = Set.insert compiler prev.succeeded }
                pure successResult
              Just reason -> do
                Log.debug $ "Skipping package version " <> formatPackageVersion name version
                Cache.put _compilationCache (CompileResult name version) $ case prevCache of
                  Nothing -> { failed: Map.singleton compiler reason, succeeded: Set.empty }
                  Just prev -> prev { failed = Map.insert compiler reason prev.failed }
                pure $ failResult reason

    Cache.get _compilationCache (CompileResult name version) >>= case _ of
      Just { failed } | Just reason <- Map.lookup compiler failed -> do
        Log.debug "Got failure from cache."
        pure $ failResult reason
      Just { succeeded } | Set.member compiler succeeded -> do
        Log.debug "Got success from cache."
        pure successResult
      cache -> runCheckWithCache cache

  installAndBuildWithVersion :: Version -> Map PackageName Version -> Run _ (Maybe FailureReason)
  installAndBuildWithVersion compiler resolutions = do
    tmp <- Tmp.mkTmpDir
    let dependenciesDir = Path.concat [ tmp, ".registry" ]
    FS.Extra.ensureDirectory dependenciesDir
    Log.debug $ "Created tmp dir for dependencies: " <> dependenciesDir
    let globs = [ Path.concat [ dependenciesDir, "*/src/**/*.purs" ] ]

    Log.debug "Downloading dependencies..."
    forWithIndex_ resolutions \name version -> do
      let
        filename = PackageName.print name <> "-" <> Version.print version <> ".tar.gz"
        filepath = Path.concat [ dependenciesDir, filename ]
      Storage.download name version filepath
      Tar.extract { cwd: dependenciesDir, archive: filename }
      Run.liftAff $ FS.Aff.unlink filepath

    Log.debug $ "Compiling with purs@" <> Version.print compiler <> " and globs " <> String.joinWith " " globs
    compilerOutput <- Run.liftAff $ Purs.callCompiler
      { command: Purs.Compile { globs }
      , version: Just compiler
      , cwd: Just tmp
      }

    FS.Extra.remove tmp

    case compilerOutput of
      Left (Purs.UnknownError error) -> do
        Log.error $ "Failed to compile because of an unknown compiler error: " <> error
        pure $ Just UnknownReason
      Left (Purs.MissingCompiler) ->
        Except.throw "Failed to compile because the compiler was not found."
      Left (Purs.CompilationError errors) -> do
        Log.debug $ "Failed to compile with purs@" <> Version.print compiler <> ": " <> Purs.printCompilerErrors errors
        pure $ Just CannotCompile
      Right _ -> do
        Log.debug $ "Successfully compiled with purs@" <> Version.print compiler
        pure Nothing

type Failure =
  { name :: PackageName
  , version :: Version
  , reason :: FailureReason
  }

failureCodec :: CJ.Codec Failure
failureCodec = CJ.named "Failure" $ CJ.Record.object
  { name: PackageName.codec
  , version: Version.codec
  , reason: failureReasonCodec
  }

type CompilerVersionResults =
  { results :: Map PackageName (Map Version (Array Version))
  , failures :: Map Version (Array Failure)
  }

data FailureReason
  = CannotSolve
  | CannotCompile
  | UnknownReason

derive instance Eq FailureReason

failureReasonCodec :: CJ.Codec FailureReason
failureReasonCodec = Profunctor.dimap toVariant fromVariant $ CJ.Variant.variantMatch
  { cannotSolve: Left unit
  , cannotCompile: Left unit
  , unknownReason: Left unit
  }
  where
  toVariant = case _ of
    CannotSolve -> Variant.inj (Proxy :: _ "cannotSolve") unit
    CannotCompile -> Variant.inj (Proxy :: _ "cannotCompile") unit
    UnknownReason -> Variant.inj (Proxy :: _ "unknownReason") unit

  fromVariant = Variant.match
    { cannotSolve: \_ -> CannotSolve
    , cannotCompile: \_ -> CannotCompile
    , unknownReason: \_ -> UnknownReason
    }

type CompilationResults =
  { failed :: Map Version FailureReason
  , succeeded :: Set Version
  }

compilationResultsCodec :: CJ.Codec CompilationResults
compilationResultsCodec = CJ.named "CompilationResults" $ CJ.Record.object
  { failed: Internal.Codec.versionMap failureReasonCodec
  , succeeded: CJ.Common.set Version.codec
  }

-- | A key type for caching compilation results
data CompilationCache (c :: Type -> Type -> Type) a = CompileResult PackageName Version (c CompilationResults a)

instance Functor2 c => Functor (CompilationCache c) where
  map k = case _ of
    CompileResult name version a -> CompileResult name version (map2 k a)

instance MemoryEncodable CompilationCache where
  encodeMemory = case _ of
    CompileResult name version next ->
      Exists.mkExists $ Key ("CompileResult__" <> PackageName.print name <> "-" <> Version.print version) next

instance FsEncodable CompilationCache where
  encodeFs = case _ of
    CompileResult name version next ->
      Exists.mkExists $ AsJson ("CompileResult__" <> PackageName.print name <> "-" <> Version.print version) compilationResultsCodec next

type COMPILATION_CACHE r = (compilationCache :: Cache CompilationCache | r)

_compilationCache :: Proxy "compilationCache"
_compilationCache = Proxy
