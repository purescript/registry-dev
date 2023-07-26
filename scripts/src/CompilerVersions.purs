module Registry.Scripts.CompilerVersions where

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Function (flip)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Semigroup.Foldable as Foldable
import Data.Set as Set
import Data.String as String
import Data.Tuple (uncurry)
import Debug (traceM)
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Prelude (type (+), type (~>), Aff, Effect, Either(..), LogVerbosity(..), Map, Maybe(..), PackageName, Tuple(..), Unit, Version, append, bind, discard, for, forWithIndex_, for_, formatPackageVersion, launchAff_, liftEffect, lmap, map, note, nowUTC, pure, scratchDir, unless, (#), ($), ($>), (*>), (<$>), (<<<), (<=<), (<>), (>>>))
import Registry.App.Prelude as Json
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tmp as Tmp
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

type Arguments =
  { package :: Maybe (Tuple PackageName Version)
  , compiler :: Maybe Version
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
      [ Arg.flag [ "--all-compilers" ] "Check all compiler versions" $> Nothing
      , Arg.argument [ "--compiler" ]
          "Check compiler versions for specific package"
          # Arg.unformat "VERSION" Version.parse
          # map Just
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
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.githubToken

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  -- GitHub
  octokit <- Octokit.newOctokit token

  -- Registry
  debouncer <- Registry.newDebouncer
  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { write: Registry.ReadOnly
      , pull: Git.Autostash
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
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
      Except.catch (\error -> Run.liftEffect (Console.log error *> Process.exit 1))
        >>> Registry.interpret (Registry.handle registryEnv)
        >>> Storage.interpret (Storage.handleReadOnly cache)
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
        >>> Run.runBaseAff'

  case arguments.package of
    Just (Tuple package version) -> interpret $ determineCompilerVersionsForPackage package version arguments.compiler
    Nothing -> do
      supportedVersions <- interpret $ determineAllCompilerVersions arguments.compiler
      for_ (Map.toUnfoldable supportedVersions :: Array _) \(Tuple package compilers) ->
        traceM $ uncurry formatPackageVersion package <> ": " <> Array.intercalate ", " (map Version.print (Array.sort compilers))

determineCompilerVersionsForPackage :: forall r. PackageName -> Version -> Maybe Version -> Run (AFF + EFFECT + REGISTRY + EXCEPT String + LOG + STORAGE + r) Unit
determineCompilerVersionsForPackage package version mbCompiler = do
  allManifests <- map ManifestIndex.toMap Registry.readAllManifests
  compilerVersions <- PursVersions.pursVersions
  Log.debug $ "Checking Manifest Index for " <> formatPackageVersion package version
  Manifest { dependencies } <- Except.rethrow $ (note "Invalid Version" <<< Map.lookup version <=< note "Invalid PackageName" <<< Map.lookup package) allManifests
  unless (Map.isEmpty dependencies) do
    Log.error "Cannot check package that has dependencies."
    Except.throw "Cannot check package that has dependencies."
  tmp <- Run.liftAff Tmp.mkTmpDir
  let formattedName = formatPackageVersion package version
  let extractedName = PackageName.print package <> "-" <> Version.print version
  let tarballName = extractedName <> ".tar.gz"
  let tarballPath = Path.concat [ tmp, tarballName ]
  let extractedPath = Path.concat [ tmp, extractedName ]
  let installPath = Path.concat [ tmp, formattedName ]
  Log.debug $ "Installing " <> formattedName
  Storage.download package version tarballPath
  Run.liftAff do
    Tar.extract { cwd: tmp, archive: tarballName }
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

  supported <- goCompilerVersions [] (Maybe.maybe (Array.sort (NEA.toArray compilerVersions)) Array.singleton mbCompiler)

  if Array.null supported then do
    Log.error $ "Could not find supported compiler versions for " <> formatPackageVersion package version
    Run.liftEffect $ Process.exit 1
  else
    Log.info $ "Found supported compiler versions for " <> formatPackageVersion package version <> ": " <> Array.intercalate ", " (map Version.print supported)

determineAllCompilerVersions :: forall r. Maybe Version -> Run (AFF + EFFECT + REGISTRY + EXCEPT String + LOG + STORAGE + r) (Map (Tuple PackageName Version) (Array Version))
determineAllCompilerVersions mbCompiler = do
  allManifests <- ManifestIndex.toSortedArray ManifestIndex.ConsiderRanges <$> Registry.readAllManifests
  compilerVersions <- PursVersions.pursVersions
  let compilersToCheck = Maybe.maybe compilerVersions NEA.singleton mbCompiler
  supportedForVersion <- map Map.fromFoldable $ for compilersToCheck \compiler -> do
    Log.info $ "Starting checks for " <> Version.print compiler
    Tuple compiler <$> Array.foldM (checkCompilation compiler) Map.empty allManifests
  pure $ Map.fromFoldableWith append do
    Tuple compiler supported <- Map.toUnfoldable supportedForVersion
    Tuple package versions <- Map.toUnfoldable supported
    Tuple version _ <- Map.toUnfoldable versions
    [ Tuple (Tuple package version) [ compiler ] ]
  where
  -- Adds packages which compile with `version` to the `DependencyIndex`
  checkCompilation :: Version -> DependencyIndex -> Manifest -> Run _ DependencyIndex
  checkCompilation compiler prev manifest@(Manifest { name, version, dependencies }) = do
    Log.info $ flip foldMapWithIndex (map Map.keys prev) \package versions -> PackageName.print package <> String.joinWith ", " (map Version.print (Set.toUnfoldable versions)) <> " "
    Log.info $ "Checking " <> formatPackageVersion name version <> " with compiler purs@" <> Version.print compiler <> "..."
    Log.debug $ "Solving " <> PackageName.print name <> "@" <> Version.print version
    case Solver.solve prev dependencies of
      Left unsolvable -> do
        Log.debug $ "Could not solve " <> formatPackageVersion name version <> " with manifest " <> Json.printJson Manifest.codec manifest
        Log.debug $ Foldable.foldMap1 (append "\n" <<< Solver.printSolverError) unsolvable
        pure prev
      Right resolutions -> do
        supported <- installAndBuildWithVersion compiler (Map.insert name version resolutions)
        if supported then do
          Log.info $ "Including package version " <> formatPackageVersion name version
          pure $ Map.insertWith Map.union name (Map.singleton version dependencies) prev
        else do
          Log.info $ "Skipping package version " <> formatPackageVersion name version
          pure prev

  installAndBuildWithVersion :: Version -> Map PackageName Version -> Run _ Boolean
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
        pure false
      Left (Purs.MissingCompiler) ->
        Except.throw "Failed to compile because the compiler was not found."
      Left (Purs.CompilationError errors) -> do
        Log.debug $ "Failed to compile with purs@" <> Version.print compiler <> ": " <> Purs.printCompilerErrors errors
        pure false
      Right _ -> do
        Log.debug $ "Successfully compiled with purs@" <> Version.print compiler
        pure true
