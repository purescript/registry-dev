module Registry.Scripts.CompilerVersions where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Data.Tuple (uncurry)
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
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tmp as Tmp
import Registry.Internal.Format as Internal.Format
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT, rethrow, throw)
import Run.Except as Except

data InputMode
  = File FilePath
  | Package PackageName Version
  | AllPackages

parser :: ArgParser InputMode
parser = Arg.choose "input (--file or --package or --all)"
  [ Arg.argument [ "--file" ]
      """Verify packages from a JSON file like: [ "prelude", "console" ]"""
      # Arg.unformat "FILE_PATH" pure
      # map File
  , Arg.argument [ "--package" ]
      "Verify the indicated package"
      # Arg.unformat "NAME@VERSION" parsePackage
      # map (uncurry Package)
  , Arg.flag [ "--all" ] "Verify all packages" $> AllPackages
  ]
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

  case arguments of
    File _ -> Console.log "Unsupported at this time." *> liftEffect (Process.exit 1)
    Package package version -> interpret $ determineCompilerVersionsForPackage package version
    AllPackages -> Console.log "Unsupported at this time." *> liftEffect (Process.exit 1)

determineCompilerVersionsForPackage :: forall r. PackageName -> Version -> Run (AFF + EFFECT + REGISTRY + EXCEPT String + LOG + STORAGE + r) Unit
determineCompilerVersionsForPackage package version = do
  allManifests <- map ManifestIndex.toMap Registry.readAllManifests
  compilerVersions <- PursVersions.pursVersions
  Log.debug $ "Checking Manifest Index for " <> formatPackageVersion package version
  Manifest { dependencies } <- rethrow $ (note "Invalid Version" <<< Map.lookup version <=< note "Invalid PackageName" <<< Map.lookup package) allManifests
  unless (Map.isEmpty dependencies) do
    Log.error "Cannot check package that has dependencies."
    throw "Cannot check package that has dependencies."
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
        , version: Just (Version.print compiler)
        , cwd: Just tmp
        }

      case result of
        Left _ -> do
          Log.debug $ "Failed to compile " <> formatPackageVersion package version <> " with purs@" <> Version.print compiler
          pure false
        Right _ -> do
          Log.debug $ "Compiled " <> formatPackageVersion package version <> " with purs@" <> Version.print compiler
          pure true

    goCompilerVersions { first: mbFirst, last: mbLast } compilers = case Array.uncons compilers of
      Nothing -> do -- no more compiler versions to check, construct range
        case mbFirst, mbLast of
          Just first, Just last -> pure $ Range.mk first (Version.bumpPatch last)
          _, _ -> pure Nothing
      Just { head, tail } -> do -- more compiler versions to check, may be done.
        case mbFirst of
          Nothing -> do -- searching for first compiler version
            supported <- checkCompiler head
            if supported then
              goCompilerVersions { first: Just head, last: Just head } tail
            else
              goCompilerVersions { first: Nothing, last: Nothing } tail
          Just first -> do -- already found first, need to check if contiguous compiler versions are valid
            supported <- checkCompiler head
            if supported then
              goCompilerVersions { first: Just first, last: Just head } tail
            else
              goCompilerVersions { first: Just first, last: mbLast } []

  mbRange <- goCompilerVersions { first: Nothing, last: Nothing } (Array.sort (NEA.toArray compilerVersions))
  case mbRange of
    Nothing -> do
      Log.error $ "Could not find supported compiler versions for " <> formatPackageVersion package version
      Run.liftEffect $ Process.exit 1
    Just range ->
      Log.info $ "Found supported compiler versions for " <> formatPackageVersion package version <> ": " <> Range.print range
