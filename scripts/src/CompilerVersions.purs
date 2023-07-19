module Registry.Scripts.CompilerVersions where

import Registry.App.Prelude

import Data.Array as Array
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.License as License
import Registry.Location as Location
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (toSortedArray)
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Solver as Solver
import Registry.Version as Version
import Run (Run, EFFECT)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

main :: Effect Unit
main = launchAff_ do
  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken

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
  let logFile = "verify-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]
  Console.log $ "Logs available at " <> logPath

  solveCompilerVersions
    # Except.catch (\error -> Run.liftEffect (Console.log error *> Process.exit 1))
    # Registry.interpret (Registry.handle registryEnv)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
    # Run.runBaseAff'

solveCompilerVersions :: forall r. Run (LOG + EFFECT + REGISTRY + EXCEPT String + r) Unit
solveCompilerVersions = do
  --allMetadata <- Registry.readAllMetadata
  allManifests <- Registry.readAllManifests
  let
    compilerPackage = unsafeFromRight $ PackageName.parse "compiler"
    compilerVersions = map (unsafeFromRight <<< Version.parse)
      [ "0.13.0"
      , "0.13.2"
      , "0.13.3"
      , "0.13.4"
      , "0.13.5"
      , "0.13.6"
      , "0.13.8"
      , "0.14.0"
      , "0.14.1"
      , "0.14.2"
      , "0.14.3"
      , "0.14.4"
      , "0.14.5"
      , "0.14.6"
      , "0.14.7"
      , "0.14.8"
      , "0.14.9"
      , "0.15.0"
      , "0.15.2"
      , "0.15.3"
      , "0.15.4"
      , "0.15.5"
      , "0.15.6"
      , "0.15.7"
      , "0.15.8"
      , "0.15.9"
      , "0.15.10"
      ]

    compilerManifests =  do
      let
        go :: Version -> Maybe (Tuple Version Manifest)
        go version = do
          license <- hush $ License.parse "MIT"
          pure $ Tuple version $ Manifest
            { name: compilerPackage
            , version
            , license
            , location: Location.GitHub { owner: "purescript", repo: "purescript", subdir: Nothing }
            , owners: Nothing
            , description: Nothing
            , files: Nothing
            , dependencies: Map.empty
            }

      Map.fromFoldable
        [ Tuple compilerPackage $ Map.fromFoldable $ Array.mapMaybe go compilerVersions
        ]

  newManifestsRef <- liftEffect $ Ref.new compilerManifests

  for_ (toSortedArray allManifests) \manifest -> do
    let
      getDependencies = _.dependencies <<< un Manifest

      insertCompilerDependency :: Manifest -> Version ->  Manifest
      insertCompilerDependency (Manifest manifest) version = Manifest $ manifest
        { dependencies = Map.insert compilerPackage (unsafeFromJust (Range.mk version (Version.bumpMinor version))) manifest.dependencies
        }

    newManifests <- liftEffect $ Ref.read newManifestsRef
    Log.info "here"
    pure $ Solver.solve (map (map getDependencies) newManifests) (getDependencies manifest)

  pure unit
