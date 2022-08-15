-- | This script attempts to install all packages in the registry index using
-- | Bower, recording which packages have a successful run and which packages
-- | fail to install. This is used to generate the list of Bower packages that
-- | succeed in the integration tests for the registry solver.
module Registry.Scripts.BowerInstaller where

import Registry.Prelude

import Control.Monad.Reader (ask, asks)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Dotenv as Dotenv
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Foreign.Tmp as Tmp
import Node.ChildProcess (Exit(..))
import Node.ChildProcess as ChildProcess
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Node.Process
import Parsing as Parsing
import Registry.API as API
import Registry.Cache as Cache
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM, readPackagesMetadata)
import Registry.RegistryM as RegistryM
import Registry.Schema (Location(..), Manifest(..), Metadata)
import Registry.Version (Version)
import Registry.Version as Version
import Sunde as Sunde

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile

  octokit <- liftEffect do
    mbToken <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
    token <- maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) mbToken
    GitHub.mkOctokit token

  cache <- Cache.useCache

  let
    env :: RegistryM.Env
    env =
      { comment: \comment -> log ("[COMMENT] " <> comment)
      , closeIssue: log "Running locally, not closing issue..."
      , commitMetadataFile: \_ _ -> unsafeCrashWith "Should not commit in bower-installer"
      , commitIndexFile: \_ _ -> unsafeCrashWith "Should not push to registry index in bower-installer."
      , commitPackageSetFile: \_ _ -> unsafeCrashWith "Should not modify package set in bower-installer."
      , uploadPackage: \_ -> unsafeCrashWith "Should not upload anything in bower-installer."
      , deletePackage: \_ -> unsafeCrashWith "Should not delete anything in bower-installer."
      , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
      , cache
      , octokit
      , username: ""
      , registry: Path.concat [ "..", "registry" ]
      , registryIndex: Path.concat [ "..", "registry-index" ]
      }

  RegistryM.runRegistryM env do
    API.fetchRegistry
    API.fetchRegistryIndex
    API.fillMetadataRef

    registryIndexPath <- asks _.registryIndex
    registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath
    metadata <- readPackagesMetadata

    -- Install packages one-by-one, collecting which ones succeed and which ones
    -- fail. Note: this will take forever to complete, so feel free to step away
    -- for a few hours.
    bowerSolverResults <- runBowerSolver registryIndex metadata

    forWithIndex_ (map Map.catMaybes bowerSolverResults) \package versions ->
      -- NOTE: These results are ignored to keep the file sizes in the repo down
      -- but they can be seen at https://github.com/thomashoneyman/bower-solver-results
      liftAff $ Json.writeJsonFile (Path.concat [ "bower-solver-results", PackageName.print package <> ".json" ]) versions

    log "Done!"

runBowerSolver :: RegistryIndex -> Map PackageName Metadata -> RegistryM (Map PackageName (Map Version (Maybe (Map PackageName Version))))
runBowerSolver index metadata =
  forWithIndex index \package versions ->
    forWithIndex versions \version (Manifest { dependencies }) -> do
      let
        bowerfile = Json.printJson { name: PackageName.print package, dependencies: bowerDependencies }
        bowerDependencies = mapWithIndex bowerDependency dependencies
        bowerDependency depName range = case Map.lookup depName metadata of
          Just { location: GitHub dep } -> "https://github.com/" <> dep.owner <> "/" <> dep.repo <> ".git#" <> Version.printRange range
          _ -> unsafeCrashWith $ Array.fold [ PackageName.print depName, " not in metadata." ]

      runBowerInstall package version bowerfile

-- It would be even better to record bower resolutions, but it's a little hairy:
-- Record bower resolutions, not just success / failure?
runBowerInstall :: PackageName -> Version -> String -> RegistryM (Maybe (Map PackageName Version))
runBowerInstall name version contents = do
  tmp <- liftEffect Tmp.mkTmpDir
  { cache } <- ask
  let key = "bower-solver__" <> PackageName.print name <> "__" <> Version.printVersion version
  liftEffect (Cache.readJsonEntry key cache) >>= case _ of
    Left _ -> do
      log key

      let
        backoff' action = withBackoff
          { delay: Milliseconds 10_000.0
          , action
          , shouldCancel: \_ -> pure true
          , shouldRetry: \attempt -> if attempt > 2 then pure Nothing else pure (Just action)
          }

      maybeResult <- liftAff $ backoff' do
        FS.Aff.writeTextFile UTF8 (Path.concat [ tmp, "bower.json" ]) contents
        Sunde.spawn { cmd: "bower", stdin: Nothing, args: [ "install", "--production", "--force-latest", "--config.interactive=false" ] } (ChildProcess.defaultSpawnOptions { cwd = Just tmp })

      parsed <- case maybeResult of
        Nothing -> pure Nothing
        Just { exit: Normally 0 } -> do
          resolutions <- liftAff $ try $ readResolutions tmp
          pure $ hush resolutions
        Just { stdout, stderr } -> do
          log (Array.fold [ String.trim stdout, String.trim stderr ])
          pure Nothing

      liftEffect (Cache.writeJsonEntry key parsed cache)
      pure parsed

    Right { value } ->
      pure value

-- case value of
--   Nothing -> liftEffect (cache.remove key) *> runBowerInstall name version contents
--   otherwise -> pure otherwise

readResolutions :: FilePath -> Aff (Map PackageName Version)
readResolutions tmp = do
  let components = Path.concat [ tmp, "bower_components" ]
  FS.Extra.ensureDirectory components
  paths <- FS.Aff.readdir components
  result <- for paths \dir -> do
    { version: rawVersion } :: { version :: String } <- Json.readJsonFile (Path.concat [ components, dir, ".bower.json" ]) >>= case _ of
      Left err -> throwError $ Exception.error err
      Right val -> pure val

    package <- case PackageName.parse (stripPureScriptPrefix dir) of
      Left err -> throwError $ Exception.error $ Parsing.parseErrorMessage err
      Right res -> pure res

    version <- case Version.parseVersion Version.Lenient rawVersion of
      Left err -> throwError $ Exception.error $ Parsing.parseErrorMessage err
      Right res -> pure res

    pure (Tuple package version)

  pure $ Map.fromFoldable result
