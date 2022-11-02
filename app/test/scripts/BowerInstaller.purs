-- | This script attempts to install all packages in the registry index using
-- | Bower, recording which packages have a successful run and which packages
-- | fail to install. This is used to generate the list of Bower packages that
-- | succeed in the integration tests for the registry solver.
module Test.Scripts.BowerInstaller where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Monad.Reader (ask, asks)
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Node.FS as FS.Extra
import Foreign.Tmp as Tmp
import Node.ChildProcess (Exit(..))
import Node.ChildProcess as ChildProcess
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Node.Process as Node.Process
import Registry.API as API
import Registry.App.LenientRange as LenientRange
import Registry.App.LenientVersion as LenientVersion
import Registry.Cache as Cache
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Prelude as Aff
import Registry.Range (Range)
import Registry.Range as Range
import Registry.RegistryM (RegistryM, readPackagesMetadata)
import Registry.RegistryM as RegistryM
import Registry.Schema (Location(..), Manifest(..), Metadata)
import Registry.Version (Version)
import Registry.Version as Version
import Sunde as Sunde

type BowerSolverResults = Map PackageName (Map Version BowerSolved)

-- The results for each package consist of the Bower solver running on the
-- Bowerfile, the Bower solver running on a manifest converted from the registry
-- index into a Bowerfile, and the parsed dependencies of the Bowerfile.
type BowerSolved =
  { bowerfileSolution :: Map PackageName Version
  , manifestSolution :: Map PackageName Version
  , dependencies :: Map PackageName Range
  }

main :: Effect Unit
main = launchAff_ do
  _ <- API.loadEnv

  FS.Extra.ensureDirectory API.scratchDir

  octokit <- liftEffect do
    mbToken <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
    token <- maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) mbToken
    GitHub.mkOctokit token

  cache <- Cache.useCache API.cacheDir

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
      , registry: Path.concat [ API.scratchDir, "registry" ]
      , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
      }

  RegistryM.runRegistryM env do
    API.fetchRegistry
    API.fetchRegistryIndex
    API.fillMetadataRef

    registryIndexPath <- asks _.registryIndex
    registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath
    metadata <- readPackagesMetadata

    let resultsPath = Path.concat [ API.scratchDir, "bower-solver-results" ]

    previousResults :: BowerSolverResults <- liftAff do
      log "Fetching thomashoneyman/bower-solver-results"
      fetchRepo { owner: "thomashoneyman", repo: "bower-solver-results" } resultsPath
      allContents <- FS.Aff.readdir resultsPath
      let files = filterMap (String.stripSuffix (String.Pattern ".json")) allContents
      log $ "Reading " <> show (Array.length files) <> " input files..."
      result <- for files \file -> do
        package <- case PackageName.parse file of
          Left err -> throwError $ Exception.error err
          Right res -> pure res
        versions <- Json.readJsonFile (Path.concat [ resultsPath, file <> ".json" ]) >>= case _ of
          Left err -> throwError $ Exception.error err
          Right versions -> pure versions
        pure (Tuple package versions)
      pure $ Map.fromFoldable result

    -- Install packages one-by-one, collecting which ones succeed and which ones
    -- fail. Note: this will take forever to complete, so feel free to step away
    -- for a few hours.
    log $ "Solving registry packages..."
    allResults <- runBowerSolver registryIndex metadata previousResults

    let
      bowerSolverResults :: BowerSolverResults
      bowerSolverResults = map Map.catMaybes allResults

    forWithIndex_ bowerSolverResults \package versions ->
      -- NOTE: These results are ignored to keep the file sizes in the repo down
      -- but they can be seen at https://github.com/thomashoneyman/bower-solver-results
      --
      -- If you would like to add your new results to the repo, please open a PR
      liftAff $ Json.writeJsonFile (Path.concat [ resultsPath, PackageName.print package <> ".json" ]) versions

    log "Done!"

data BowerInstallType = ManifestInstall | BowerfileInstall

derive instance Eq BowerInstallType

printBowerInstallType :: BowerInstallType -> String
printBowerInstallType = case _ of
  ManifestInstall -> "manifest-install"
  BowerfileInstall -> "bowerfile-install"

-- | Attempt to solve the entire registry index, relying on previous solver
-- | results if available.
runBowerSolver :: RegistryIndex -> Map PackageName Metadata -> BowerSolverResults -> RegistryM (Map PackageName (Map Version (Maybe BowerSolved)))
runBowerSolver index metadata previousResults =
  forWithIndex index \package versions ->
    forWithIndex versions \version (Manifest manifest) ->
      case Map.lookup package previousResults >>= Map.lookup version of
        Nothing -> map hush $ Except.runExceptT do
          originalBowerfile <- case Map.lookup package metadata of
            Just { location: GitHub { owner, repo }, published } | Just { ref } <- Map.lookup version published -> do
              { octokit, cache } <- ask
              bowerfile <- Except.mapExceptT (liftAff <<< map (lmap GitHub.printGitHubError)) $ GitHub.getContent octokit cache { owner, repo } ref "bower.json"
              pure $ JsonRepair.tryRepair bowerfile
            _ -> unsafeCrashWith $ Array.fold [ PackageName.print package, " not in metadata." ]

          dependencies <- case Json.parseJson originalBowerfile of
            Left err -> Except.throwError err
            Right ({ dependencies } :: { dependencies :: Map String String }) -> either Except.throwError pure do
              parsedNames <- traverseKeys (PackageName.parse <<< stripPureScriptPrefix) dependencies
              parsedRanges <- traverse (map LenientRange.range <<< LenientRange.parse) parsedNames
              pure parsedRanges

          bowerfileSolution <- Except.ExceptT do
            maybeRes <- runBowerInstall BowerfileInstall package version originalBowerfile
            pure $ case maybeRes of
              Nothing -> Left $ "Failed to install from bowerfile:\n" <> originalBowerfile
              Just result -> Right result

          manifestSolution <- Except.ExceptT do
            let
              bowerfile = Json.printJson { name: PackageName.print package, dependencies: bowerDependencies }
              bowerDependencies = mapWithIndex bowerDependency manifest.dependencies
              bowerDependency depName range = case Map.lookup depName metadata of
                Just { location: GitHub dep } -> "https://github.com/" <> dep.owner <> "/" <> dep.repo <> ".git#" <> Range.print range
                _ -> unsafeCrashWith $ Array.fold [ PackageName.print depName, " not in metadata." ]

            maybeRes <- runBowerInstall ManifestInstall package version bowerfile
            pure $ case maybeRes of
              Nothing -> Left $ "Failed to install from manifest-produced bowerfile:\n" <> bowerfile
              Just result -> Right result

          pure { dependencies, bowerfileSolution, manifestSolution }

        Just solutions ->
          pure (Just solutions)

runBowerInstall :: BowerInstallType -> PackageName -> Version -> String -> RegistryM (Maybe (Map PackageName Version))
runBowerInstall installType name version contents = do
  tmp <- liftEffect Tmp.mkTmpDir
  { cache } <- ask
  let key = "bower-solved-" <> printBowerInstallType installType <> "-" <> PackageName.print name <> "__" <> Version.print version
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
      Left err -> throwError $ Exception.error err
      Right res -> pure res

    version <- case LenientVersion.parse rawVersion of
      Left err -> throwError $ Exception.error err
      Right res -> pure $ LenientVersion.version res

    pure (Tuple package version)

  pure $ Map.fromFoldable result

fetchRepo :: GitHub.Address -> FilePath -> Aff Unit
fetchRepo address path = liftEffect (FS.Sync.exists path) >>= case _ of
  true -> do
    Except.runExceptT (Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just path)) >>= case _ of
      Left err -> Aff.throwError $ Exception.error err
      Right _ -> pure unit
  _ -> do
    Except.runExceptT (Git.runGit [ "clone", "https://github.com/" <> address.owner <> "/" <> address.repo <> ".git", path ] Nothing) >>= case _ of
      Left err -> Aff.throwError $ Exception.error err
      Right _ -> pure unit
