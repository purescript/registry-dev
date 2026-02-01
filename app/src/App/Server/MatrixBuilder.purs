module Registry.App.Server.MatrixBuilder
  ( BuildPlanEntry
  , checkIfNewCompiler
  , installBuildPlan
  , printCompilerFailure
  , readCompilerIndex
  , resolutionsToBuildPlan
  , runMatrixJob
  , solveForAllCompilers
  , solveDependantsForCompiler
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.API.V1 (MatrixJobData)
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Tmp as Tmp
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 (Sha256)
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

runMatrixJob :: forall r. MatrixJobData -> Run (REGISTRY + STORAGE + LOG + AFF + EFFECT + EXCEPT String + r) (Map PackageName Range)
runMatrixJob { compilerVersion, packageName, packageVersion, payload: buildPlan } = do
  workdir <- Tmp.mkTmpDir
  let installed = Path.concat [ workdir, ".registry" ]
  FS.Extra.ensureDirectory installed

  -- Read metadata to get integrity info for each package in the build plan
  buildPlanWithIntegrity <- resolutionsToBuildPlan
    (Map.insert packageName packageVersion buildPlan)

  installBuildPlan buildPlanWithIntegrity installed
  result <- Purs.compile
    { globs: [ Path.concat [ installed, "*/src/**/*.purs" ] ]
    , version: Just compilerVersion
    , cwd: Just workdir
    }
  FS.Extra.remove workdir
  case result of
    Left err -> do
      Log.info $ Array.fold
        [ "Compilation failed with compiler " <> Version.print compilerVersion
        , ":\n"
        , printCompilerFailure compilerVersion err
        ]
      Except.throw $ "Compilation failed with compiler " <> Version.print compilerVersion
    Right _ -> do
      Log.info $ "Compilation succeeded with compiler " <> Version.print compilerVersion

      Registry.readMetadata packageName >>= case _ of
        Nothing -> do
          Log.error $ "No existing metadata for " <> PackageName.print packageName
          Except.throw $ "No metadata found for " <> PackageName.print packageName
        Just (Metadata metadata) -> do
          let
            metadataWithCompilers = metadata
              { published = Map.update
                  ( \publishedMetadata@{ compilers } ->
                      Just $ publishedMetadata { compilers = NonEmptySet.toUnfoldable1 $ NonEmptySet.fromFoldable1 $ NonEmptyArray.cons compilerVersion compilers }
                  )
                  packageVersion
                  metadata.published
              }
          Registry.writeMetadata packageName (Metadata metadataWithCompilers)
          Log.debug $ "Wrote new metadata " <> printJson Metadata.codec (Metadata metadataWithCompilers)

          Log.info "Wrote completed metadata to the registry!"
          Registry.readManifest packageName packageVersion >>= case _ of
            Just (Manifest manifest) -> pure manifest.dependencies
            Nothing -> do
              Log.error $ "No existing metadata for " <> PackageName.print packageName <> "@" <> Version.print packageVersion
              Except.throw $ "No manifest found for " <> PackageName.print packageName <> "@" <> Version.print packageVersion

-- TODO feels like we should be doing this at startup and use the cache instead
-- of reading files all over again
readCompilerIndex :: forall r. Run (REGISTRY + AFF + EXCEPT String + r) Solver.CompilerIndex
readCompilerIndex = do
  metadata <- Registry.readAllMetadata
  manifests <- Registry.readAllManifests
  allCompilers <- PursVersions.pursVersions
  pure $ Solver.buildCompilerIndex allCompilers manifests metadata

-- | A build plan entry with integrity information for verification.
type BuildPlanEntry = { version :: Version, hash :: Sha256, bytes :: Number }

-- | Install all dependencies indicated by the build plan to the specified
-- | directory. Packages will be installed at 'dir/package-name-x.y.z'.
installBuildPlan :: forall r. Map PackageName BuildPlanEntry -> FilePath -> Run (STORAGE + LOG + AFF + EXCEPT String + r) Unit
installBuildPlan resolutions dependenciesDir = do
  Run.liftAff $ FS.Extra.ensureDirectory dependenciesDir
  -- We fetch every dependency at its resolved version, unpack the tarball, and
  -- store the resulting source code in a specified directory for dependencies.
  forWithIndex_ resolutions \name { version, hash, bytes } -> do
    let
      -- This filename uses the format the directory name will have once
      -- unpacked, ie. package-name-major.minor.patch
      filename = PackageName.print name <> "-" <> Version.print version <> ".tar.gz"
      filepath = Path.concat [ dependenciesDir, filename ]
    Storage.download name version filepath { hash, bytes }
    Run.liftAff (Aff.attempt (Tar.extract { cwd: dependenciesDir, archive: filename })) >>= case _ of
      Left error -> do
        Log.error $ "Failed to unpack " <> filename <> ": " <> Aff.message error
        Except.throw "Failed to unpack dependency tarball, cannot continue."
      Right _ ->
        Log.debug $ "Unpacked " <> filename
    Run.liftAff $ FS.Aff.unlink filepath
    Log.debug $ "Installed " <> formatPackageVersion name version

-- | Convert resolutions (Map PackageName Version) to build plan entries using metadata.
-- | Fetches metadata for each package as needed.
resolutionsToBuildPlan :: forall r. Map PackageName Version -> Run (REGISTRY + EXCEPT String + r) (Map PackageName BuildPlanEntry)
resolutionsToBuildPlan resolutions =
  forWithIndex resolutions \name version -> do
    maybeMetadata <- Registry.readMetadata name
    case maybeMetadata of
      Nothing -> Except.throw $ "No metadata found for package " <> PackageName.print name
      Just (Metadata meta) -> case Map.lookup version meta.published of
        Nothing -> Except.throw $ "Version " <> Version.print version <> " not found in metadata for " <> PackageName.print name
        Just { hash, bytes } -> pure { version, hash, bytes }

printCompilerFailure :: Version -> CompilerFailure -> String
printCompilerFailure compiler = case _ of
  MissingCompiler -> Array.fold
    [ "Compilation failed because the build plan compiler version "
    , Version.print compiler
    , " is not supported. Please try again with a different compiler."
    ]
  CompilationError errs -> String.joinWith "\n"
    [ "Compilation failed because the build plan does not compile with version " <> Version.print compiler <> " of the compiler:"
    , "```"
    , Purs.printCompilerErrors errs
    , "```"
    ]
  UnknownError err -> String.joinWith "\n"
    [ "Compilation failed with version " <> Version.print compiler <> " because of an error :"
    , "```"
    , err
    , "```"
    ]

type MatrixSolverData =
  { compilerIndex :: Solver.CompilerIndex
  , compiler :: Version
  , name :: PackageName
  , version :: Version
  , dependencies :: Map PackageName Range
  }

type MatrixSolverResult =
  { name :: PackageName
  , version :: Version
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

solveForAllCompilers :: forall r. MatrixSolverData -> Run (AFF + EXCEPT String + LOG + r) (Set MatrixSolverResult)
solveForAllCompilers { compilerIndex, name, version, compiler, dependencies } = do
  -- remove the compiler we tested with from the set of all of them
  compilers <- (Array.filter (_ /= compiler) <<< NonEmptyArray.toArray) <$> PursVersions.pursVersions
  newJobs <- for compilers \target -> do
    Log.debug $ "Trying compiler " <> Version.print target <> " for package " <> PackageName.print name
    case Solver.solveWithCompiler (Range.exact target) compilerIndex dependencies of
      Left _solverErrors -> do
        Log.info $ "Failed to solve with compiler " <> Version.print target
        -- Log.debug $ Solver.printSolverError solverErrors
        pure Nothing
      Right (Tuple solvedCompiler resolutions) -> case solvedCompiler == target of
        true -> do
          Log.debug $ "Solved with compiler " <> Version.print solvedCompiler
          pure $ Just { compiler: target, resolutions, name, version }
        false -> do
          Log.debug $ Array.fold
            [ "Produced a compiler-derived build plan that selects a compiler ("
            , Version.print solvedCompiler
            , ") that differs from the target compiler ("
            , Version.print target
            , ")."
            ]
          pure Nothing
  pure $ Set.fromFoldable $ Array.catMaybes newJobs

solveDependantsForCompiler :: forall r. MatrixSolverData -> Run (EXCEPT String + LOG + REGISTRY + r) (Set MatrixSolverResult)
solveDependantsForCompiler { compilerIndex, name, version, compiler } = do
  manifestIndex <- Registry.readAllManifests
  let dependentManifests = ManifestIndex.dependants manifestIndex name version
  newJobs <- for dependentManifests \(Manifest manifest) -> do
    -- we first verify if we have already attempted this package with this compiler,
    -- either in the form of having it in the metadata already, or as a failed compilation
    -- (i.e. if we find compilers in the metadata for this version we only check this one
    -- if it's newer, because all the previous ones have been tried)
    shouldAttemptToCompile <- Registry.readMetadata manifest.name >>= case _ of
      Nothing -> pure false
      Just metadata -> pure $ case Map.lookup version (un Metadata metadata).published of
        Nothing -> false
        Just { compilers } -> any (_ > compiler) compilers
    case shouldAttemptToCompile of
      false -> pure Nothing
      true -> do
        -- if all good then run the solver
        Log.debug $ "Trying compiler " <> Version.print compiler <> " for package " <> PackageName.print manifest.name
        case Solver.solveWithCompiler (Range.exact compiler) compilerIndex manifest.dependencies of
          Left _solverErrors -> do
            Log.info $ "Failed to solve with compiler " <> Version.print compiler
            -- Log.debug $ Solver.printSolverError solverErrors
            pure Nothing
          Right (Tuple solvedCompiler resolutions) -> case compiler == solvedCompiler of
            true -> do
              Log.debug $ "Solved with compiler " <> Version.print solvedCompiler
              pure $ Just { compiler, resolutions, name: manifest.name, version: manifest.version }
            false -> do
              Log.debug $ Array.fold
                [ "Produced a compiler-derived build plan that selects a compiler ("
                , Version.print solvedCompiler
                , ") that differs from the target compiler ("
                , Version.print compiler
                , ")."
                ]
              pure Nothing
  pure $ Set.fromFoldable $ Array.catMaybes newJobs

checkIfNewCompiler :: forall r. Run (EXCEPT String + LOG + REGISTRY + AFF + r) (Maybe Version)
checkIfNewCompiler = do
  Log.info "Checking if there's a new compiler in town..."
  latestCompiler <- NonEmptyArray.foldr1 max <$> PursVersions.pursVersions
  maybeMetadata <- Registry.readMetadata $ unsafeFromRight $ PackageName.parse "prelude"
  pure $ maybeMetadata >>= \(Metadata metadata) ->
    Map.findMax metadata.published
      >>= \{ key: _version, value: { compilers } } -> do
        case all (_ < latestCompiler) compilers of
          -- all compilers compatible with the latest prelude are older than this one
          true -> Just latestCompiler
          false -> Nothing
