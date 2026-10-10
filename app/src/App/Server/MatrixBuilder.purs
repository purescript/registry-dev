module Registry.App.Server.MatrixBuilder
  ( BuildPlanEntry
  , MatrixSnapshot
  , MatrixSolverResult
  , checkIfNewCompiler
  , installBuildPlan
  , printCompilerFailure
  , readCompilerIndex
  , readMatrixSnapshot
  , resolutionsToBuildPlan
  , runMatrixJob
  , solveForAllCompilers
  , solveDependantsForCompiler
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (elem, foldM)
import Data.FoldableWithIndex (foldMapWithIndex)
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
import Registry.App.Effect.Registry (REGISTRY, REGISTRY_READ)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Tmp as Tmp
import Registry.ManifestIndex (ManifestIndex)
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

-- | A pass-local snapshot, captured after publication or compatibility writes.
-- | The executor is serial; queued jobs cannot change it during scheduling.
type MatrixSnapshot =
  { compilerIndex :: Solver.CompilerIndex
  , manifestIndex :: ManifestIndex
  , metadata :: Map PackageName Metadata
  , compilers :: NonEmptyArray Version
  }

readCompilerIndex :: forall r. Run (REGISTRY_READ + AFF + EXCEPT String + r) Solver.CompilerIndex
readCompilerIndex = _.compilerIndex <$> readMatrixSnapshot

readMatrixSnapshot :: forall r. Run (REGISTRY_READ + AFF + EXCEPT String + r) MatrixSnapshot
readMatrixSnapshot = do
  metadata <- Registry.readAllMetadata
  manifestIndex <- Registry.readAllManifests
  compilers <- PursVersions.pursVersions
  pure { compilerIndex: Solver.buildCompilerIndex compilers manifestIndex metadata, manifestIndex, metadata, compilers }

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
resolutionsToBuildPlan :: forall r. Map PackageName Version -> Run (REGISTRY_READ + EXCEPT String + r) (Map PackageName BuildPlanEntry)
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
  { snapshot :: MatrixSnapshot
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

-- | Emit each solved plan before continuing, so cancellation does not discard
-- | all progress. Callers can persist plans using the existing matrix queue.
solveForAllCompilers :: forall r. MatrixSolverData -> (MatrixSolverResult -> Run (LOG + r) Unit) -> Run (LOG + r) (Set MatrixSolverResult)
solveForAllCompilers solverData@{ compiler, snapshot } emit = do
  -- remove the compiler we tested with from the set of all of them
  let compilers = Array.filter (_ /= compiler) $ NonEmptyArray.toArray snapshot.compilers
  newJobs <- for compilers \target -> do
    result <- trySolveForCompiler (solverData { compiler = target })
    for_ result emit
    pure result
  pure $ Set.fromFoldable $ Array.catMaybes newJobs

solveDependantsForCompiler :: forall r. MatrixSolverData -> (MatrixSolverResult -> Run (LOG + r) Unit) -> Run (LOG + r) (Set MatrixSolverResult)
solveDependantsForCompiler { snapshot, name, version, compiler } emit = do
  let seed = Tuple name version
  -- Sort this fixed index once, not once per already-compatible dependant.
  let manifests = ManifestIndex.toSortedArray ManifestIndex.ConsiderRanges snapshot.manifestIndex
  { results, visited } <- go manifests (Set.singleton seed) name version
  Log.info $ Array.fold
    [ "Cascade from "
    , PackageName.print name
    , "@"
    , Version.print version
    , ": "
    , show (Set.size results)
    , " solved out of "
    , show (Set.size visited - 1)
    , " dependants visited"
    ]
  pure results
  where
  -- Recursively find packages to enqueue. Trivially this includes direct
  -- dependants, but we need more than that: when a direct dependant is already
  -- compatible with the target compiler, recurse down to its own dependants,
  -- and so on.
  -- This handles niche cases of transitive version-conflict cascades:
  -- if A depends on B which depends on C (wide range), and A's full plan forces
  -- C@new (because of other packages) but all versions of B already compiled
  -- against C@old, then - if we only propagated direct dependents - B will
  -- never be retriggered.
  -- With this recursive propagation, when C@new completes we cascade through
  -- B (already compiled) and reach A, allowing for a plan to resolve.
  go manifests visited pkgName pkgVersion = do
    let
      dependentManifests = Array.filter
        (\(Manifest manifest) -> maybe false (flip Range.includes pkgVersion) $ Map.lookup pkgName manifest.dependencies)
        manifests
    foldM (processManifest manifests) { visited, results: Set.empty } dependentManifests

  processManifest manifests acc (Manifest manifest) = do
    let pv = Tuple manifest.name manifest.version
    if Set.member pv acc.visited then
      pure acc
    else do
      let newVisited = Set.insert pv acc.visited
      case Map.lookup manifest.name snapshot.metadata of
        Nothing -> do
          Log.warn $ "No metadata for dependant " <> PackageName.print manifest.name <> ", skipping"
          pure { visited: newVisited, results: acc.results }
        Just metadata ->
          case Map.lookup manifest.version (un Metadata metadata).published of
            Nothing -> do
              Log.warn $ "Dependant " <> PackageName.print manifest.name <> "@" <> Version.print manifest.version <> " not in metadata.published, skipping"
              pure { visited: newVisited, results: acc.results }
            Just { compilers }
              | elem compiler compilers -> do
                  -- Already has compiler: propagate through to find stranded packages
                  sub <- go manifests newVisited manifest.name manifest.version
                  pure { visited: sub.visited, results: acc.results <> sub.results }
              | otherwise -> do
                  result <- trySolveForCompiler { snapshot, compiler, name: manifest.name, version: manifest.version, dependencies: manifest.dependencies }
                  case result of
                    Nothing -> pure { visited: newVisited, results: acc.results }
                    Just entry -> do
                      emit entry
                      pure { visited: newVisited, results: Set.insert entry acc.results }

-- | Try to solve a package's dependencies for a specific compiler. Returns
-- | the solver result if the produced build plan targets the expected compiler,
-- | Nothing otherwise (solver failure or compiler mismatch).
trySolveForCompiler :: forall r. MatrixSolverData -> Run (LOG + r) (Maybe MatrixSolverResult)
trySolveForCompiler { snapshot, compiler, name, version, dependencies } = do
  Log.debug $ "Trying compiler " <> Version.print compiler <> " for package " <> PackageName.print name
  case Solver.solveWithCompiler (Range.exact compiler) snapshot.compilerIndex dependencies of
    Left solverErrors -> do
      Log.info $ "Failed to solve with compiler " <> Version.print compiler <> ": " <> PackageName.print name <> "@" <> Version.print version
      Log.debug $ "Solver errors:\n" <> foldMapWithIndex
        (\i error -> "[Error " <> show (i + 1) <> "]\n" <> Solver.printSolverError error <> "\n")
        solverErrors
      pure Nothing
    Right (Tuple solvedCompiler resolutions)
      | solvedCompiler == compiler -> do
          Log.debug $ "Solved " <> PackageName.print name <> "@" <> Version.print version <> " with compiler " <> Version.print solvedCompiler
          pure $ Just { compiler, resolutions, name, version }
      | otherwise -> do
          Log.debug $ Array.fold
            [ "Produced a compiler-derived build plan that selects a compiler ("
            , Version.print solvedCompiler
            , ") that differs from the target compiler ("
            , Version.print compiler
            , ")."
            ]
          pure Nothing

checkIfNewCompiler :: forall r. Run (EXCEPT String + LOG + REGISTRY_READ + AFF + r) (Maybe Version)
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
