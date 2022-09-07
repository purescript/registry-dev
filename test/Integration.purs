module Test.Integration where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Monad.State as State
import Data.Array.NonEmpty as NEA
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.String as String
import Effect.Exception as Exception
import Foreign.Git as Git
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Parsing as Parsing
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location(..), Manifest(..))
import Registry.Solver as Solver
import Registry.Version (Range, Version)
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ do
  { index, bowerSolutions } <- setup

  let
    -- Note: this segmented index only considers packages that have a
    -- corresponding Bower solution.
    segmentedIndex :: SegmentedByOwner
    segmentedIndex = segmentSolvableByOwner index bowerSolutions

    solverIndex :: Solver.Dependencies
    solverIndex = map (map (\(Manifest m) -> m.dependencies)) index

    corePackages :: Solver.Dependencies
    corePackages = unsafeFromJust $ Map.lookup "purescript" segmentedIndex

    contribPackages :: Solver.Dependencies
    contribPackages = unsafeFromJust $ Map.lookup "purescript-contrib" segmentedIndex

    nodePackages :: Solver.Dependencies
    nodePackages = unsafeFromJust $ Map.lookup "purescript-node" segmentedIndex

    webPackages :: Solver.Dependencies
    webPackages = unsafeFromJust $ Map.lookup "purescript-web" segmentedIndex

    -- These are not run by default, but they can be added at any time for spot
    -- checks.
    _otherPackages :: Solver.Dependencies
    _otherPackages =
      foldl (Map.unionWith Map.union) Map.empty
        $ Map.delete "purescript"
        $ Map.delete "purescript-contrib"
        $ Map.delete "purescript-node"
        $ Map.delete "purescript-web" segmentedIndex

  runSpec' defaultConfig [ consoleReporter ] do
    let
      testPackages :: Solver.Dependencies -> Spec.Spec Unit
      testPackages pkgs = void $ forWithIndex pkgs \package versions ->
        Spec.it ("Solves " <> PackageName.print package) do
          log $ "Solving " <> PackageName.print package <> "..."
          void $ forWithIndex versions \version dependencies -> do
            let
              name = PackageName.print package <> "@" <> Version.printVersion version
              isNoVersionsError = case _ of
                Solver.NoVersionsInRange _ _ _ _ -> true
                _ -> false

            case Solver.solve solverIndex dependencies of
              -- If we can't provide a solution because no versions are available in
              -- the index, then we either restricted our ranges too much or the
              -- problem is that our index has fewer versions than Bower's. In the
              -- interest of useful tests, we assume the latter.
              Left errs | NEA.any isNoVersionsError errs -> pure unit
              -- Otherwise, we failed to find a solution and failed the test.
              Left errs -> do
                let printedErrs = String.joinWith "\n" $ NEA.toArray $ map Solver.printSolverError errs
                let bowerSolution = unsafeFromJust $ Map.lookup package bowerSolutions >>= Map.lookup version
                Assert.fail $ String.joinWith "\n----------\n"
                  [ name
                  , printedErrs
                  , Json.printJson bowerSolution
                  ]
              -- If we found a solution then we passed the test.
              -- TODO: We can also check that our solution produces versions as
              -- high as those produced by Bower, if we want.
              Right _ -> pure unit

    Spec.describe "Solves core packages" do
      testPackages corePackages

  {-
  Spec.describe "Solves contrib packages" do
    const (pure unit) $ testPackages contribPackages

  Spec.describe "Solves web packages" do
    const (pure unit) $ testPackages webPackages

  Spec.describe "Solves node packages" do
    const (pure unit) $ testPackages nodePackages

  Spec.describe "Solves other packages" do
    const (pure unit) $ testPackages _otherPackages
  -}
  where
  setup :: Aff { index :: RegistryIndex, bowerSolutions :: Map PackageName (Map Version (Map PackageName Version)) }
  setup = do
    tmp <- liftEffect Tmp.mkTmpDir

    result <- withBackoff' $ Except.runExceptT do
      Git.runGit_ [ "clone", "https://github.com/purescript/registry-index", "--depth", "20" ] (Just tmp)
      Git.runGit_ [ "checkout", "b2aaad20631178421e1140a414f02c89374341e9" ] (Just (Path.concat [ tmp, "registry-index" ]))
      Git.runGit_ [ "clone", "https://github.com/thomashoneyman/bower-solver-results" ] (Just tmp)
      Git.runGit_ [ "checkout", "302e26e0c9a1c587c1acdb04192d0b93c073bdf1" ] (Just (Path.concat [ tmp, "bower-solver-results" ]))

    case result of
      Nothing -> throwError $ Exception.error "Could not clone registry index and/or solver results."
      Just _ -> pure unit

    index <- Index.readRegistryIndex (Path.concat [ tmp, "registry-index" ])

    bowerSolutions <- do
      contents <- FS.Aff.readdir (Path.concat [ tmp, "bower-solver-results" ])
      let files = filterMap (String.stripSuffix (String.Pattern ".json")) contents
      parsed <- for files \filename -> do
        package <- case PackageName.parse filename of
          Left err -> throwError $ Exception.error $ Parsing.parseErrorMessage err
          Right parsed -> pure parsed
        versions <- Json.readJsonFile (Path.concat [ tmp, "bower-solver-results", filename <> ".json" ]) >>= case _ of
          Left err -> throwError $ Exception.error err
          Right versions -> pure versions
        pure (Tuple package versions)
      liftEffect $ log "Finished reading JSON"
      pure $ Map.fromFoldable parsed

    pure { index, bowerSolutions }

type BowerSolutions = Map PackageName (Map Version (Map PackageName Version))

type Owner = String

type SegmentedByOwner = Map Owner (Map PackageName (Map Version (Map PackageName Range)))

segmentSolvableByOwner :: RegistryIndex -> BowerSolutions -> SegmentedByOwner
segmentSolvableByOwner index bower = snd $ flip State.runState Map.empty do
  void $ forWithIndex index \package versions ->
    void $ forWithIndex versions \version (Manifest manifest) -> do
      -- We only include packages that Bower considers solvable.
      case Map.lookup package bower >>= Map.lookup version of
        Nothing -> pure unit
        Just _ -> case manifest.location of
          GitHub { owner } -> do
            let
              modifier :: SegmentedByOwner -> SegmentedByOwner
              modifier = Map.insertWith (Map.unionWith Map.union) owner (Map.singleton package (Map.singleton manifest.version manifest.dependencies))
            State.modify_ modifier
          _ ->
            pure unit
