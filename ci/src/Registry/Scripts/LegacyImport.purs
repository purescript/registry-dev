module Registry.Scripts.LegacyImport where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.Argonaut as Json
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Lens (_Left, preview)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Registry.Error (mkError)
import Registry.Index (RegistryIndex, insertManifest)
import Registry.License (produceLicense)
import Registry.Manifest (constructManifestFields)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Process as Process
import Registry.Scripts.LegacyImport.Stats as Stats
import Registry.Types (APIResource(..), ImportError(..), ManifestError(..), ManifestFields, PackageFailures(..), RawPackageName(..), RawVersion(..), RemoteResource(..), RequestError(..))
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as StringParser

-- | This main loop uploads legacy packages to the new Registry
-- | In order to do this, we:
-- | - get an index of the legacy packages with their bowerfiles
-- | - create a graph (a tree really) where a package is a node and dependencies are edges
-- | - topologically sort this graph so that packages with no dependencies are at the root
-- | - go through this list: if the package is in the registry index then skip, otherwise upload
main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  log "Starting import from legacy registries..."
  registry <- downloadLegacyRegistry

  let indexPath = "registry-index"
  log $ "Writing registry index to " <> indexPath

  exists <- FS.exists indexPath
  guard (not exists) $ FS.mkdir indexPath
  for_ registry \semVer -> for_ semVer (insertManifest indexPath)
  log "Done!"

downloadLegacyRegistry :: Aff RegistryIndex
downloadLegacyRegistry = do
  octokit <- liftEffect GitHub.mkOctokit
  bowerPackages <- readRegistryFile "bower-packages.json"
  newPackages <- readRegistryFile "new-packages.json"

  let
    allPackages = Map.union bowerPackages newPackages
    initialPackages = { failures: PackageFailures Map.empty, packages: allPackages }

  log "Fetching package releases..."
  releaseIndex <- Process.forPackage initialPackages \name repoUrl -> do
    address <- case GitHub.parseRepo repoUrl of
      Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
      Right address -> pure address

    let
      repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]
      mkError = ResourceError <<< { resource: APIResource GitHubReleases, error: _ }

    releases <- Process.withCache Process.jsonSerializer repoCache (Just $ Hours 24.0) do
      log $ "Fetching releases for package " <> un RawPackageName name
      result <- lift $ try $ GitHub.getReleases octokit address
      case result of
        Left err -> logShow err *> throwError (mkError $ DecodeError $ Aff.message err)
        Right v -> pure v

    versions <- case NEA.fromArray releases of
      Nothing -> throwError $ mkError $ DecodeError "No releases returned from the GitHub API."
      Just arr -> pure $ Map.fromFoldable $ map (\tag -> Tuple (RawVersion tag.name) unit) arr

    pure $ Tuple { name, address } versions

  log "Parsing names and versions..."
  packageRegistry <- Process.forPackageVersionKeys releaseIndex \{ name, address } tag -> do
    packageName <- case PackageName.parse $ un RawPackageName name of
      Left err ->
        throwError $ MalformedPackageName $ StringParser.printParserError err
      Right pname ->
        pure pname

    packageSemVer <- case SemVer.parseSemVer $ un RawVersion tag of
      Nothing ->
        throwError $ ManifestError $ NEA.singleton $ BadVersion $ un RawVersion tag
      Just semVer ->
        pure semVer

    let
      outerKey = { name: packageName, original: name, address }
      innerKey = { semVer: packageSemVer, original: tag }

    pure $ Tuple outerKey innerKey

  log "Converting to manifests..."
  let forPackageRegistry = Process.forPackageVersion packageRegistry
  manifestRegistry :: Process.ProcessedPackageVersions
    { address :: GitHub.Address
    , name :: PackageName
    , original :: RawPackageName
    }
    { semVer :: SemVer, original :: RawVersion }
    Manifest <- forPackageRegistry \{ name, original: originalName, address } tag _ -> do
    manifestFields <- constructManifestFields originalName tag.original address

    let
      repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
      liftError = map (lmap ManifestError)

    Except.mapExceptT liftError $ toManifest name repo tag.semVer manifestFields

  log "Checking dependencies are self-contained..."
  containedRegistry :: Process.ProcessedPackageVersions
    { address :: GitHub.Address
    , name :: PackageName
    , original :: RawPackageName
    }
    { semVer :: SemVer, original :: RawVersion }
    Manifest <- Process.forPackageVersion manifestRegistry \_ _ manifest -> do
    _ <- selfContainedDependencies (Map.keys allPackages) manifest
    pure manifest

  let
    registryIndex :: RegistryIndex
    registryIndex = do
      let
        packageManifests :: Array (Tuple PackageName (Map SemVer Manifest))
        packageManifests =
          map (lmap _.name)
            $ map (map (Map.fromFoldable <<< map (lmap _.semVer) <<< (Map.toUnfoldable :: _ -> Array _)))
            $ Map.toUnfoldable containedRegistry.packages

      Map.fromFoldable packageManifests

  log "Writing exclusions file..."
  writeJsonFile "./bower-exclusions.json" manifestRegistry.failures
  Stats.logStats $ Stats.errorStats manifestRegistry
  pure registryIndex

-- | TODO: This check can't be done on a per-package basis without ordering.
-- | It requires that all versions are topologically-sorted first.
-- |
-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
selfContainedDependencies :: Set RawPackageName -> Manifest -> ExceptT ImportError Aff Unit
selfContainedDependencies registry { targets } = do
  let
    allDeps :: Array RawPackageName
    allDeps = coerce $ Array.nub $ Array.foldMap (Object.keys <<< _.dependencies) $ Object.values targets

  outsideDeps <- for allDeps \packageName -> do
    name <- cleanPackageName packageName
    pure $ if Set.member name registry then Nothing else Just name

  for_ (NEA.fromArray $ Array.catMaybes outsideDeps) \outside ->
    throwError $ NonRegistryDependencies outside

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest
  :: PackageName
  -> Repo
  -> SemVer
  -> ManifestFields
  -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package repository version manifest = do
  let
    eitherLicense = lmap (NEA.singleton <<< LicenseError) (produceLicense manifest.license)
    eitherTargets = do
      let
        -- We trim out packages that don't begin with `purescript-`, as these
        -- are JavaScript dependencies being specified in the Bowerfile.
        filterNames = catMaybes <<< map \(Tuple packageName versionRange) ->
          case String.take 11 packageName of
            "purescript-" -> Just $ Tuple (String.drop 11 packageName) versionRange
            _ -> Nothing

        parsePairs = map \(Tuple packageName versionRange) -> case PackageName.parse packageName of
          Left _ -> Left packageName
          Right name -> Right (Tuple name versionRange)

        normalizeDeps deps = do
          let { fail, success } = partitionEithers $ parsePairs $ filterNames deps
          case NEA.fromArray fail of
            Nothing -> pure success
            Just err -> mkError $ InvalidDependencyNames err

        checkDepPair (Tuple packageName versionStr) =
          case SemVer.parseRange versionStr of
            Nothing -> Left { dependency: packageName, failedBounds: versionStr }
            Just range -> Right $ Tuple (PackageName.print packageName) range

      normalizedDeps <- normalizeDeps $ Object.toUnfoldable manifest.dependencies
      normalizedDevDeps <- normalizeDeps $ Object.toUnfoldable manifest.devDependencies

      let
        readDeps = map checkDepPair >>> partitionEithers >>> \{ fail, success } ->
          case NEA.fromArray fail of
            Nothing ->
              Right success
            Just err ->
              mkError $ BadDependencyVersions err

        eitherDeps = readDeps normalizedDeps
        eitherDevDeps = readDeps normalizedDevDeps

      case eitherDeps, eitherDevDeps of
        Left e1, Left e2 -> Left (e1 <> e2)
        Left e, Right _ -> Left e
        Right _, Left e -> Left e
        Right deps, Right devDeps -> Right $ Object.fromFoldable $ Array.catMaybes
          [ Just $ Tuple "lib"
              { sources: [ "src/**/*.purs" ]
              , dependencies: Object.fromFoldable deps
              }
          , if (Array.null devDeps) then Nothing
            else Just $ Tuple "test"
              { sources: [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies: Object.fromFoldable (deps <> devDeps)
              }
          ]

    errs = do
      let
        toMaybeErrors :: forall e a. Either e a -> Maybe e
        toMaybeErrors = preview _Left

      map NEA.concat $ NEA.fromArray $ Array.catMaybes
        [ toMaybeErrors eitherLicense
        , toMaybeErrors eitherTargets
        ]

  case errs of
    Nothing -> do
      -- Technically this shouldn't be needed, since we've already checked these
      -- for errors, but this is just so the types all work out.
      license <- Except.except eitherLicense
      targets <- Except.except eitherTargets
      pure { name: package, license, repository, targets, version }

    Just err ->
      throwError err

-- Packages can be specified either in 'package-name' format or
-- in owner/package-name format. This function ensures we don't pick
-- up owner names as part of package names.
--
-- Example:
-- https://github.com/newlandsvalley/purescript-abc-parser/blob/1.1.2/bower.json
cleanPackageName :: RawPackageName -> ExceptT ImportError Aff RawPackageName
cleanPackageName (RawPackageName name) = do
  let
    split = String.split (String.Pattern "/") <<< coerce
    strip = coerce <<< stripPureScriptPrefix

  map strip $ case split name of
    [ packageName ] -> pure packageName
    [ _owner, repo ] -> pure repo
    _ -> throwError $ MalformedPackageName name

-- | Read the list of packages in a registry file
readRegistryFile :: FilePath -> Aff (Map RawPackageName PackageURL)
readRegistryFile source = do
  registryFile <- readJsonFile ("../" <> source)
  case registryFile of
    Left err -> do
      let decodeError = "Decoding " <> source <> "failed with error:\n\n" <> Json.printJsonDecodeError err
      throwError $ Aff.error decodeError
    Right packages -> do
      let toPackagesArray = Object.toArrayWithKey \k -> Tuple (RawPackageName $ stripPureScriptPrefix k)
      pure $ Map.fromFoldable $ toPackagesArray packages
