module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except as Except
import Control.Monad.State as State
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.Lens (_Left, preview)
import Data.Map as Map
import Data.Newtype as Newtype
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Now (nowDateTime) as Time
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Scripts.BowerImport.Error (ImportError(..), ImportErrorKey, ManifestError(..), PackageFailures(..), RawPackageName(..), RawVersion(..))
import Registry.Scripts.BowerImport.Error as BowerImport.Error
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as StringParser
import Web.Bower.PackageMeta as Bower

-- | This main loop uploads legacy packages to the new Registry
-- | In order to do this, we:
-- | - get an index of the legacy packages with their bowerfiles
-- | - create a graph (a tree really) where a package is a node and dependencies are edges
-- | - topologically sort this graph so that packages with no dependencies are at the root
-- | - go through this list: if the package is in the registry index then skip, otherwise upload
main :: Effect Unit
main = Aff.launchAff_ do
  log "Starting import from legacy registries..."
  _registry <- downloadBowerRegistry
  log "Done!"

downloadBowerRegistry :: Aff RegistryIndex
downloadBowerRegistry = do
  bowerPackages <- readRegistryFile "bower-packages.json"
  newPackages <- readRegistryFile "new-packages.json"

  let
    handleError _ = do
      log "bower-exclusions.json does not exist, writing..."
      let failures = PackageFailures Map.empty
      writeJsonFile "bower-exclusions.json" failures
      pure (Right failures)

  excludedPackages <- Except.catchError (readJsonFile "bower-exclusions.json") handleError >>= case _ of
    Left error -> throwError $ Aff.error $ Json.printJsonDecodeError error
    Right (failures :: PackageFailures) -> pure failures

  let
    allPackages = Map.union bowerPackages newPackages
    initialPackages =
      { failures: excludedPackages
      , packages: do
          -- We preemptively filter out packages we can't cache releases for.
          let
            shouldAccept = case _ of
              NoReleases -> false
              InvalidGitHubRepo _ -> false
              _ -> true

          filterFailedPackages shouldAccept excludedPackages allPackages
      }

  log "Fetching package releases..."
  releaseIndex <- forPackage initialPackages identity \name repoUrl -> do
    address <- case GitHub.parseRepo repoUrl of
      Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
      Right address -> pure address

    let repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]

    releases <- withCache repoCache (Just $ Hours 24.0) do
      log $ "Fetching releases for package " <> un RawPackageName name
      lift $ GitHub.getReleases address

    versions <- case NEA.fromArray releases of
      Nothing -> throwError NoReleases
      Just arr -> pure $ Map.fromFoldable $ map (\tag -> Tuple (RawVersion tag.name) unit) arr

    pure $ Tuple { name, address } versions

  let
    validBower = releaseIndex
      { packages = do
          -- We filter out package versions we can't cache a bowerfile for.
          let
            shouldAccept = case _ of
              MissingBowerfile -> false
              MalformedBowerJson _ -> false
              _ -> true

          filterFailedPackageVersions shouldAccept releaseIndex.failures _.name releaseIndex.packages
      }

  log "Fetching package bowerfiles..."
  bowerRegistry <- forPackageVersion validBower _.name identity \{ name, address } tag _ -> do
    bowerfile <- fetchBowerfile name address tag
    let packagesWithReleases = Set.map _.name $ Map.keys releaseIndex.packages
    selfContainedDependencies packagesWithReleases bowerfile
    pure bowerfile

  log "Parsing names and versions..."
  packageRegistry <- forPackageVersionKeys bowerRegistry _.name identity \{ name, address } tag -> do
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

  log "Converting bowerfiles to manifests..."
  let forPackageRegistry = forPackageVersion packageRegistry _.original _.original
  manifestRegistry <- forPackageRegistry \{ name, address } { semVer } bowerfile -> do
    let
      repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
      liftError = map (lmap ManifestError)

    Except.mapExceptT liftError $ toManifest name repo semVer bowerfile

  let
    registryIndex :: RegistryIndex
    registryIndex = do
      let
        packageManifests :: Array (Tuple PackageName (Map SemVer Manifest))
        packageManifests =
          map (lmap _.name)
            $ map (map (Map.fromFoldable <<< map (lmap _.semVer) <<< (Map.toUnfoldable :: _ -> Array _)))
            $ Map.toUnfoldable manifestRegistry.packages

      Map.fromFoldable packageManifests

  log "Writing exclusions file..."
  writeJsonFile "./bower-exclusions.json" manifestRegistry.failures
  pure registryIndex

-- | Find the bower.json files associated with the package's released tags,
-- | caching the file to avoid re-fetching each time the tool runs.
fetchBowerfile :: RawPackageName -> GitHub.Address -> RawVersion -> ExceptT ImportError Aff Bower.PackageMeta
fetchBowerfile name address tag = do
  let
    url = "https://raw.githubusercontent.com/" <> address.owner <> "/" <> address.repo <> "/" <> un RawVersion tag <> "/bower.json"
    bowerfileCache = "bowerfile__" <> un RawPackageName name <> "__" <> un RawVersion tag

  withCache bowerfileCache Nothing do
    lift (Http.get ResponseFormat.string url) >>= case _ of
      -- TODO: We should retry requests if they fail, because likely GitHub
      -- rate-limited us. Or at least we should collect the errors and print them
      -- out because it's quite possible the Bowerfile actually does work and
      -- we could have more granular errors to avoid excluding those packages.
      Left err -> do
        log $ "Unable to retrieve bowerfile. Bad request: " <> Http.printError err
        throwError MissingBowerfile
      Right { body, status }
        | status == StatusCode 404 -> do
            log $ "Unable to retrieve bowerfile because none exists (404 error)."
            throwError MissingBowerfile
        | status /= StatusCode 200 -> do
            log $ "Unable to retrieve bowerfile. Bad status: " <> body
            throwError $ BadStatus $ un StatusCode status
        | otherwise -> case Json.parseJson body >>= Json.decodeJson of
            Left err -> do
              log $ "Unable to parse bowerfile: " <> Json.printJsonDecodeError err <> " Malformed body: " <> body <> "."
              throwError $ MalformedBowerJson { error: Json.printJsonDecodeError err, contents: body }
            Right (bowerfile :: Bower.PackageMeta) -> pure bowerfile

-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
selfContainedDependencies :: Set RawPackageName -> Bower.PackageMeta -> ExceptT ImportError Aff Unit
selfContainedDependencies registry (Bower.PackageMeta bowerfile) = do
  let Bower.Dependencies allDeps = bowerfile.dependencies <> bowerfile.devDependencies
  outsideDeps <- for allDeps \{ packageName } -> do
    name <- cleanPackageName $ RawPackageName packageName
    pure $ if Set.member name registry then Nothing else Just name
  for_ (NEA.fromArray $ Array.catMaybes outsideDeps) \outside ->
    throwError $ NonRegistryDependencies $ coerce outside

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest
  :: PackageName
  -> Repo
  -> SemVer
  -> Bower.PackageMeta
  -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package repository version (Bower.PackageMeta bowerfile) = do
  let
    mkError :: forall a. ManifestError -> Either (NonEmptyArray ManifestError) a
    mkError = Left <<< NEA.singleton

    eitherLicense = do
      let
        rewrite = case _ of
          [ "Apache 2" ] -> [ "Apache-2.0" ]
          [ "Apache-2" ] -> [ "Apache-2.0" ]
          [ "Apache 2.0" ] -> [ "Apache-2.0" ]
          [ "BSD" ] -> [ "BSD-3-Clause" ]
          [ "BSD3" ] -> [ "BSD-3-Clause" ]
          [ "BSD-3" ] -> [ "BSD-3-Clause" ]
          [ "3-Clause BSD" ] -> [ "BSD-3-Clause" ]
          other -> other

        { fail, success } = partitionEithers $ SPDX.parse <$> rewrite bowerfile.license

      case fail of
        [] -> case success of
          [] -> mkError MissingLicense
          arr -> Right $ SPDX.joinWith SPDX.Or arr
        _ ->
          mkError $ BadLicense fail

    eitherTargets = do
      let
        parseName packageName =
          lmap (const packageName) $ PackageName.parse $ stripPureScriptPrefix packageName

        parsePairs = map \{ packageName, versionRange } -> case parseName packageName of
          Left e -> Left e
          Right name -> Right (Tuple name versionRange)

        normalizeDeps deps = do
          let { fail, success } = partitionEithers $ parsePairs deps
          case NEA.fromArray fail of
            Nothing -> pure success
            Just err -> mkError $ InvalidDependencyNames err

        checkDepPair (Tuple packageName versionStr) =
          case SemVer.parseRange versionStr of
            Nothing -> Left { dependency: packageName, failedBounds: versionStr }
            Just range -> Right $ Tuple (PackageName.print packageName) range

      normalizedDeps <- normalizeDeps $ un Bower.Dependencies bowerfile.dependencies
      normalizedDevDeps <- normalizeDeps $ un Bower.Dependencies bowerfile.devDependencies

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

-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall a
   . Json.DecodeJson a
  => Json.EncodeJson a
  => FilePath
  -> Maybe Hours
  -> ExceptT ImportError Aff a
  -> ExceptT ImportError Aff a
withCache path maybeDuration action = do
  let
    cacheFolder = ".cache"
    objectPath = cacheFolder <> "/" <> path
    fromJson = Json.jsonParser >=> (Json.decodeJson >>> lmap Json.printJsonDecodeError)
    onCacheMiss = do
      log $ "No cache hit for " <> show path
      result <- action
      lift $ writeJsonFile objectPath result
      pure result
    isCacheHit = liftAff do
      exists <- FS.exists objectPath
      expired <- case exists, maybeDuration of
        _, Nothing -> pure false
        false, _ -> pure false
        true, Just duration -> do
          lastModified <- FS.stat objectPath <#> unsafePartial fromJust <<< JSDate.toDateTime <<< _.mtime <<< (\(Stats s) -> s)
          now <- liftEffect $ Time.nowDateTime
          let expiryTime = unsafePartial fromJust $ Time.adjust duration lastModified
          pure (now > expiryTime)
      pure (exists && not expired)

  lift $ unlessM (FS.exists cacheFolder) (FS.mkdir cacheFolder)

  isCacheHit >>= case _ of
    true -> do
      strResult <- lift $ FS.readTextFile UTF8 objectPath
      case fromJson strResult of
        Right res -> pure res
        Left err -> do
          log $ "Unable to read cache file " <> err
          onCacheMiss
    false -> do
      onCacheMiss

-- Packages can be specified either in 'package-name' format or
-- in owner/package-name format. This function ensures we don't pick
-- up owner names as part of package names.
--
-- Example:
-- https://github.com/newlandsvalley/purescript-abc-parser/blob/1.1.2/bower.json
cleanPackageName :: forall m. Monad m => RawPackageName -> ExceptT ImportError m RawPackageName
cleanPackageName (RawPackageName name) = do
  let
    split = String.split (String.Pattern "/") <<< coerce
    strip = coerce <<< stripPureScriptPrefix

  map strip $ case split name of
    [ packageName ] -> pure packageName
    [ _owner, repo ] -> pure repo
    _ -> throwError $ MalformedPackageName name

-- | Read the list of packages in a registry file
readRegistryFile :: FilePath -> Aff (Map RawPackageName String)
readRegistryFile source = do
  registryFile <- readJsonFile ("../" <> source)
  case registryFile of
    Left err -> do
      let decodeError = "Decoding " <> source <> "failed with error:\n\n" <> Json.printJsonDecodeError err
      throwError $ Aff.error decodeError
    Right packages -> do
      let toPackagesArray = Object.toArrayWithKey \k -> Tuple (RawPackageName $ stripPureScriptPrefix k)
      pure $ Map.fromFoldable $ toPackagesArray packages

type ProcessedPackages k a =
  { failures :: PackageFailures
  , packages :: Map k a
  }

-- | Execute the provided transform on every package in the input packages map
-- | collecting failures into `PackageFailures` and saving transformed packages.
forPackage
  :: forall k1 k2 a b
   . Ord k1
  => Ord k2
  => ProcessedPackages k1 a
  -> (k1 -> RawPackageName)
  -> (k1 -> a -> ExceptT ImportError Aff (Tuple k2 b))
  -> Aff (ProcessedPackages k2 b)
forPackage input keyToPackageName f =
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate = forWithIndex_ input.packages \key value ->
    lift (Except.runExceptT (f key value)) >>= case _ of
      Left err -> do
        let
          errorType = BowerImport.Error.printImportErrorKey err
          name = keyToPackageName key
          failure = Map.singleton name (Map.singleton Nothing err)
        State.modify \state -> state { failures = insertFailure errorType failure state.failures }
      Right (Tuple newKey result) -> do
        let insertPackage = Map.insert newKey result
        State.modify \state -> state { packages = insertPackage state.packages }

type ProcessedPackageVersions k1 k2 a = ProcessedPackages k1 (Map k2 a)

-- | Execute the provided transform on every package in the input packages map,
-- | at every version of that package, collecting failures into `PackageFailures`
-- | and preserving transformed packages.
forPackageVersion
  :: forall k1 k2 a b
   . Ord k1
  => Ord k2
  => ProcessedPackageVersions k1 k2 a
  -> (k1 -> RawPackageName)
  -> (k2 -> RawVersion)
  -> (k1 -> k2 -> a -> ExceptT ImportError Aff b)
  -> Aff (ProcessedPackageVersions k1 k2 b)
forPackageVersion input keyToPackageName keyToTag f = do
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate =
    forWithIndex_ input.packages \k1 inner ->
      forWithIndex_ inner \k2 value -> do
        lift (Except.runExceptT (f k1 k2 value)) >>= case _ of
          Left err -> do
            let
              errorType = BowerImport.Error.printImportErrorKey err
              name = keyToPackageName k1
              tag = keyToTag k2
              failure = Map.singleton name (Map.singleton (Just tag) err)
            State.modify \state -> state { failures = insertFailure errorType failure state.failures }
          Right result -> do
            let
              newPackage = Map.singleton k2 result
              insertPackage = Map.insertWith Map.union k1 newPackage
            State.modify \state -> state { packages = insertPackage state.packages }

forPackageVersionKeys
  :: forall k1 k2 k3 k4 a
   . Ord k1
  => Ord k2
  => Ord k3
  => Ord k4
  => ProcessedPackageVersions k1 k2 a
  -> (k1 -> RawPackageName)
  -> (k2 -> RawVersion)
  -> (k1 -> k2 -> ExceptT ImportError Aff (Tuple k3 k4))
  -> Aff (ProcessedPackageVersions k3 k4 a)
forPackageVersionKeys input keyToPackageName keyToTag f = do
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate =
    forWithIndex_ input.packages \k1 inner ->
      forWithIndex_ inner \k2 value -> do
        lift (Except.runExceptT (f k1 k2)) >>= case _ of
          Left err -> do
            let
              errorType = BowerImport.Error.printImportErrorKey err
              name = keyToPackageName k1
              tag = keyToTag k2
              failure = Map.singleton name (Map.singleton (Just tag) err)
            State.modify \state -> state { failures = insertFailure errorType failure state.failures }
          Right (Tuple k3 k4) -> do
            let
              newPackage = Map.singleton k4 value
              insertPackage = Map.insertWith Map.union k3 newPackage
            State.modify \state -> state { packages = insertPackage state.packages }

insertFailure
  :: ImportErrorKey
  -> Map RawPackageName (Map (Maybe RawVersion) ImportError)
  -> PackageFailures
  -> PackageFailures
insertFailure key value failures = do
  let insert = Map.insertWith (Map.unionWith Map.union) key value
  Newtype.over PackageFailures insert failures

-- | Remove failing packages from the set of available packages to process
filterFailedPackages
  :: forall v
   . (ImportError -> Boolean)
  -> PackageFailures
  -> Map RawPackageName v
  -> Map RawPackageName v
filterFailedPackages shouldAccept (PackageFailures failures) = do
  let
    failedPackages :: Map RawPackageName (Map (Maybe RawVersion) ImportError)
    failedPackages = Map.unions $ Map.values failures

  Map.filterKeys \package -> case Map.lookup package failedPackages of
    Nothing -> true
    Just versionsMap -> not any (not shouldAccept) versionsMap

-- | Remove failing package versions from the set of available package versions
-- | to process. This will also remove packages that no longer have any versions
-- | after the filter is applied.
filterFailedPackageVersions
  :: forall k v
   . Ord k
  => (ImportError -> Boolean)
  -> PackageFailures
  -> (k -> RawPackageName)
  -> Map k (Map RawVersion v)
  -> Map k (Map RawVersion v)
filterFailedPackageVersions shouldAccept (PackageFailures failures) toPackageName = do
  let
    failedPackages :: Map RawPackageName (Map (Maybe RawVersion) ImportError)
    failedPackages =
      fromMaybe Map.empty
        $ map (NEA.foldl1 (Map.unionWith Map.union))
        $ NEA.fromFoldable
        $ Map.values failures

    skipFailedVersions = Map.mapMaybeWithKey \key rawVersions -> Just do
      let package = toPackageName key
      rawVersions # Map.filterKeys \version ->
        case Map.lookup package failedPackages of
          Nothing -> true
          Just failedVersions -> case Map.lookup (Just version) failedVersions of
            Nothing -> true
            Just failedVersion
              | shouldAccept failedVersion -> true
              | otherwise -> false

  Map.filter (not Map.isEmpty) <<< skipFailedVersions
