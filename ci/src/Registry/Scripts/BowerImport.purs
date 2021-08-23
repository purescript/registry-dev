module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except as Except
import Control.Monad.State as State
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.Lens (_Left, preview)
import Data.Map as Map
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Now (nowDateTime) as Time
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Scripts.BowerImport.Error (ImportError(..), ManifestError(..))
import Registry.Scripts.BowerImport.Error as BowerImport.Error
import Text.Parsing.StringParser as StringParser
import Web.Bower.PackageMeta as Bower

exclusionsFile :: FilePath
exclusionsFile = "./bower-exclusions.json"

errorsFile :: FilePath
errorsFile = "./bower-failures.json"

manifestsFile :: FilePath
manifestsFile = "./bower-manifests.json"

-- | This main loop uploads legacy packages to the new Registry
-- | In order to do this, we:
-- | - get an index of the legacy packages with their bowerfiles
-- | - create a graph (a tree really) where a package is a node and dependencies are edges
-- | - topologically sort this graph so that packages with no dependencies are at the root
-- | - go through this list: if the package is in the registry index then skip, otherwise upload
main :: Effect Unit
main = Aff.launchAff_ do
  log "Starting import from legacy registries..."
  manifests <- downloadLegacyRegistry
  log "Writing manifests file..."
  writeJsonFile manifestsFile manifests
  log "Done!"

-- | An unprocessed package name, which may possibly be malformed.
type RawPackageName = String

-- | A map of package names to valid manifests that have been imported from the
-- | old registries and which can be added to the new registry.
type PackageManifests = Map RawPackageName (NonEmptyArray Manifest)

-- | A map of package names to versions that do not produce a valid manifest,
-- | along with information about why they failed.
type PackageFailures = Map RawPackageName (Map (Maybe GitHub.Tag) ImportError)

-- | Convert legacy packages to package manifests. Packages that cannot be
-- | converted are written out to an errors file.
downloadLegacyRegistry :: Aff PackageManifests
downloadLegacyRegistry = do
  -- First, we read in all the legacy packages contained in the bower-packages
  -- and new-packages files.
  log "Reading legacy registry packages..."
  allPackages <- readRegistryPackages
  exclusions <- readExclusionsFile

  -- Then we transform each package into a manifest, fixing as many packages as
  -- we can, and collecting errors along the way.
  log "Transforming legacy packages to manifests..."
  { failures, packages } <-
    runPackageTransform getReleases { failures: Map.empty, packages: allPackages }
      >>= runPackageVersionTransform (skipExcluded exclusions)
      >>= runPackageVersionTransform fetchBowerfile
      >>= runPackageVersionTransform (checkSelfContained allPackages)
      >>= runPackageVersionTransform convertToManifest

  -- Then, we write failed packages out to files so we can fix the errors or
  -- simply list packages that won't be included in the new registry.
  log "Writing exclusions and errors files..."
  writeFailuresFile failures
  writeExclusionsFile failures

  let
    manifests = do
      let toManifestArray { versions } = NEA.fromArray $ Array.fromFoldable $ Map.values versions
      Map.mapMaybe toManifestArray packages

  log "Finished transforming legacy packages!"
  pure manifests

-- | Find all released tags for the package, returning the parsed repository
-- | location and a map of tags.
getReleases :: PackageTransform RawPackageName { meta :: GitHub.Address, versions :: Map GitHub.Tag Unit }
getReleases = PackageTransform "Get released GitHub tags" \name repo -> do
  address <- case GitHub.parseRepo repo of
    Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
    Right address -> pure address

  let repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]

  releases <- withCache repoCache (Just $ Hours 24.0) do
    log $ "Fetching releases for package " <> name
    lift $ GitHub.getReleases address

  versions <- case releases of
    [] -> throwError NoReleases
    _ -> pure $ Map.fromFoldable $ map (\tag -> Tuple tag unit) releases

  pure { meta: address, versions }

-- | Find the bower.json files associated with the package's relaesed tags.
skipExcluded :: Map String (Array (Maybe String)) -> PackageVersionTransform GitHub.Address Unit Unit
skipExcluded exclusions = PackageVersionTransform "Filter out excluded packages" \package _ tag _ -> do
  for_ (Map.lookup package exclusions) \tags ->
    when (Array.elem Nothing tags || Array.elem (Just tag.name) tags) do
      throwError ExcludedPackage

-- | Find the bower.json files associated with the package's released tags,
-- | caching the file to avoid re-fetching each time the tool runs.
fetchBowerfile :: PackageVersionTransform GitHub.Address Unit Bower.PackageMeta
fetchBowerfile = PackageVersionTransform "Fetch bower.json for tag" \package address tag _ -> do
  let
    url = "https://raw.githubusercontent.com/" <> address.owner <> "/" <> address.repo <> "/" <> tag.name <> "/bower.json"
    bowerfileCache = "bowerfile__" <> package <> "__" <> tag.name

  withCache bowerfileCache Nothing do
    lift (Http.get ResponseFormat.string url) >>= case _ of
      Left _ -> throwError MissingBowerfile
      Right { body } -> case Json.parseJson body >>= Json.decodeJson of
        Left err -> throwError $ MalformedBowerJson err
        Right res -> pure res

-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
checkSelfContained :: Map RawPackageName String -> PackageVersionTransform GitHub.Address Bower.PackageMeta Bower.PackageMeta
checkSelfContained registry =
  PackageVersionTransform "Check dependencies are all in the registry" \_ _ _ (Bower.PackageMeta bowerfile) -> do
    let
      -- Packages can be specified either in 'package-name' format or
      -- in owner/package-name format. This function ensures we don't pick
      -- up owner names as part of package names.
      --
      -- Example:
      -- https://github.com/newlandsvalley/purescript-abc-parser/blob/1.1.2/bower.json
      normalizePackageName raw = map stripPureScriptPrefix case String.split (String.Pattern "/") raw of
        [ packageName ] -> pure packageName
        [ _owner, repo ] -> pure repo
        _ -> throwError $ MalformedPackageName raw

      Bower.Dependencies allDeps = bowerfile.dependencies <> bowerfile.devDependencies

    outsideDeps <- for allDeps \{ packageName } -> do
      name <- normalizePackageName packageName
      pure $ case Map.lookup name registry of
        Nothing -> Just name
        Just _ -> Nothing

    case NEA.fromArray $ Array.catMaybes outsideDeps of
      Nothing ->
        pure $ Bower.PackageMeta bowerfile
      Just outside ->
        throwError $ NonRegistryDependencies outside

convertToManifest :: PackageVersionTransform GitHub.Address Bower.PackageMeta Manifest
convertToManifest = PackageVersionTransform "Convert Bowerfile to a manifest" \package address tag bowerfile -> do
  let
    repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
    liftError = map (lmap ManifestError)

  Except.mapExceptT liftError $ toManifest package repo tag.name bowerfile

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest
  :: RawPackageName
  -> Repo
  -> String
  -> Bower.PackageMeta
  -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package repository ref (Bower.PackageMeta bowerfile) = do
  let
    mkError :: forall a. ManifestError -> Either (NonEmptyArray ManifestError) a
    mkError = Left <<< NEA.singleton

    bowerName = stripPureScriptPrefix bowerfile.name

    eitherName = case PackageName.parse bowerName of
      Right name | bowerName == package ->
        Right name
      Right _ -> do
        mkError $ MismatchedName { expected: package, received: bowerfile.name }
      Left _ -> do
        mkError $ MismatchedName { expected: package, received: bowerfile.name }

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

    eitherVersion = case SemVer.parseSemVer ref of
      Nothing ->
        mkError $ BadVersion ref
      Just version ->
        Right version

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
        [ toMaybeErrors eitherName
        , toMaybeErrors eitherLicense
        , toMaybeErrors eitherVersion
        , toMaybeErrors eitherTargets
        ]

  case errs of
    Nothing -> do
      -- Technically this shouldn't be needed, since we've already checked these
      -- for errors, but this is just so the types all work out.
      name <- Except.except eitherName
      license <- Except.except eitherLicense
      version <- Except.except eitherVersion
      targets <- Except.except eitherTargets
      pure { name, license, repository, targets, version }

    Just err ->
      throwError err

-- | Perform an effectful transformation on a package, returning the transformed
-- | package. If a package cannot be transformed, throw an `ImportError`
-- | exception via `ExceptT` and the error will be collected.
data PackageTransform a b =
  PackageTransform
    String
    (RawPackageName -> a -> ExceptT ImportError Aff b)

-- | Execute the provided transform on every package in the input packages map,
-- | collecting failures into the `PackageFailures` and preserving successfully-
-- | transformed packages in the returned packages map.
runPackageTransform
  :: forall a b
   . PackageTransform a b
  -> { failures :: PackageFailures, packages :: Map RawPackageName a }
  -> Aff { failures :: PackageFailures, packages :: Map RawPackageName b }
runPackageTransform (PackageTransform label run) input = do
  log $ "[STEP] " <> label
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate = forWithIndex_ input.packages \package value -> do
    lift (Except.runExceptT (run package value)) >>= case _ of
      Left err -> do
        let
          insertFailure = Map.insertWith Map.union package
          newFailure = Map.singleton Nothing err
        State.modify \state -> state { failures = insertFailure newFailure state.failures }
      Right res -> do
        State.modify \state -> state { packages = Map.insert package res state.packages }

-- | A map of package names to versions to some `a`, with the ability to
-- | associate some data `meta` with a particular package
type PackageMap meta a = Map RawPackageName { meta :: meta, versions :: Map GitHub.Tag a }

-- | Perform an effectful transformation on a package at a particular version,
-- | using additional metadata `meta` about the package. If a package cannot be
-- | transformed, throw an `ImportError` exception via `ExceptT` and the error
-- | will be collected.
data PackageVersionTransform meta a b =
  PackageVersionTransform
    String
    (RawPackageName -> meta -> GitHub.Tag -> a -> ExceptT ImportError Aff b)

-- | Execute the provided transform on every package in the input packages map, at
-- | every version of that package, collecting failures into `PackageFailures`
-- | and preserving transformed packages in the returned packages map.
runPackageVersionTransform
  :: forall meta a b
   . PackageVersionTransform meta a b
  -> { failures :: PackageFailures, packages :: PackageMap meta a }
  -> Aff { failures :: PackageFailures, packages :: PackageMap meta b }
runPackageVersionTransform (PackageVersionTransform label run) input = do
  log $ "[STEP] " <> label
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate =
    forWithIndex_ input.packages \package { meta, versions } ->
      forWithIndex_ versions \version value -> do
        lift (Except.runExceptT (run package meta version value)) >>= case _ of
          Left err -> do
            let
              insertFailure = Map.insertWith Map.union package
              newFailure = Map.singleton (Just version) err
            State.modify \state -> state { failures = insertFailure newFailure state.failures }
          Right res -> do
            let
              mergeVersions old new = { meta, versions: Map.union old.versions new.versions }
              insertPackage = Map.insertWith mergeVersions package
              newPackage = { meta, versions: Map.singleton version res }
            State.modify \state -> state { packages = insertPackage newPackage state.packages }

-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall a
   . Json.DecodeJson a
  => Json.EncodeJson a
  => String
  -> Maybe Hours
  -> ExceptT ImportError Aff a
  -> ExceptT ImportError Aff a
withCache path maybeDuration action = do
  let cacheFolder = ".cache"
  let objectPath = cacheFolder <> "/" <> path
  let dump = Json.encodeJson >>> Json.stringifyWithIndent 2
  let fromJson = Json.jsonParser >=> (Json.decodeJson >>> lmap Json.printJsonDecodeError)
  let yolo a = unsafePartial $ fromJust a

  let
    cacheHit = liftAff do
      exists <- FS.exists objectPath
      expired <- case exists, maybeDuration of
        _, Nothing -> pure false
        false, _ -> pure false
        true, Just duration -> do
          lastModified <- (yolo <<< JSDate.toDateTime <<< _.mtime <<< (\(Stats s) -> s))
            <$> FS.stat objectPath
          now <- liftEffect $ Time.nowDateTime
          let expiryTime = yolo $ Time.adjust duration lastModified
          pure (now > expiryTime)
      pure (exists && not expired)
  lift $ unlessM (FS.exists cacheFolder) (FS.mkdir cacheFolder)

  let
    onMiss = do
      log $ "No cache hit for " <> show path
      result <- action
      lift $ FS.writeTextFile UTF8 objectPath (dump result)
      pure result

  cacheHit >>= case _ of
    true -> do
      strResult <- lift $ FS.readTextFile UTF8 objectPath
      case (fromJson strResult) of
        Right res -> pure res
        Left err -> do
          log $ "Unable to read cache file " <> err
          onMiss
    false -> do
      onMiss

-- The exclusions file pairs package name keys with an array of versions that
-- should be excluded.
readExclusionsFile :: Aff (Map String (Array (Maybe String)))
readExclusionsFile = do
  str <- FS.readTextFile UTF8 exclusionsFile
  case Json.parseJson str >>= Json.decodeJson of
    Left e ->
      throwError $ Aff.error $ Json.printJsonDecodeError e
    Right (v :: Object (Array (Maybe String))) ->
      pure $ Map.fromFoldable (Object.toUnfoldable v :: Array _)

-- | Write packages that failed because they do not have a Bowerfile out to an
-- | exclusions file so that we don't attempt to fetch their Bowerfile each time
-- | the tool runs.
writeExclusionsFile :: PackageFailures -> Aff Unit
writeExclusionsFile failures =
  for_ (toExclusionsObject failures) (writeJsonFile exclusionsFile)

-- | Segment out packages that should be excluded from future runs of the Bower
-- | import tool. Packages should be excluded if we cannot get a valid Bowerfile.
toExclusionsObject :: PackageFailures -> Maybe (Object (Array (Maybe String)))
toExclusionsObject =
  (\obj -> if Object.isEmpty obj then Nothing else Just obj)
    <<< map (map (map _.name))
    <<< Object.fromFoldable
    <<< map (lmap stripPureScriptPrefix)
    <<< (Map.toUnfoldable :: _ -> Array (Tuple String (Array (Maybe GitHub.Tag))))
    <<< map (map fst <<< Map.toUnfoldable)
    <<< filterErrors
  where
  -- If we reach the point where we attempt to get a Bowerfile, but we can't
  -- get one for a package, then one of these errors will be thrown. We can use
  -- them to skip fetching unavailable files.
  condition = case _ of
    MissingBowerfile -> true
    MalformedBowerJson _ -> true
    _ -> false

  filterErrors = Map.mapMaybe \a -> case Map.filter condition a of
    newMap
      | Map.isEmpty newMap -> Nothing
      | otherwise -> Just newMap

-- | Write package failures out to a diagnostics file
writeFailuresFile :: PackageFailures -> Aff Unit
writeFailuresFile = writeJsonFile errorsFile <<< toFailureObject

toFailureObject
  :: PackageFailures
  -- Map ErrorName (Map PackageName (Map (Maybe Tag) (Maybe (Array DetailedError))))
  -> Object (Object (Object String))
toFailureObject m = do
  foldlWithIndex foldFn Object.empty m
  where
  foldFn package exclusionMap errorMap = do
    let new = Object.fromFoldable $ (Map.toUnfoldable :: _ -> Array _) $ reassociate package errorMap
    Object.unionWith (Object.unionWith append) new exclusionMap

  reassociate
    :: String
    -> Map (Maybe GitHub.Tag) ImportError
    -> Map String (Object (Object String))
  reassociate package semverMap = foldlWithIndex foldFn' Map.empty semverMap
    where
    foldFn' mbSemVer exclusionMap importError = do
      let
        errorKey = BowerImport.Error.printImportErrorKey importError
        errorDetail = BowerImport.Error.printImportError importError
        version = case mbSemVer of
          Nothing -> "null"
          Just tag -> tag.name

      Map.insertWith (Object.unionWith append) errorKey (Object.singleton package (Object.singleton version errorDetail)) exclusionMap

-- | Get the list of packages in the Bower and PureScript registries.
readRegistryPackages :: Aff (Map RawPackageName String)
readRegistryPackages = do
  bowerRegistry <- readRegistry "bower-packages.json"
  purescriptRegistry <- readRegistry "new-packages.json"
  pure $ Map.union bowerRegistry purescriptRegistry
  where
  readRegistry :: FilePath -> Aff (Map RawPackageName String)
  readRegistry source = do
    let decodeRegistry = Json.decodeJson <=< Json.parseJson
    registryFile <- FS.readTextFile UTF8 ("../" <> source)
    case decodeRegistry registryFile of
      Left err -> throwError $ Aff.error $ Array.fold
        [ "Decoding "
        , source
        , "failed with error:\n\n"
        , Json.printJsonDecodeError err
        ]
      Right (packages :: Object String) -> do
        let toArray = Object.toArrayWithKey \k -> Tuple (stripPureScriptPrefix k)
        pure $ Map.fromFoldable $ toArray packages
