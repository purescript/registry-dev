module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, except, lift, mapExceptT, runExceptT)
import Control.Monad.State (StateT, modify, runStateT)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, parseJson, printJsonDecodeError)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (adjust) as Time
import Data.FoldableWithIndex (foldlWithIndex)
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Data.Tuple (fst)
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now (nowDateTime) as Time
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Partial.Unsafe (unsafePartial)
import Registry.PackageMap (PackageMap(..))
import Registry.PackageMap as PackageMap
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Scripts.BowerImport.Error (ImportError(..), ManifestError(..), printImportError, printImportErrorKey)
import Web.Bower.PackageMeta (Dependencies(..))
import Web.Bower.PackageMeta as Bower

{- TODO

  - [ ] Change `runStep` so that it iterates through the package names AND the
        individual versions of a package, so that exceptions are raised in
        relation to a package + a version, but the next step iterates through the
        next version instead of dropping the package altogether.

  - [ ] Adjust exclusions so that packages are only excluded by particular
        versions, or by all versions, but not _always_ excluded by all versions.
        See: bifunctors.

  - [ ] Adjust the `fetchBowerfiles` step so that it only fetches the files
        themselves with no processing. That way, we can fill the exclusions
        file only with packages truly missing bowerfiles. This maximizes our
        ability to see errors for all other reasons, but not hit the GitHub API
        to do so.

  - [ ] Remove the `PackageMap` and `Step` newtypes, and write new functions
        `encodeManifests` and `encodeExclusions` that just use objects.
-}

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
  { manifests, failures } <- downloadLegacyRegistry
  log "Writing exclusions, errors, and manifest files..."
  for_ (toExclusionsObject failures) (writeJsonFile exclusionsFile)
  writeJsonFile errorsFile (toFailureObject failures)
  writeJsonFile manifestsFile manifests
  log "Done!"

-- | A data structure representing package names that have successfully been
-- | imported from the old registries and can be added to the new registry.
type PackageManifests = PackageMap (NonEmptyArray Manifest)

-- | A data structure representing packages and versions that do not produce a
-- | valid manifest, along with information about why they fail to import.
type PackageFailures = Map String (Map (Maybe GitHub.Tag) ImportError)

-- | Convert legacy packages to package manifests, if possible, collecting
-- | failed packages along the way.
downloadLegacyRegistry :: Aff { failures :: PackageFailures, manifests :: PackageManifests }
downloadLegacyRegistry = do
  log "Reading registry packages..."
  init@(Tuple _ allPackages) <- readRegistryPackages
  log "Fetching package releases..."
  Tuple failures manifests <-
    runStep getReleases init
      >>= (\tup -> log "Fetching package bowerfiles..." *> runStep fetchBowerfiles tup)
      >>= (\tup -> log "Checking package dependencies are in the registry..." *> runStep (checkSelfContained allPackages) tup)
      >>= (\tup -> log "Converting package bowerfiles to manifests..." *> runStep convertToManifest tup)
  pure { failures, manifests }

-- | Perform an effectful transformation on a package, returning the transformed
-- | package. If a package cannot be transformed, throw an `ImportError`
-- | exception via `ExceptT` and the error will be collected.
newtype Step a b = Step (PackageName -> a -> ExceptT (Tuple (Maybe GitHub.Tag) ImportError) Aff b)

-- TODO: This needs to be changed to be a nested traversal, once through the
-- package name and then through each version. That way exceptions are raised
-- for specific versions, not for a package as a whole, and successful versions
-- of a package still make it through.
runStep
  :: forall a b
   . Step a b
  -> Tuple PackageFailures (PackageMap a)
  -> Aff (Tuple PackageFailures (PackageMap b))
runStep (Step run) (Tuple bad (PackageMap good)) = do
  Tuple _ (Tuple bad' good') <- runStateT st (Tuple bad PackageMap.empty)
  pure $ Tuple bad' good'
  where
  st = do
    forWithIndex_ good \package a -> do
      res <- lift $ runExceptT (run package a)
      case res of
        Left (Tuple tag err) -> do
          modify (lmap (Map.insertWith Map.union (PackageName.print package) (Map.singleton tag err)))
        Right v ->
          modify (map (PackageMap.insert package v))

type PackageReleases =
  { address :: GitHub.Address
  , releases :: Set GitHub.Tag
  }

-- | Find all released tags for the package.
getReleases :: Step GitHub.Address PackageReleases
getReleases = Step \package address -> do
  let
    name = PackageName.print package
    repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]
  releases <- withCache repoCache (Just $ Hours 24.0) do
    log $ "Fetching releases for package " <> name
    Set.fromFoldable <$> lift (GitHub.getReleases address)
  pure { releases, address }

type PackageBowerfiles =
  { address :: GitHub.Address
  , bowerfiles :: Map GitHub.Tag Bowerfile
  }

-- | A normalized bowerfile type for ease of use
type Bowerfile =
  { name :: String
  , license :: Array String
  , dependencies :: Array (Tuple PackageName String)
  , devDependencies :: Array (Tuple PackageName String)
  , source :: String
  }

-- | Find the bower.json files associated with the package's relaesed tags.
fetchBowerfiles :: Step PackageReleases PackageBowerfiles
fetchBowerfiles = Step \package { address, releases } -> do
  let
    mkUrl { name } =
      "https://raw.githubusercontent.com/"
        <> address.owner
        <> "/"
        <> address.repo
        <> "/"
        <> name
        <> "/bower.json"

    mkCache release =
      "bowerfile__" <> PackageName.print package <> "__" <> release.name

  bowerfiles <- for (Set.toUnfoldable releases :: Array _) \release -> withCache (mkCache release) Nothing do
    let url = mkUrl release
    Bower.PackageMeta bowerfile <- lift (Http.get ResponseFormat.json url) >>= case _ of
      Left _ -> do
        throwError $ Tuple (Just release) MissingBowerfile
      Right { body } -> case decodeJson body of
        Left err ->
          throwError $ Tuple (Just release) $ MalformedBowerJson err
        Right bowerfile -> do
          pure bowerfile

    -- Before continuing, we'll normalize the Bowerfile as much as necessary for
    -- checks after this one. For now, all we do is ensure the dependency names
    -- are well-formed, but these checks could be also moved to their own steps
    -- so we know we have a well-formed Bowerfile before we even attempt to go
    -- on and create a manifest.
    let
      -- Packages can be specified either in 'package-name' format or
      -- in owner/package-name format. This function ensures we don't pick
      -- up owner names as part of package names.
      --
      -- Example:
      -- https://github.com/newlandsvalley/purescript-abc-parser/blob/1.1.2/bower.json
      normalizePackageName raw = case String.split (String.Pattern "/") raw of
        [ packageName ] -> Right packageName
        [ _owner, repo ] -> Right repo
        _ -> Left $ "Couldn't parse package name " <> show raw

      parseName packageName = case normalizePackageName packageName of
        Left _ ->
          Left packageName
        Right name ->
          lmap (const packageName) $ PackageName.parse $ stripPureScriptPrefix name

      parsePairs = map \{ packageName, versionRange } -> case parseName packageName of
        Left e -> Left e
        Right name -> Right (Tuple name versionRange)

      normalizeDeps deps = do
        let { fail, success } = partitionEithers $ parsePairs deps
        case NEA.fromArray fail of
          Nothing -> pure success
          Just errs -> throwError $ Tuple (Just release) $ InvalidDependencyNames errs

    deps <- normalizeDeps $ un Dependencies bowerfile.dependencies
    devDeps <- normalizeDeps $ un Dependencies bowerfile.devDependencies

    let
      normalized =
        { name: bowerfile.name
        , license: bowerfile.license
        , dependencies: deps
        , devDependencies: devDeps
        , source: url
        }

    pure $ Tuple release normalized

  pure { address, bowerfiles: Map.fromFoldable bowerfiles }

-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
checkSelfContained :: PackageMap GitHub.Address -> Step PackageBowerfiles PackageBowerfiles
checkSelfContained registry = Step \_ { address, bowerfiles } -> do
  new <- forWithIndex bowerfiles \release bowerfile@{ dependencies, devDependencies } -> do
    let
      allDeps :: Array (Tuple PackageName String)
      allDeps = dependencies <> devDependencies

      outsideDeps :: Array (Maybe PackageName)
      outsideDeps = allDeps <#> \(Tuple name _) -> case PackageMap.lookup name registry of
        Nothing -> Just name
        Just _ -> Nothing

    case NEA.fromArray $ Array.catMaybes outsideDeps of
      Nothing ->
        pure bowerfile
      Just outside ->
        throwError $ Tuple (Just release) $ NonRegistryDependencies outside

  pure { address, bowerfiles: new }

convertToManifest :: Step PackageBowerfiles (NonEmptyArray Manifest)
convertToManifest = Step \package { address, bowerfiles } -> do
  let
    repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
    toManifest' release meta = mapExceptT (map (lmap (Tuple (Just release) <<< ManifestError))) (toManifest package repo release meta)
  releaseMap <- forWithIndex bowerfiles toManifest'
  case NEA.fromArray (Array.fromFoldable $ Map.values releaseMap) of
    Nothing ->
      throwError $ Tuple Nothing NoManifests
    Just manifests ->
      pure manifests

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest :: PackageName -> Repo -> GitHub.Tag -> Bowerfile -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package repository release bowerfile = do
  let
    mkError :: forall a. ManifestError -> Either (NonEmptyArray ManifestError) a
    mkError = Left <<< NEA.singleton

    eitherName = case PackageName.parse bowerfile.name of
      Right name | name == package ->
        Right name
      Right _ -> do
        mkError $ MismatchedName { expected: package, received: bowerfile.name }
      Left _ -> do
        mkError $ MismatchedName { expected: package, received: bowerfile.name }

    eitherLicense = do
      let
        rewrite = case _ of
          [ "Apache 2.0" ] -> [ "Apache-2.0" ]
          [ "BSD" ] -> [ "BSD-3-Clause" ]
          other -> other

        { fail, success } = partitionEithers $ SPDX.parse <$> rewrite bowerfile.license

      case fail of
        [] -> case success of
          [] -> mkError MissingLicense
          arr -> Right $ SPDX.joinWith SPDX.Or arr
        _ ->
          mkError $ BadLicense fail

    eitherVersion = case SemVer.parseSemVer release.name of
      Nothing ->
        mkError $ BadVersion release.name
      Just version ->
        Right version

    eitherTargets = do
      let
        checkDepPair (Tuple packageName versionStr) =
          case SemVer.parseRange versionStr of
            Nothing -> Left { dependency: packageName, failedBounds: versionStr }
            Just range -> Right $ Tuple (PackageName.print packageName) range

        readDeps = map checkDepPair >>> partitionEithers >>> \{ fail, success } ->
          case NEA.fromArray fail of
            Nothing ->
              Right success
            Just err ->
              mkError $ BadDependencyVersions err

        eitherDeps = readDeps bowerfile.dependencies
        eitherDevDeps = readDeps bowerfile.devDependencies

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

    errs = map NEA.concat $ NEA.fromArray $ Array.catMaybes
      [ unpackErrors eitherName
      , unpackErrors eitherLicense
      , unpackErrors eitherVersion
      , unpackErrors eitherTargets
      ]

  case errs of
    Nothing -> do
      -- Technically this shouldn't be needed, since we've already checked these
      -- for errors, but this is just so the types all work out.
      name <- except eitherName
      license <- except eitherLicense
      version <- except eitherVersion
      targets <- except eitherTargets
      pure { name, license, repository, targets, version }

    Just err ->
      throwError err

-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall a
   . DecodeJson a
  => EncodeJson a
  => String
  -> Maybe Hours
  -> ExceptT (Tuple (Maybe GitHub.Tag) ImportError) Aff a
  -> ExceptT (Tuple (Maybe GitHub.Tag) ImportError) Aff a
withCache path maybeDuration action = do
  let cacheFolder = ".cache"
  let objectPath = cacheFolder <> "/" <> path
  let dump = encodeJson >>> stringifyWithIndent 2
  let fromJson = jsonParser >=> (decodeJson >>> lmap printJsonDecodeError)
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
  cacheHit >>= case _ of
    true -> lift do
      strResult <- FS.readTextFile UTF8 objectPath
      case (fromJson strResult) of
        Right res -> pure res
        -- Here we just blindly assume that we are the only ones to serialize here
        Left err -> Aff.throwError $ Exception.error err
    false -> do
      log $ "No cache hit for " <> show path
      result <- action
      lift $ FS.writeTextFile UTF8 objectPath (dump result)
      pure result

-- | Segment out packages that should be excluded from future runs of the Bower
-- | import tool. Packages should be excluded if we cannot get a well-formed
-- | Bowerfile for them. For example, packages should be excluded if they are
-- | not on GitHub, or they have a malformed package name, or they are missing a Bowerfile, or they have
-- | a malformed Bowerfile.
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
  condition = case _ of
    NotOnGitHub -> true
    MalformedPackageName _ -> true
    MissingBowerfile -> true
    MalformedBowerJson _ -> true
    InvalidDependencyNames _ -> true
    _ -> false

  filterErrors = Map.mapMaybe \a -> case Map.filter condition a of
    newMap
      | Map.isEmpty newMap -> Nothing
      | otherwise -> Just newMap

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
        errorKey = printImportErrorKey importError
        errorDetail = printImportError importError
        version = case mbSemVer of
          Nothing -> "null"
          Just tag -> tag.name

      Map.insertWith (Object.unionWith append) errorKey (Object.singleton package (Object.singleton version errorDetail)) exclusionMap

-- | Get the list of packages in the Bower and PureScript registries.
readRegistryPackages :: Aff (Tuple PackageFailures (PackageMap GitHub.Address))
readRegistryPackages = do
  exclusions <- readExclusionsFile
  bowerRegistry <- readRegistry "bower-packages.json"
  purescriptRegistry <- readRegistry "new-packages.json"
  let condition = flip Array.notElem exclusions <<< stripPureScriptPrefix
  toPackageMap $ Object.filterKeys condition $ Object.union bowerRegistry purescriptRegistry
  where
  readRegistry :: FilePath -> Aff (Object String)
  readRegistry source = do
    let decodeRegistry = decodeJson <=< parseJson
    registryFile <- FS.readTextFile UTF8 ("../" <> source)
    case decodeRegistry registryFile of
      Left err -> throwError $ Aff.error $ Array.fold
        [ "Decoding "
        , source
        , "failed with error:\n\n"
        , printJsonDecodeError err
        ]
      Right packages ->
        pure packages

  readExclusionsFile :: Aff (Array String)
  readExclusionsFile = do
    str <- FS.readTextFile UTF8 exclusionsFile
    case parseJson str >>= decodeJson of
      Left e ->
        throwError $ Aff.error $ printJsonDecodeError e
      Right (v :: Object (Array (Maybe String))) ->
        pure $ Object.keys v

  toPackageMap :: Object String -> Aff (Tuple PackageFailures (PackageMap GitHub.Address))
  toPackageMap allPackages = do
    Tuple _ (Tuple bad good) <- runStateT runPackages (Tuple Map.empty PackageMap.empty)
    pure $ Tuple bad good
    where
    runPackages :: StateT (Tuple PackageFailures (PackageMap GitHub.Address)) Aff Unit
    runPackages = forWithIndex_ allPackages \name repo ->
      case PackageName.parse name of
        Left _ ->
          modify (lmap (Map.insert name (Map.singleton Nothing (MalformedPackageName name))))
        Right k' -> case GitHub.parseRepo repo of
          Left _ ->
            modify (lmap (Map.insert name (Map.singleton Nothing NotOnGitHub)))
          Right address ->
            modify (map (PackageMap.insert k' address))

readBowerfile :: String -> Aff (Either String Bower.PackageMeta)
readBowerfile path = do
  let fromJson = jsonParser >=> (decodeJson >>> lmap printJsonDecodeError)
  ifM (not <$> FS.exists path)
    (pure $ Left $ "Bowerfile not found at " <> path)
    do
      strResult <- FS.readTextFile UTF8 path
      pure $ fromJson strResult
