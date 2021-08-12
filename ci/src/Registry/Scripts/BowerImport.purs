module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, lift, mapExceptT, runExceptT)
import Control.Monad.State (modify, runStateT)
import Data.Argonaut (decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array (fold, foldMap)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Debug as Debug
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now (nowDateTime) as Time
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer (Range, SemVer)
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Record as Record
import Registry.PackageMap (PackageMap(..))
import Registry.PackageMap as PackageMap
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Scripts.BowerImport.Error (ImportError(..), ManifestError(..))
import Type.Proxy (Proxy(..))
import Web.Bower.PackageMeta (Dependencies(..))
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
  { manifests, failures } <- downloadLegacyRegistry
  Debug.traceM manifests
  Debug.traceM failures
-- TODO: upload packages

-- | A data structure representing package names that have successfully been
-- | imported from the old registries and can be added to the new registry.
type PackageManifests = PackageMap (NonEmptyArray Manifest)

-- | A data structure representing packages and versions that do not produce a
-- | valid manifest, along with information about why they fail to import.
type PackageFailures = PackageMap (Map (Maybe SemVer) ImportError)

-- | We go through all the legacy packages, and:
-- | - collect all the releases of each package
-- | - download the bower.json file for every release, caching it
-- | - return an index of this "Bower Registry"
downloadLegacyRegistry :: Aff { manifests :: PackageManifests, failures :: PackageFailures }
downloadLegacyRegistry = do
  allPackages <- readRegistryPackages
  Tuple failures manifests <- convertPackages allPackages
  pure { manifests, failures }

-- | Convert legacy packages to package manifests, if possible, collecting
-- | failed packages along the way.
convertPackages :: PackageMap Repo -> Aff (Tuple PackageFailures PackageManifests)
convertPackages allPackages = do
  runStep getReleases (Tuple PackageMap.empty allPackages)
    >>= runStep fetchBowerfiles
    >>= runStep (checkSelfContained allPackages)
    >>= runStep convertToManifest

-- | Perform an effectful transformation on a package, returning the transformed
-- | package. If a package cannot be transformed, throw an `ImportError`
-- | exception via `ExceptT` and the error will be collected.
--
-- TODO: Support the ability to abort a specific _version_ of a package, and not
-- just abort the entire package. This won't be possible in the 'fetch releases'
-- stage, but it will be possible once all releases are fetched.
newtype Step a b = Step (PackageName -> a -> ExceptT ImportError Aff b)

-- TODO: This is pretty janky; ideally we could just compose steps without
-- having to run the state each time. But maybe type-changing state ain't a
-- viable thing.
runStep
  :: forall a b
   . Step a b
  -> Tuple PackageFailures (PackageMap a)
  -> Aff (Tuple PackageFailures (PackageMap b))
runStep (Step run) (Tuple bad (PackageMap m)) = do
  Tuple _ (Tuple bad' good) <- runStateT st (Tuple bad PackageMap.empty)
  pure $ Tuple bad' good
  where
  st = do
    forWithIndex_ m \package a -> do
      b <- lift $ runExceptT (run package a)
      case b of
        Left e ->
          modify (lmap (PackageMap.insert package (Map.singleton Nothing e)))
        Right v ->
          modify (map (PackageMap.insert package v))

type PackageReleases =
  { address :: GitHub.Address
  , releases :: Set GitHub.Tag
  }

-- | Find all released tags for the package.
getReleases :: Step Repo PackageReleases
getReleases = Step \package repo -> do
  withCache "releaseIndex" (Just $ Hours 1.0) do
    let name = PackageName.print package
    address <- case repo of
      Git _ ->
        throwError NotOnGitHub
      GitHub address ->
        pure $ Record.delete (Proxy :: Proxy "subdir") address
    let repoCache = fold [ "releases__", address.owner, "__", address.repo ]
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
  }

-- | Find the bower.json files associated with the package's relaesed tags.
fetchBowerfiles :: Step PackageReleases PackageBowerfiles
fetchBowerfiles = Step \_ { address, releases } -> do
  let
    mkUrl { name } =
      "https://raw.githubusercontent.com/"
        <> address.owner
        <> "/"
        <> address.repo
        <> "/"
        <> name
        <> "/bower.json"

  -- TODO: Exclude releases that shouldn't be included
  -- TODO: Use cache
  bowerfiles <- for (Set.toUnfoldable releases :: Array _) \release -> do
    let url = mkUrl release
    Bower.PackageMeta bowerfile <- lift (Http.get ResponseFormat.json url) >>= case _ of
      Left _ -> do
        throwError $ MissingBowerfile
      Right { body } -> case Json.decodeJson body of
        Left err ->
          throwError $ MalformedBowerJson err
        Right bowerfile ->
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
        [ package ] -> Right package
        [ _owner, repo ] -> Right repo
        _ -> Left $ "Couldn't parse package name " <> show raw

      parseName packageName = case normalizePackageName packageName of
        Left _ ->
          Left packageName
        Right name ->
          lmap (const packageName) $ PackageName.parse name

      parsePairs = map \{ packageName, versionRange } -> case parseName packageName of
        Left e -> Left e
        Right name -> Right (Tuple name versionRange)

      partitionDeps = parsePairs >>> foldMap case _ of
        Left err -> { fail: [ err ], success: [] }
        Right res -> { fail: [], success: [ res ] }

      normalizeDeps deps = do
        let { fail, success } = partitionDeps deps
        case NEA.fromArray fail of
          Nothing -> pure success
          Just errs -> throwError $ InvalidDependencyNames errs

    deps <- normalizeDeps $ un Dependencies bowerfile.dependencies
    devDeps <- normalizeDeps $ un Dependencies bowerfile.devDependencies

    let
      normalized =
        { name: bowerfile.name
        , license: bowerfile.license
        , dependencies: deps
        , devDependencies: devDeps
        }

    pure $ Tuple release normalized

  pure { address, bowerfiles: Map.fromFoldable bowerfiles }

-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
checkSelfContained :: PackageMap Repo -> Step PackageBowerfiles PackageBowerfiles
checkSelfContained registry = Step \_ { address, bowerfiles } -> do
  new <- for bowerfiles \bowerfile@{ dependencies, devDependencies } -> do
    let
      allDeps :: Array (Tuple PackageName String)
      allDeps = dependencies <> devDependencies

      outsideDeps :: Array (Maybe PackageName)
      outsideDeps = allDeps <#> \(Tuple name _) -> case PackageMap.lookup name registry of
        Nothing -> Just name
        Just _ -> Nothing

    case NEA.fromArray $ Array.catMaybes outsideDeps of
      Nothing ->
        pure (bowerfile :: Bowerfile)
      Just outside ->
        throwError $ NonRegistryDependencies outside

  pure { address, bowerfiles: new }

convertToManifest :: Step PackageBowerfiles (NonEmptyArray Manifest)
convertToManifest = Step \package { address, bowerfiles } -> do
  let
    repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
    toManifest' release meta = mapExceptT (map (lmap ManifestError)) (toManifest package repo release meta)
  releaseMap <- forWithIndex bowerfiles toManifest'
  case NEA.fromArray (Array.fromFoldable $ Map.values releaseMap) of
    Nothing ->
      throwError NoManifests
    Just manifests ->
      pure manifests

toManifest :: PackageName -> Repo -> GitHub.Tag -> Bowerfile -> ExceptT ManifestError Aff Manifest
toManifest package repository release bowerfile = do
  name <- case PackageName.parse bowerfile.name of
    Left _ ->
      throwError $ MismatchedName { expected: package, received: bowerfile.name }
    Right name | name /= package ->
      throwError $ MismatchedName { expected: package, received: bowerfile.name }
    Right name ->
      pure name

  license <- do
    let
      rewrite = case _ of
        [ "Apache 2.0" ] -> [ "Apache-2.0" ]
        [ "BSD" ] -> [ "BSD-3-Clause" ]
        other -> other

    licenses <- for (rewrite bowerfile.license) \license ->
      case SPDX.parse license of
        Left err ->
          throwError $ BadLicense err
        Right res ->
          pure res

    case licenses of
      [] -> throwError MissingLicense
      arr -> pure $ SPDX.joinWith SPDX.Or arr

  version <- case SemVer.parseSemVer release.name of
    Nothing ->
      throwError $ BadVersion release.name
    Just version ->
      pure version

  targets <- do
    let
      checkDepPair (Tuple packageName versionStr) =
        case SemVer.parseRange versionStr of
          Nothing -> Left { dependency: packageName, failedBounds: versionStr }
          Just range -> Right $ Tuple (PackageName.print packageName) range

      partition = foldMap case _ of
        Left err -> { fail: [ err ], success: [] }
        Right res -> { fail: [], success: [ res ] }

      readDeps = map checkDepPair >>> partition >>> \{ fail, success } -> case NEA.fromArray fail of
        Nothing ->
          pure success
        Just errs ->
          throwError $ BadDependencyVersions errs

    deps :: Array (Tuple String Range) <- readDeps bowerfile.dependencies
    devDeps <- readDeps bowerfile.devDependencies

    pure $ Object.fromFoldable $ Array.catMaybes
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

  pure { name, license, repository, targets, version }

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
  let dump = Json.encodeJson >>> stringifyWithIndent 2
  let fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)
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
      log $ "Using cache for " <> show path
      strResult <- FS.readTextFile UTF8 objectPath
      case (fromJson strResult) of
        Right res -> pure res
        -- Here we just blindly assume that we are the only ones to serialize here
        Left err -> Aff.throwError $ Exception.error err
    false -> do
      result <- action
      lift $ FS.writeTextFile UTF8 objectPath (dump result)
      pure result

type ToSkip =
  { missingLicense :: PackageMap (Array String)
  , malformedLicense :: PackageMap (Array String)
  , missingBowerfile :: PackageMap (Array String)
  , malformedBowerfile :: PackageMap (Array String)
  , dependOnCommitHashOrBranch :: PackageMap (Array String)
  , dependenciesOutsideOfRegistry :: PackageMap (Array String)
  , invalidRelease :: PackageMap (Array String)
  }

readPackagesToSkip :: Aff (Set (Tuple PackageName String))
readPackagesToSkip = do
  exclusionsStr <- FS.readTextFile UTF8 "./bower-exclusions.json"
  let parseJson str = Json.jsonParser str >>= (Json.decodeJson >>> lmap Json.printJsonDecodeError)
  case parseJson exclusionsStr of
    Left err -> unsafeCrashWith $ "Error: couldn't parse bower-exclusions.json, error: " <> err
    Right (o :: ToSkip) -> pure $ Set.fromFoldable
      $ f o.missingLicense
      <> f o.malformedLicense
      <> f o.missingBowerfile
      <> f o.malformedBowerfile
      <> f o.dependOnCommitHashOrBranch
      <> f o.dependenciesOutsideOfRegistry
      <> f o.invalidRelease
  where
  f :: PackageMap (Array String) -> Array (Tuple PackageName String)
  f (PackageMap m) =
    let
      unpack :: Tuple PackageName (Array String) -> Array (Tuple PackageName String)
      unpack (packageName /\ versions) = map (\version -> packageName /\ version) versions
    in
      Array.foldMap unpack $ Map.toUnfoldable m

-- | Get the list of packages in the Bower and PureScript registries.
readRegistryPackages :: Aff (PackageMap Repo)
readRegistryPackages = do
  bowerRegistry <- readRegistry "bower-packages.json"
  purescriptRegistry <- readRegistry "new-packages.json"
  pure $ PackageMap.union bowerRegistry purescriptRegistry
  where
  readRegistry :: FilePath -> Aff (PackageMap Repo)
  readRegistry source = do
    let decodeRegistry = decodeJson <=< parseJson
    registryFile <- FS.readTextFile UTF8 ("../" <> source)
    case decodeRegistry registryFile of
      Left err -> throwError $ Aff.error $ fold
        [ "Error: Decoding "
        , source
        , "failed with error:\n\n"
        , printJsonDecodeError err
        ]
      Right packages ->
        pure packages

readBowerfile :: String -> Aff (Either String Bower.PackageMeta)
readBowerfile path = do
  let fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)
  ifM (not <$> FS.exists path)
    (pure $ Left $ "Bowerfile not found at " <> path)
    do
      strResult <- FS.readTextFile UTF8 path
      pure $ fromJson strResult

{- OLD IMPLEMENTATION
-- TODO: return a { success :: RegistryIndex, errors :: Map ErrorType (Map PackageName Version) }
-- once we have the index we can go through it and write to file all
-- the manifests that we're missing
_ <- forWithIndex releaseIndex \name { address, releases } -> do
  -- some releases should be skipped because they have issues
  toSkip <- readPackagesToSkip
  let
    (workingReleases :: Array GitHub.Tag) = Set.toUnfoldable $
      Set.filter (\release -> not $ Set.member (Tuple name release.name) toSkip) releases

  Map.fromFoldable <$> for workingReleases \release -> do
    manifest <- withCache ("manifest__" <> PackageName.print name <> "__" <> release.name) Nothing do
      -- we download the Bower file or use the cached one if available.
      -- note that we don't need to expire the cache ever here, because the
      -- tags are supposed to be immutable
      let
        fetchBowerfile = do
          let url = "https://raw.githubusercontent.com/" <> address.owner <> "/" <> address.repo <> "/" <> release.name <> "/bower.json"
          log $ "Fetching bowerfile: " <> url
          Http.get ResponseFormat.json url >>= case _ of
            Left err -> do
              error $ "Got error while fetching bowerfile, you might want to add the following to the packages to skip: " <> PackageName.print name <> " /\\ " <> show release.name
              Aff.throwError $ Exception.error $ Http.printError err
            Right { body } -> case Json.decodeJson body of
              Left err -> Aff.throwError $ Exception.error $ Json.printJsonDecodeError err
              Right (bowerfile :: Bower.PackageMeta) -> pure bowerfile
      bowerfile <- withCache ("bowerfile__" <> PackageName.print name <> "__" <> release.name) Nothing fetchBowerfile
      -- then we check if all dependencies/versions are self-contained in the registry
      case selfContainedDependencies releaseIndex bowerfile of
        Just somePkgs ->
          throw $ Array.fold
            [ "Dependencies for the package "
            , PackageName.print name
            , "@"
            , release.name
            , " are not all contained in the registry, skipping."
            , "\nDeps are:\n"
            , show $ map PackageName.print somePkgs
            ]
        Nothing ->
          let
            eitherManifest = toManifest bowerfile release.name
              $ GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
          in
            case eitherManifest of
              Right manifest -> pure manifest
              Left err -> throw $ "Could not make a manifest for Bowerfile: " <> show bowerfile <> "\n\nError: " <> err

    pure (release /\ manifest)
-}
