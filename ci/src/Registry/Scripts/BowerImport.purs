module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Debug as Debug
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now (nowDateTime) as Time
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub as GitHub
import Foreign.Object as Foreign
import Foreign.SPDX as SPDX
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Node.FS.Sync as FSE
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Registry.Index (RegistryIndex)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Text.Parsing.StringParser as Parser
import Web.Bower.PackageMeta (Dependencies(..))
import Web.Bower.PackageMeta as Bower

type PackageName = String

type ReleasesIndex = Map PackageName PackageReleases

type PackageReleases = { address :: GitHub.Address, releases :: Set GitHub.Tag }

-- | This main loop uploads legacy packages to the new Registry
-- | In order to do this, we:
-- | - get an index of the legacy packages with their bowerfiles
-- | - create a graph (a tree really) where a package is a node and dependencies are edges
-- | - topologically sort this graph so that packages with no dependencies are at the root
-- | - go through this list: if the package is in the registry index then skip, otherwise upload
main :: Effect Unit
main = Aff.launchAff_ do
  log "Starting import from Bower.."
  registry <- downloadBowerRegistry
  Debug.traceM registry
-- TODO: upload packages

-- | We go through all the legacy packages, and:
-- | - collect all the releases of each package
-- | - download the bower.json file for every release, caching it
-- | - return an index of this "Bower Registry"
downloadBowerRegistry :: Aff RegistryIndex
downloadBowerRegistry = do
  -- Get the lists of packages: Bower packages and new packages
  -- Assumption: we are running in the `ci` folder or the registry repo
  bowerPackagesStr <- FS.readTextFile UTF8 "../bower-packages.json"
  newPackagesStr <- FS.readTextFile UTF8 "../new-packages.json"

  let
    parseJsonMap str =
      Json.jsonParser str
        >>= (Json.decodeJson >>> lmap Json.printJsonDecodeError)
        >>> map (Map.fromFoldableWithIndex :: Foreign.Object String -> Map String String)

  case parseJsonMap bowerPackagesStr, parseJsonMap newPackagesStr of
    Left err, _ -> throw $ "Error: couldn't parse bower-packages.json, error: " <> err
    _, Left err -> throw $ "Error: couldn't parse new-packages.json, error: " <> err
    Right bowerPackages, Right newPackages -> do
      -- as first thing we iterate through all the packages and fetch all the
      -- releases from GitHub, to populate an in-memory "releases index".
      -- This is necessary so that we can do the "self-containment" check later.
      -- We keep a temporary cache on disk, so that it's easier to do development
      -- without consuming the GitHub request limit.
      let (SemigroupMap allPackages) = SemigroupMap bowerPackages <> SemigroupMap newPackages
      releaseIndex :: ReleasesIndex <- withCache ("releaseIndex") (Just $ Hours 1.0) $ Map.fromFoldable <$> forWithIndex allPackages \nameWithPrefix repoUrl -> do
        let name = stripPureScriptPrefix nameWithPrefix
        let address = fromRight' (\_ -> unsafeCrashWith $ "Failed to parse the repo url: " <> show repoUrl) $ GitHub.parseRepo repoUrl
        releases <- withCache ("releases__" <> address.owner <> "__" <> address.repo) (Just $ Hours 24.0) do
          log $ "Fetching releases for package " <> show name
          Set.fromFoldable <$> GitHub.getReleases address
        pure $ Tuple name { releases, address }

      -- TODO: return a { success :: RegistryIndex, errors :: Map ErrorType (Map PackageName Version) }
      -- once we have the index we can go through it and write to file all
      -- the manifests that we're missing
      bowerRegistry <- forWithIndex releaseIndex \name { address, releases } -> do
        -- some releases should be skipped because they have issues, see `toSkip`
        let
          (workingReleases :: Array GitHub.Tag) = Set.toUnfoldable $
            Set.filter (\release -> not $ Set.member (Tuple name release.name) toSkip) releases
        Map.fromFoldable <$> for workingReleases \release -> do
          manifest <- withCache ("manifest__" <> name <> "__" <> release.name) Nothing do
            -- we download the Bower file or use the cached one if available.
            -- note that we don't need to expire the cache ever here, because the
            -- tags are supposed to be immutable
            let
              fetchBowerfile = do
                let url = "https://raw.githubusercontent.com/" <> address.owner <> "/" <> address.repo <> "/" <> release.name <> "/bower.json"
                log $ "Fetching bowerfile: " <> url
                Http.get ResponseFormat.json url >>= case _ of
                  Left err -> do
                    error $ "Got error while fetching bowerfile, you might want to add the following to the packages to skip: " <> show name <> " /\\ " <> show release.name
                    Aff.throwError $ Exception.error $ Http.printError err
                  Right { body } -> case Json.decodeJson body of
                    Left err -> Aff.throwError $ Exception.error $ Json.printJsonDecodeError err
                    Right (bowerfile :: Bower.PackageMeta) -> pure bowerfile
            bowerfile <- withCache ("bowerfile__" <> name <> "__" <> release.name) Nothing fetchBowerfile
            -- then we check if all dependencies/versions are self-contained in the registry
            case selfContainedDependencies releaseIndex bowerfile of
              Just somePkgs ->
                throw $ Array.fold
                  [ "Dependencies for the package "
                  , show name
                  , "@"
                  , release.name
                  , " are not all contained in the registry, skipping."
                  , "\nDeps are:\n"
                  , show somePkgs
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

      -- TODO: process the errors above and write them in the bower-exclusions.json file
      pure bowerRegistry

readBowerfile :: String -> Aff (Either String Bower.PackageMeta)
readBowerfile path = do
  let fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)
  ifM (not <$> FS.exists path)
    (pure $ Left $ "Bowerfile not found at " <> path)
    do
      strResult <- FS.readTextFile UTF8 path
      pure $ fromJson strResult

-- | Convert a Bowerfile into a Registry Manifest
toManifest :: Bower.PackageMeta -> String -> Repo -> Either String Manifest
toManifest (Bower.PackageMeta bowerfile) ref address = do
  let
    toDepPair { packageName, versionRange }
      = map (Tuple $ cleanPackageName packageName)
      $ note ("Failed to parse range: " <> versionRange)
      $ SemVer.parseRange versionRange
    subdir = Nothing
    repository = case _.url <$> bowerfile.repository of
      Nothing -> address
      Just url -> case GitHub.parseRepo url of
        Left _err -> Git { url, subdir }
        Right { repo, owner } -> GitHub { repo, owner, subdir }

  name <- lmap Parser.printParserError $ PackageName.parse $ bowerfile.name
  -- We fix the license for some old package versions
  licenseArray <- traverse SPDX.parse case bowerfile.license of
    [ "Apache 2.0" ] -> [ "Apache-2.0" ]
    [ "BSD" ] -> [ "BSD-3-Clause" ]
    other -> other
  license <-
    if Array.null licenseArray then Left "License missing"
    else Right $ SPDX.joinWith SPDX.Or licenseArray
  version <- note ("Could not parse version: " <> ref) $ SemVer.parseSemVer ref
  deps <- traverse toDepPair $ List.fromFoldable $ un Dependencies bowerfile.dependencies
  devDeps <- traverse toDepPair $ List.fromFoldable $ un Dependencies bowerfile.devDependencies

  let
    targets = Foreign.fromFoldable $ Array.catMaybes
      [ Just $ Tuple "lib"
          { sources: [ "src/**/*.purs" ]
          , dependencies: Foreign.fromFoldable deps
          }
      , if (List.null devDeps) then Nothing
        else Just $ Tuple "test"
          { sources: [ "src/**/*.purs", "test/**/*.purs" ]
          , dependencies: Foreign.fromFoldable (deps <> devDeps)
          }
      ]

  pure { name, license, repository, targets, version }

-- | Are all the dependencies PureScript packages or are there any external Bower/JS packages?
selfContainedDependencies :: ReleasesIndex -> Bower.PackageMeta -> Maybe (Array String)
selfContainedDependencies packageIndex (Bower.PackageMeta { dependencies, devDependencies }) =
  let
    (Bower.Dependencies allDeps) = dependencies <> devDependencies
    isInRegistry { packageName } = case Map.lookup (cleanPackageName packageName) packageIndex of
      Nothing -> Just $ cleanPackageName packageName
      Just _ -> Nothing
    outsideDeps = Array.catMaybes (map isInRegistry allDeps)
  in
    if Array.null outsideDeps then Nothing
    else Just outsideDeps

-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall a
   . Json.DecodeJson a
  => Json.EncodeJson a
  => String
  -> Maybe Hours
  -> Aff a
  -> Aff a
withCache path maybeDuration action = do
  let cacheFolder = ".cache"
  let objectPath = cacheFolder <> "/" <> path
  let dump = Json.encodeJson >>> stringifyWithIndent 2
  let fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)
  let yolo a = unsafePartial $ fromJust a
  let
    cacheHit = do
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
  unlessM (FS.exists cacheFolder) (FS.mkdir cacheFolder)
  cacheHit >>= case _ of
    true -> do
      log $ "Using cache for " <> show path
      strResult <- FS.readTextFile UTF8 objectPath
      case (fromJson strResult) of
        Right res -> pure res
        -- Here we just blindly assume that we are the only ones to serialize here
        Left err -> Aff.throwError $ Exception.error err
    false -> do
      result <- action
      FS.writeTextFile UTF8 objectPath (dump result)
      pure result

cleanPackageName :: String -> String
cleanPackageName raw = stripPureScriptPrefix $ case String.split (String.Pattern "/") raw of
  [ p ] -> p
  [ _owner, repo ] -> repo -- e.g. in abc-parser@1.1.2
  _ -> unsafeCrashWith $ "Couldn't parse package name " <> show raw

throw :: forall m a. MonadThrow Aff.Error m => String -> m a
throw = Aff.throwError <<< Aff.error

type ToSkip =
  { missingLicense :: Foreign.Object (Array String)
  , malformedLicense :: Foreign.Object (Array String)
  , missingBowerfile :: Foreign.Object (Array String)
  , malformedBowerfile :: Foreign.Object (Array String)
  , dependOnCommitHashOrBranch :: Foreign.Object (Array String)
  , dependenciesOutsideOfRegistry :: Foreign.Object (Array String)
  , invalidRelease :: Foreign.Object (Array String)
  }

toSkip :: Set (Tuple String String)
toSkip = unsafePerformEffect $ do
  exclusionsStr <- FSE.readTextFile UTF8 "./bower-exclusions.json"

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
  f :: Foreign.Object (Array String) -> Array (Tuple String String)
  f o =
    let
      unpack :: Tuple String (Array String) -> Array (Tuple String String)
      unpack (packageName /\ versions) = map (\version -> packageName /\ version) versions
    in
      Array.foldMap unpack $ Foreign.toUnfoldable o
