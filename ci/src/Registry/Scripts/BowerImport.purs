module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.List as List
import Data.Map as Map
import Data.Newtype (over2)
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Debug as Debug
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now (nowDateTime) as Time
import Foreign.GitHub as GitHub
import Foreign.Object as Foreign
import Foreign.SPDX as SPDX
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Record as Record
import Registry.PackageMap (PackageMap(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest, Repo(..))
import Text.Parsing.StringParser as Parser
import Type.Proxy (Proxy(..))
import Web.Bower.PackageMeta (Dependencies(..))
import Web.Bower.PackageMeta as Bower

{- TODO:
Go through all packages in the registry at all versions and attempt to produce
a manifest for that package. For now we are only attempting to convert
Bowerfiles, but some packages in the registry have a valid spago.dhall file but
not a valid Bowerfile, and some packages have no valid manifest at all.

The result of importing all packages from the registry should be two data
structures:

1. A map of package names to manifests for all packages at versions that
   produce a valid manifest. This structure will be used to push manifests
   into the registry index and to create tarballs for the registry itself.
2. A map of import errors to package information for all packages at versions
   that do not produce a valid manifest. This structure will be used to fix
   as many packages and versions as we can, and otherwise to list all the
   packages and versions that are not being included in the official registry
   along with why and how they could be fixed.

TODO:

- [x] Create a data structure for successfully-produced manifests
- [x] Create a data structure for packages and versions that do not have a
      valid manifest as output
- [ ] Create a helper function that attempts to produce a manifest for a
      package and version and produces either a success or failure result. Pull
      the relevant functionality out of `main`
- [ ] Create a helper function that can produce all successful manifests and
      failed manifests and creates the data structures in steps 1 & 2.
- [ ] Create a function to write out all failed packages & versions into a
      diagnostics file that we can use to go through and fix broken packages.
- [ ] Rewrite `main` to use these helper functions so that successful manifests
      are stored on disk and failed manifests produce a diagnostics file.
1. Return a Map PackageName (NonEmptyArray Manifest) for all successful packages
2. Return a Map ImportError (NonEmptyArray FailedImport) for packages that fail so that we
  can diagnose what is wrong and whether we can fix it.
-}

type PackageReleases = { address :: GitHub.Address, releases :: Set GitHub.Tag }

-- | A data structure representing package names that have successfully been
-- | imported from the old registries and can be added to the new registry.
type PackageManifests = PackageMap (NonEmptyArray Manifest)

-- | A data structure representing packages and versions that do not produce a
-- | valid manifest, along with information about why they fail to import.
type PackageFailures = PackageMap (Map SemVer (NonEmptyArray ImportError))

-- | An error representing why a package version cannot be imported from the
-- | Bower registry.
data ImportError
  = MissingBowerfile
  | MalformedBowerJson JsonDecodeError
  | NonRegistryDependencies (NonEmptyArray PackageName)
  | ManifestError ManifestError

printImportError :: ImportError -> String
printImportError = case _ of
  MissingBowerfile ->
    "Missing bower file."

  MalformedBowerJson err ->
    "Malformed bower file:\n\n" <> printJsonDecodeError err

  NonRegistryDependencies deps ->
    "Bower file contains dependencies not in the registry:\n\n"
      <> String.joinWith ", " (PackageName.print <$> NEA.toArray deps)

  ManifestError err ->
    printManifestError err

-- | An error representing why a Bowerfile cannot be migrated into a manifest.
data ManifestError
  = MissingName
  | MissingLicense
  | BadLicense String
  -- TODO: What do we do when the Bowerfile version and the tagged version do
  -- not match? It's somewhat common to forget to update the Bowerfile version.
  | BadVersion String
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

printManifestError :: ManifestError -> String
printManifestError = case _ of
  MissingName ->
    "Bower file does not contain a 'name' field."

  MissingLicense ->
    "Bower file does not contain a 'license' field."

  BadLicense err ->
    "Bower file contains non-SPDX license:\n\n" <> err

  BadVersion version ->
    "Bower file declares an invalid version:\n\n" <> version

  BadDependencyVersions deps -> do
    let fromDep { dependency, failedBounds } = PackageName.print dependency <> ": " <> failedBounds
    "Bower file declares one or more dependencies with invalid version bounds:\n\n"
      <> String.joinWith "\n" (fromDep <$> NEA.toArray deps)

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

-- | We go through all the legacy packages, and:
-- | - collect all the releases of each package
-- | - download the bower.json file for every release, caching it
-- | - return an index of this "Bower Registry"
downloadLegacyRegistry :: Aff { manifests :: PackageManifests, failures :: PackageFailures }
downloadLegacyRegistry = do
  allPackages <- readRegistryPackages
  PackageMap releaseIndex <- createReleaseIndex allPackages

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

  -- TODO
  pure { manifests: PackageMap Map.empty, failures: PackageMap Map.empty }

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
selfContainedDependencies :: forall a. Map PackageName a -> Bower.PackageMeta -> Maybe (Array PackageName)
selfContainedDependencies packageIndex (Bower.PackageMeta { dependencies, devDependencies }) =
  let
    (Bower.Dependencies allDeps) = dependencies <> devDependencies
    isInRegistry { packageName } = case PackageName.parse packageName of
      Left _ ->
        unsafeCrashWith $ "Bower file contains a bad package name: " <> packageName
      Right name -> case Map.lookup name packageIndex of
        Nothing -> Just name
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
cleanPackageName raw = case String.split (String.Pattern "/") raw of
  [ p ] -> p
  [ _owner, repo ] -> repo -- e.g. in abc-parser@1.1.2
  _ -> unsafeCrashWith $ "Couldn't parse package name " <> show raw

throw :: forall m a. MonadThrow Aff.Error m => String -> m a
throw = Aff.throwError <<< Aff.error

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

-- | Find a list of all released tags for each package in the provided registry.
createReleaseIndex :: PackageMap Repo -> Aff (PackageMap PackageReleases)
createReleaseIndex (PackageMap allPackages) =
  withCache "releaseIndex" (Just $ Hours 1.0) do
    (PackageMap <<< Map.fromFoldable) <$> forWithIndex allPackages \package repo -> do
      let name = PackageName.print package
      address <- case repo of
        Git _ -> throw $ "Error: Package " <> name <> " is not on GitHub."
        GitHub address -> pure $ Record.delete (Proxy :: Proxy "subdir") address
      let repoCache = fold [ "releases__", address.owner, "__", address.repo ]
      releases <- withCache repoCache (Just $ Hours 24.0) do
        log $ "Fetching releases for package " <> name
        Set.fromFoldable <$> GitHub.getReleases address
      pure $ Tuple package { releases, address }

-- | Get the list of packages in the Bower and PureScript registries.
readRegistryPackages :: Aff (PackageMap Repo)
readRegistryPackages = do
  bowerRegistry <- readRegistry BowerRegistry
  purescriptRegistry <- readRegistry PureScriptRegistry
  pure $ over2 PackageMap Map.union bowerRegistry purescriptRegistry

data RegistrySource = BowerRegistry | PureScriptRegistry

-- | Get the list of packages from the given registry.
-- | Assumption: we are running in the `ci` folder of the registry repo
readRegistry :: RegistrySource -> Aff (PackageMap Repo)
readRegistry source = do
  let
    sourceFile :: FilePath
    sourceFile = case source of
      BowerRegistry -> "bower-packages.json"
      PureScriptRegistry -> "new-packages.json"

    decodeRegistry :: String -> Either JsonDecodeError (PackageMap Repo)
    decodeRegistry = decodeJson <=< parseJson

  registryFile <- FS.readTextFile UTF8 ("../" <> sourceFile)
  case decodeRegistry registryFile of
    Left err -> throw $ fold
      [ "Error: Decoding "
      , sourceFile
      , "failed with error:\n\n"
      , printJsonDecodeError err
      ]
    Right packages ->
      pure packages
