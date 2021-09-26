module Registry.Scripts.LegacyImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except as Except
import Data.Argonaut as Json
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Interpolate (i)
import Data.Lens (_Left, preview)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Foreign.Dhall as Dhall
import Foreign.GitHub as GitHub
import Foreign.Licensee as Licensee
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Registry.Index (RegistryIndex, insertManifest)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Scripts.LegacyImport.Bowerfile (Bowerfile(..))
import Registry.Scripts.LegacyImport.Error (FileResource(..), ImportError(..), ManifestError(..), PackageFailures(..), RawPackageName(..), RawVersion(..), RemoteResource(..), RequestError(..), fileResourcePath)
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Scripts.LegacyImport.SpagoJson (SpagoJson)
import Registry.Scripts.LegacyImport.SpagoJson as SpagoJson
import Registry.Scripts.LegacyImport.Stats as Stats
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
  registry <- downloadBowerRegistry

  let indexPath = "registry-index"
  log $ "Writing registry index to " <> indexPath

  exists <- FS.exists indexPath
  guard (not exists) $ FS.mkdir indexPath
  for_ registry \semVer -> for_ semVer (insertManifest indexPath)
  log "Done!"

type ImportM e a = ExceptT e Aff a

downloadBowerRegistry :: Aff RegistryIndex
downloadBowerRegistry = do
  octokit <- liftEffect GitHub.mkOctokit
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

          Process.filterFailedPackages shouldAccept excludedPackages allPackages
      }

  log "Fetching package releases..."
  releaseIndex <- Process.forPackage initialPackages identity \name repoUrl -> do
    address <- case GitHub.parseRepo repoUrl of
      Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
      Right address -> pure address

    let repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]

    releases <- Process.withCache Process.jsonSerializer repoCache (Just $ Hours 100.0) do
      log $ "Fetching releases for package " <> un RawPackageName name
      result <- lift $ try $ GitHub.getReleases octokit address
      case result of
        Left err -> logShow err *> throwError NoReleases
        Right v -> pure v

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

          Process.filterFailedPackageVersions shouldAccept releaseIndex.failures _.name releaseIndex.packages
      }

  log "Fetching package bowerfiles..."
  bowerRegistry <- Process.forPackageVersion validBower _.name identity \{ name, address } tag _ -> do
    bowerfile <- fetchBowerfile name address tag
    let packagesWithReleases = Set.map _.name $ Map.keys releaseIndex.packages
    selfContainedDependencies packagesWithReleases bowerfile
    pure bowerfile

  log "Parsing names and versions..."
  packageRegistry <- Process.forPackageVersionKeys bowerRegistry _.name identity \{ name, address } tag -> do
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
  let forPackageRegistry = Process.forPackageVersion packageRegistry _.original _.original
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
  Stats.logStats $ Stats.errorStats manifestRegistry
  pure registryIndex

-- | Find the bower.json files associated with the package's released tags,
-- | caching the file to avoid re-fetching each time the tool runs.
fetchBowerfile :: RawPackageName -> GitHub.Address -> RawVersion -> ImportM ImportError Bowerfile
fetchBowerfile _ _ _ = throwError NoReleases

-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
selfContainedDependencies :: Set RawPackageName -> Bowerfile -> ImportM ImportError Unit
selfContainedDependencies registry (Bowerfile { dependencies, devDependencies }) = do
  let allDeps = Object.keys $ dependencies <> devDependencies
  outsideDeps <- for allDeps \packageName -> do
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
  -> Bowerfile
  -> ImportM (NonEmptyArray ManifestError) Manifest
toManifest package repository version (Bowerfile bowerfile) = do
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

      case bowerfile.license of
        Nothing -> mkError MissingLicense
        Just licenses -> do
          let
            parsed = map SPDX.parse $ rewrite $ NEA.toArray licenses
            { fail, success } = partitionEithers parsed

          case fail, success of
            [], [] -> mkError MissingLicense
            [], _ -> Right $ SPDX.joinWith SPDX.Or success
            _, _ -> mkError $ BadLicense fail

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

      normalizedDeps <- normalizeDeps $ Object.toUnfoldable bowerfile.dependencies
      normalizedDevDeps <- normalizeDeps $ Object.toUnfoldable bowerfile.devDependencies

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
cleanPackageName :: RawPackageName -> ImportM ImportError RawPackageName
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

type RawManifest =
  { name :: RawPackageName
  , version :: RawVersion
  , repository :: GitHub.Address
  , license :: Array String
  , dependencies :: Object String
  , devDependencies :: Object String
  }

-- | Attempt to construct the basic fields necessary for a manifest file by reading
-- | the package version's bower.json, spago.dhall, package.json, and LICENSE
-- | files, if present.
constructRawManifest
  :: RawPackageName
  -> RawVersion
  -> GitHub.Address
  -> ExceptT ImportError Aff RawManifest
constructRawManifest package version address = do
  -- We can construct a manifest from a package's bowerfile, package.json file,
  -- spago.dhall file, and/or LICENSE files. A package doesn't need to have all
  -- of these files; several of these files duplicate information. We try to
  -- fetch all files but won't throw an exception (yet) if they're missing.
  files <- liftAff do
    licenseFile <- Except.runExceptT $ fileRequest LicenseFile Process.stringSerializer

    bowerJson <- do
      let (serialize :: Process.Serialize _ Bowerfile) = Process.jsonSerializer
      Except.runExceptT $ fileRequest BowerJson serialize

    packageJson <- do
      let (serialize :: Process.Serialize _ (Object String)) = Process.jsonSerializer
      Except.runExceptT $ fileRequest PackageJson serialize

    spagoJson <- Except.runExceptT requestSpagoJson

    pure { licenseFile, bowerJson, packageJson, spagoJson }

  -- We can detect the license for the project using a combination of Licensee
  -- and reading the license directly out of the Spago file.
  license <- detectLicense files

  -- We can pull dependencies from the bower.json or spago.dhall files. If both
  -- files are present, but their dependencies differ, then we use the file with
  -- newer dependencies; presumably, it's the more up-to-date file.
  --
  -- Since Bower users typically use ranges, but package sets use precise versions,
  -- we check to see whether one uses later major versions than the other does.
  --
  -- If the files differ but it isn't clear which file is newer, then we prefer
  -- the Bower file since it's the legacy format used for package publishing.
  let
    dependencies = Object.empty
    devDependencies = Object.empty

  pure
    { name: package
    , version
    , repository: address
    , license
    , dependencies
    , devDependencies
    }
  where
  detectLicense :: _ -> ExceptT ImportError Aff (Array String)
  detectLicense { licenseFile, bowerJson, packageJson, spagoJson } = do
    licenseeResult <- liftAff $ Licensee.detectFiles $ Array.catMaybes $ map hush
      [ bowerJson <#> Json.encodeJson >>> { name: "bower.json", contents: _ }
      , packageJson <#> Json.encodeJson >>> { name: "package.json", contents: _ }
      , licenseFile <#> Json.fromString >>> { name: "LICENSE", contents: _ }
      ]

    detectedLicenses <- case licenseeResult of
      Left err -> do
        log $ "Licensee decoding error, ignoring: " <> err
        pure []
      Right { licenses: licenseArray } ->
        pure $ map _.spdx_id licenseArray

    spagoLicense <- case SpagoJson.license =<< hush spagoJson of
      Nothing -> pure []
      Just license -> pure [ license ]

    pure $ Array.concat [ detectedLicenses, spagoLicense ]

  -- Attempt to construct a Spago JSON file by fetching the spago.dhall and
  -- packages.dhall files and converting them to JSON with dhall-to-json.
  requestSpagoJson :: ExceptT ImportError Aff SpagoJson
  requestSpagoJson = do
    spagoDhall <- fileRequest SpagoDhall Process.stringSerializer
    packagesDhall <- fileRequest PackagesDhall Process.stringSerializer

    tmp <- liftEffect Tmp.mkTmpDir
    liftAff $ FS.writeTextFile UTF8 (tmp <> "/packages.dhall") packagesDhall

    spagoJson <- do
      let
        mkError = ResourceError <<< { resource: FileResource SpagoDhall, error: _ } <<< DecodeError
        runDhallJson = Dhall.dhallToJson { dhall: spagoDhall, cwd: Just tmp }

      Except.mapExceptT (map (lmap mkError))
        $ Except.ExceptT
        $ map (_ >>= (Json.decodeJson >>> lmap Json.printJsonDecodeError)) runDhallJson

    liftAff $ FS.rmdir tmp
    pure spagoJson

  -- Request a file from the remote repository associated with the package
  -- version. Files will be cached using the provided serializer and
  -- will be read from the cache up to the cache expiry time given in `Hours`.
  fileRequest :: forall a. FileResource -> Process.Serialize String a -> ExceptT ImportError Aff a
  fileRequest resource serialize = do
    let
      name = un RawPackageName package
      tag = un RawVersion version
      filePath = fileResourcePath resource
      url = i "https://raw.githubusercontent.com/" address.owner "/" address.repo "/" tag "/" filePath
      fileCacheName = String.replace (String.Pattern ".") (String.Replacement "_") filePath
      cacheKey = i fileCacheName "__" name "__" tag
      mkError = ResourceError <<< { resource: FileResource resource, error: _ }

    Process.withCache serialize cacheKey Nothing do
      liftAff (Http.get ResponseFormat.string url) >>= case _ of
        Left error -> do
          let printed = Http.printError error
          log $ i "Unable to retrieve " filePath " because the request failed: " printed
          throwError $ mkError BadRequest
        Right { status: StatusCode status, body }
          | status == 404 -> do
              log $ i "Unable to retrieve " filePath " because none exists (404 error)."
              throwError $ mkError $ BadStatus status
          | status /= 200 -> do
              log $ i "Unable to retrieve " filePath " because of a bad status code: " body
              throwError $ mkError $ BadStatus status
          | otherwise -> case serialize.decode body of
              Left failure -> do
                log $ i "Failed to decode " filePath ":"
                log failure
                log $ i "  arising from the body of the request:"
                log body
                throwError $ mkError $ DecodeError failure
              Right result ->
                pure result
