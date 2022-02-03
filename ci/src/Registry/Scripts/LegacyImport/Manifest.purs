module Registry.Scripts.LegacyImport.Manifest (toManifest, constructManifestFields) where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except as Except
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Interpolate (i)
import Data.Lens as Lens
import Data.Map as Map
import Data.String as String
import Data.String.NonEmpty as NES
import Foreign.Dhall as Dhall
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Licensee as Licensee
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Registry.Json as Json
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Repo, Target(..))
import Registry.Scripts.LegacyImport.Bowerfile as Bowerfile
import Registry.Scripts.LegacyImport.Error (FileResource(..), ImportError(..), ManifestError(..), RemoteResource(..), RequestError(..), fileResourcePath)
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Scripts.LegacyImport.SpagoJson (SpagoJson)
import Registry.Scripts.LegacyImport.SpagoJson as SpagoJson
import Registry.Types (RawPackageName(..), RawVersion(..))
import Registry.Version (ParseMode(..), Version)
import Registry.Version as Version

-- | Attempt to construct the basic fields necessary for a manifest file by reading
-- | the package version's bower.json, spago.dhall, package.json, and LICENSE
-- | files, if present.
constructManifestFields
  :: RawPackageName
  -> RawVersion
  -> GitHub.Address
  -> ExceptT ImportError Aff ManifestFields
constructManifestFields package version address = do
  let cacheKey = i "manifest-fields__" (un RawPackageName package) "__" (un RawVersion version)
  Process.withCache Process.jsonSerializer cacheKey Nothing do
    -- We can construct a manifest from a package's bowerfile, package.json file,
    -- spago.dhall file, and/or LICENSE files. A package doesn't need to have all
    -- of these files; several of these files duplicate information. We try to
    -- fetch all files but won't throw an exception (yet) if they're missing.
    log $ "Constructing manifest fields for " <> un RawPackageName package <> " " <> un RawVersion version
    let mkRequest file = parallel $ Except.runExceptT $ fileRequest file Process.stringSerializer
    files <- liftAff $ sequential ado
      licenseFile <- mkRequest LicenseFile
      bowerJson <- mkRequest BowerJson
      packageJson <- mkRequest PackageJson
      in { licenseFile, bowerJson, packageJson }

    -- TODO: Improve this heuristic by checking the Bower _and_ Spago files.
    --
    -- We can pull dependencies from the bower.json or spago.dhall files. If both
    -- files are present, but their dependencies differ, then we should use the
    -- file with newer dependencies; presumably, it's the more up-to-date file.
    --
    -- Since Bower users typically use ranges, but package sets use precise
    -- versions, we could check to see whether one uses later major versions
    -- than the other does; checking minor or patch versions will be inaccurate.
    --
    -- If the files differ but it isn't clear which file is newer, then we should
    -- prefer the Bower file since it's the legacy format used for package p
    -- publishing.
    --
    -- For now, that's exactly what we do: use the Bower file if it is present,
    -- and otherwise fall back to the Spago file.
    bowerManifest <- Except.runExceptT do
      result <- Except.except files.bowerJson
      case RegistryJson.parseJson $ JsonRepair.tryRepair result of
        Left error -> do
          log $ "Could not decode returned bower.json: " <> error
          log result
          throwError $ ResourceError { resource: FileResource BowerJson, error: DecodeError error }
        Right bowerfile ->
          pure $ Bowerfile.toManifestFields bowerfile

    spagoJson <- liftAff $ Except.runExceptT requestSpagoJson
    let spagoManifest = map SpagoJson.toManifestFields spagoJson

    { dependencies, devDependencies } <- case bowerManifest, spagoManifest of
      Left _, Left _ -> do
        -- TODO: We may want to report a `NonEmptyArray ImportError` so as to
        -- report on multiple errors, such as the multiple missing files in this
        -- situation.
        throwError NoDependencyFiles
      Left _, Right { dependencies, devDependencies } ->
        pure { dependencies, devDependencies }
      Right { dependencies, devDependencies }, _ ->
        pure { dependencies, devDependencies }

    -- We can detect the license for the project using a combination of `licensee`
    -- and reading the license directly out of the Spago and Bower files (the
    -- CLI tool will not read from either file).
    licenseeOutput <- detectLicense files

    let
      spagoLicenses = maybe [] NEA.toArray $ _.license =<< hush spagoManifest
      bowerLicenses = maybe [] NEA.toArray $ _.license =<< hush bowerManifest
      licenseeLicenses = Array.mapMaybe NES.fromString licenseeOutput
      license = NEA.fromArray $ Array.nub $ Array.concat [ licenseeLicenses, spagoLicenses, bowerLicenses ]
      description = join (_.description <$> hush bowerManifest)

    when (license == Nothing) do
      log $ "No license available for " <> un RawPackageName package <> " " <> un RawVersion version

    pure { license, dependencies, devDependencies, description }
  where
  detectLicense { licenseFile, packageJson } = do
    licenseeResult <- liftAff $ Licensee.detectFiles $ Array.catMaybes $ map hush
      -- Detection only works on these files, and won't work on Spago files,
      -- Bower files, or the JSON produced by the dhall-to-json result of
      -- converting the Spago file.
      [ packageJson <#> { name: "package.json", contents: _ }
      , licenseFile <#> { name: "LICENSE", contents: _ }
      ]

    detectedLicenses <- case licenseeResult of
      Left err -> do
        log $ "Licensee decoding error, ignoring: " <> err
        pure []
      Right licenses ->
        pure licenses

    pure detectedLicenses

  -- Attempt to construct a Spago JSON file by fetching the spago.dhall and
  -- packages.dhall files and converting them to JSON with dhall-to-json.
  requestSpagoJson :: ExceptT ImportError Aff SpagoJson
  requestSpagoJson = do
    files <- sequential ado
      spagoDhall <- parallel $ fileRequest SpagoDhall Process.stringSerializer
      packagesDhall <- parallel $ fileRequest PackagesDhall Process.stringSerializer
      in { spagoDhall, packagesDhall }

    tmp <- liftEffect Tmp.mkTmpDir
    liftAff $ FS.writeTextFile UTF8 (tmp <> "/packages.dhall") files.packagesDhall

    spagoJson <- do
      let
        mkError = ResourceError <<< { resource: FileResource SpagoDhall, error: _ } <<< DecodeError
        runDhallJson = Dhall.dhallToJson { dhall: files.spagoDhall, cwd: Just tmp }

      Except.mapExceptT (map (lmap mkError))
        $ Except.ExceptT
        $ map (_ >>= Json.decode) runDhallJson

    pure spagoJson

  -- Request a file from the remote repository associated with the package
  -- version. Files will be cached using the provided serializer and
  -- will be read from the cache up to the cache expiry time given in `Hours`.
  fileRequest :: FileResource -> Process.Serialize String String -> ExceptT ImportError Aff String
  fileRequest resource serialize = do
    let
      name = un RawPackageName package
      tag = un RawVersion version
      filePath = fileResourcePath resource
      url = i "https://raw.githubusercontent.com/" address.owner "/" address.repo "/" tag "/" filePath
      fileCacheName = String.replace (String.Pattern ".") (String.Replacement "-") filePath
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
          | otherwise ->
              pure body

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest
  :: PackageName
  -> Repo
  -> Version
  -> ManifestFields
  -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package repository version manifest = do
  let
    mkError :: forall a. ManifestError -> Either (NonEmptyArray ManifestError) a
    mkError = Left <<< NEA.singleton

    eitherLicense = do
      let
        rewrite = case _ of
          "Apache 2" -> "Apache-2.0"
          "Apache-2" -> "Apache-2.0"
          "Apache 2.0" -> "Apache-2.0"
          "BSD" -> "BSD-3-Clause"
          "BSD3" -> "BSD-3-Clause"
          "BSD-3" -> "BSD-3-Clause"
          "3-Clause BSD" -> "BSD-3-Clause"
          other -> other

      case manifest.license of
        Nothing -> mkError MissingLicense
        Just licenses -> do
          let
            parsed =
              map (SPDX.parse <<< rewrite)
                $ Array.filter (_ /= "LICENSE")
                $ map NES.toString
                $ NEA.toArray licenses
            { fail, success } = partitionEithers parsed

          case fail, success of
            [], [] -> mkError MissingLicense
            [], _ -> Right $ SPDX.joinWith SPDX.And success
            _, _ -> mkError $ BadLicense fail

    eitherTargets = do
      let
        -- We trim out packages that don't begin with `purescript-`, as these
        -- are JavaScript dependencies being specified in the Bowerfile.
        filterNames = Array.catMaybes <<< map \(Tuple (RawPackageName packageName) (RawVersion versionRange)) ->
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

      normalizedDeps <- normalizeDeps $ Map.toUnfoldable manifest.dependencies
      normalizedDevDeps <- normalizeDeps $ Map.toUnfoldable manifest.devDependencies

      let
        checkDepPair (Tuple packageName versionStr) = case Version.parseRange Lenient versionStr of
          Left _ -> do
            Left { dependency: packageName, failedBounds: versionStr }
          Right range ->
            Right $ Tuple (PackageName.print packageName) range

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
          [ Just $ Tuple "lib" $ Target
              { sources: [ "src/**/*.purs" ]
              , dependencies: Object.fromFoldable deps
              }
          , if (Array.null devDeps) then Nothing
            else Just $ Tuple "test" $ Target
              { sources: [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies: Object.fromFoldable (deps <> devDeps)
              }
          ]

    errs = do
      let
        toMaybeErrors :: forall e a. Either e a -> Maybe e
        toMaybeErrors = Lens.preview Lens._Left

      map NEA.concat $ NEA.fromArray $ Array.catMaybes
        [ toMaybeErrors eitherLicense
        , toMaybeErrors eitherTargets
        ]

  case errs of
    Nothing -> do
      let description = manifest.description

      -- Technically this shouldn't be needed, since we've already checked these
      -- for errors, but this is just so the types all work out.
      license <- Except.except eitherLicense
      targets <- Except.except eitherTargets
      pure $ Manifest { name: package, license, repository, description, targets, version }

    Just err ->
      throwError err
