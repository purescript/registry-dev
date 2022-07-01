module Registry.Scripts.LegacyImport.Manifest
  ( toManifest
  , constructManifestFields
  ) where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Lens as Lens
import Data.Map as Map
import Data.String.NonEmpty as NES
import Effect.Ref as Ref
import Foreign.Dhall as Dhall
import Foreign.GitHub (GitHubCache, Octokit)
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Licensee as Licensee
import Foreign.SPDX as SPDX
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Registry.Json as Json
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Location)
import Registry.Scripts.LegacyImport.Bowerfile as Bowerfile
import Registry.Scripts.LegacyImport.Error (FileResource(..), ImportError(..), ManifestError(..), RemoteResource(..), RequestError(..), fileResourcePath)
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)
import Registry.Scripts.LegacyImport.SpagoJson (SpagoJson)
import Registry.Scripts.LegacyImport.SpagoJson as SpagoJson
import Registry.Types (RawPackageName(..), RawVersion(..))
import Registry.Version (ParseMode(..), Version)
import Registry.Version as Version
import Safe.Coerce (coerce)

-- | Attempt to construct the basic fields necessary for a manifest file by reading
-- | the package version's bower.json, spago.dhall, package.json, and LICENSE
-- | files, if present.
constructManifestFields
  :: Octokit
  -> Ref GitHubCache
  -> RawVersion
  -> GitHub.Address
  -> ExceptT ImportError Aff ManifestFields
constructManifestFields octokit cacheRef (RawVersion version) address = do
  let
    getFile :: forall m. MonadAff m => FileResource -> m (Either ImportError String)
    getFile resource = liftAff $ Except.runExceptT do
      let throwResourceError = throwError <<< ResourceError <<< { resource: FileResource resource, error: _ }
      result <- liftAff $ Except.runExceptT do
        GitHub.getContent octokit cacheRef address version (fileResourcePath resource)
      case result of
        Left (GitHub.DecodeError err) ->
          throwResourceError (DecodeError err)
        Left (GitHub.APIError githubError)
          | githubError.statusCode >= 400 -> throwResourceError (BadStatus githubError.statusCode)
          | otherwise -> throwResourceError BadRequest
        Right contents ->
          pure contents

  -- We can construct a manifest from a package's bowerfile, package.json file,
  -- spago.dhall file, and/or LICENSE files. A package doesn't need to have all
  -- of these files; several of these files duplicate information. We try to
  -- fetch all files but won't throw an exception (yet) if they're missing.
  bowerJson <- getFile BowerJson
  spagoDhall <- getFile SpagoDhall
  packagesDhall <- getFile PackagesDhall
  packageJson <- getFile PackageJson
  licenseFile <- getFile LicenseFile
  liftAff $ GitHub.writeGitHubCache =<< liftEffect (Ref.read cacheRef)

  bowerManifest <- Except.runExceptT do
    result <- Except.except bowerJson
    case RegistryJson.parseJson $ JsonRepair.tryRepair result of
      Left error ->
        throwError $ ResourceError { resource: FileResource BowerJson, error: DecodeError error }
      Right bowerfile ->
        pure $ Bowerfile.toManifestFields bowerfile

  spagoManifest <- liftAff $ Except.runExceptT do
    spago <- Except.except spagoDhall
    packages <- Except.except packagesDhall
    spagoJson <- buildSpagoJson { spagoDhall: spago, packagesDhall: packages }
    pure $ SpagoJson.toManifestFields spagoJson

  dependencies <- case bowerManifest, spagoManifest of
    Left _, Left _ -> throwError NoDependencyFiles
    Left _, Right { dependencies } -> pure dependencies
    Right { dependencies }, _ -> pure dependencies

  -- We can detect the license for the project using a combination of `licensee`
  -- and reading the license directly out of the Spago and Bower files (the
  -- CLI tool will not read from either file).
  licenseeOutput <- detectLicense { licenseFile: hush licenseFile, packageJson: hush packageJson }

  let
    spagoLicenses = maybe [] NEA.toArray $ _.license =<< hush spagoManifest
    bowerLicenses = maybe [] NEA.toArray $ _.license =<< hush bowerManifest
    licenseeLicenses = Array.mapMaybe NES.fromString licenseeOutput
    license = NEA.fromArray $ Array.nub $ Array.concat [ licenseeLicenses, spagoLicenses, bowerLicenses ]
    description = join (_.description <$> hush bowerManifest)

  pure { license, dependencies, description }
  where
  -- Attempt to detect the package license from the package.json and LICENSE
  -- files (the only files that Licensee can detect from).
  detectLicense :: { licenseFile :: Maybe String, packageJson :: Maybe String } -> ExceptT ImportError Aff (Array String)
  detectLicense files = do
    licenseeResult <- liftAff $ Licensee.detectFiles $ Array.catMaybes
      [ files.packageJson <#> { name: "package.json", contents: _ }
      , files.licenseFile <#> { name: "LICENSE", contents: _ }
      ]

    case licenseeResult of
      Left err -> log ("Licensee decoding error, ignoring: " <> err) $> []
      Right licenses -> pure licenses

  -- Attempt to construct a Spago JSON file by fetching the spago.dhall and
  -- packages.dhall files and converting them to JSON with dhall-to-json.
  buildSpagoJson :: { spagoDhall :: String, packagesDhall :: String } -> ExceptT ImportError Aff SpagoJson
  buildSpagoJson files = do
    tmp <- liftEffect Tmp.mkTmpDir
    liftAff $ FS.writeTextFile UTF8 (tmp <> "/packages.dhall") files.packagesDhall

    let
      mkError = ResourceError <<< { resource: FileResource SpagoDhall, error: _ } <<< DecodeError
      runDhallJson = Dhall.dhallToJson { dhall: files.spagoDhall, cwd: Just tmp }

    Except.mapExceptT (map (lmap mkError)) $ Except.ExceptT $ map (_ >>= Json.decode) runDhallJson

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest :: PackageName -> Location -> Version -> ManifestFields -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package location version manifest = do
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

    eitherDependencies = do
      let
        parsePairs = map \(Tuple (RawPackageName packageName) versionRange) -> case PackageName.parse packageName of
          Left _ -> Left packageName
          Right name -> Right (Tuple name (coerce versionRange))

        normalizeDeps deps = do
          let { fail, success } = partitionEithers $ parsePairs deps
          case NEA.fromArray fail of
            Nothing -> pure success
            Just err -> mkError $ InvalidDependencyNames err

      normalizedDeps <- normalizeDeps $ Map.toUnfoldable manifest.dependencies

      let
        checkDepPair (Tuple packageName versionStr) = case Version.parseRange Lenient versionStr of
          Left _ -> do
            Left { dependency: packageName, failedBounds: versionStr }
          Right range ->
            Right $ Tuple packageName range

        readDeps = map checkDepPair >>> partitionEithers >>> \{ fail, success } ->
          case NEA.fromArray fail of
            Nothing ->
              Right success
            Just err ->
              mkError $ BadDependencyVersions err

      readDeps normalizedDeps <#> Map.fromFoldable

    errs = do
      let
        toMaybeErrors :: forall e a. Either e a -> Maybe e
        toMaybeErrors = Lens.preview Lens._Left

      map NEA.concat $ NEA.fromArray $ Array.catMaybes
        [ toMaybeErrors eitherLicense
        , toMaybeErrors eitherDependencies
        ]

  case errs of
    Nothing -> do
      let description = manifest.description

      -- Technically this shouldn't be needed, since we've already checked these
      -- for errors, but this is just so the types all work out.
      license <- Except.except eitherLicense
      dependencies <- Except.except eitherDependencies
      pure $ Manifest { name: package, license, location, description, dependencies, version, owners: Nothing, files: Nothing }

    Just err ->
      throwError err
