module Registry.Manifest where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except as Except
import Control.Parallel (parallel, sequential)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Compactable (compact)
import Data.Interpolate (i)
import Data.Maybe (maybe)
import Data.String as String
import Data.String.NonEmpty as NES
import Foreign.Dhall as Dhall
import Foreign.GitHub as GitHub
import Foreign.Jsonic as Jsonic
import Foreign.Licensee as Licensee
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Registry.Process as Process
import Registry.Types (FileResource(..), ImportError(..), ManifestFields, RawPackageName(..), RawVersion(..), RemoteResource(..), RequestError(..), SpagoJson, bowerToManifestFields, fileResourcePath, spagoToManifestFields)

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
      case Jsonic.parseJson result >>= Json.decodeJson of
        Left err -> do
          let printed = Json.printJsonDecodeError err
          log $ "Could not decode returned bower.json. " <> printed
          log result
          throwError $ ResourceError { resource: FileResource BowerJson, error: DecodeError printed }
        Right bowerfile ->
          pure $ bowerToManifestFields bowerfile

    spagoJson <- liftAff $ Except.runExceptT requestSpagoJson
    let spagoManifest = map spagoToManifestFields spagoJson

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
    licenseeOutput <- detectLicense files ------------------ pull this out

    let
      spagoLicenses = maybe [] NEA.toArray $ _.license =<< hush spagoManifest
      bowerLicenses = maybe [] NEA.toArray $ _.license =<< hush bowerManifest
      licenseeLicenses = compact $ map NES.fromString licenseeOutput
      license = NEA.fromArray $ Array.nub $ Array.concat [ licenseeLicenses, spagoLicenses, bowerLicenses ]

    when (license == Nothing) do
      log $ "No license available for " <> un RawPackageName package <> " " <> un RawVersion version

    pure { license, dependencies, devDependencies }
  where
  detectLicense { licenseFile, packageJson } = do
    licenseeResult <- liftAff $ Licensee.detectFiles $ compact $ map hush
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
        $ map (_ >>= (Json.decodeJson >>> lmap Json.printJsonDecodeError)) runDhallJson

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
