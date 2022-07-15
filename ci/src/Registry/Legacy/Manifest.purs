module Registry.Legacy.Manifest where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Either as Either
import Data.Map as Map
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.These (These(..))
import Data.These as These
import Foreign.Dhall as Dhall
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Licensee as Licensee
import Foreign.SPDX (License)
import Foreign.SPDX as SPDX
import Foreign.Tmp as Tmp
import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.Json ((.:), (.:?))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM)
import Registry.Schema (Location, Manifest(..))
import Registry.Version (Range, Version)
import Registry.Version as Version
import Text.Parsing.StringParser as Parser

type LegacyManifest =
  { license :: License
  , description :: Maybe String
  , dependencies :: Map PackageName Range
  }

toManifest :: PackageName -> Version -> Location -> LegacyManifest -> Manifest
toManifest name version location { license, description, dependencies } =
  Manifest { name, version, location, license, description, dependencies, files: Nothing, owners: Nothing }

-- TODO: If both dependency maps parse, then find shared dependencies and
--       check if one set uses higher ranges than the other (ie. one uses
--       prelude >=4 and the other uses prelude >=5). If so, prefer it. If not,
--       prefer Bower. Compare the ranges on their `rhs`?
-- TODO: Go put this in place of the existing 'fetch >>= parse' pipeline.

-- TODO: (Separately: add the `supervise` guard on top of calls to the uploader.)

fetchLegacyManifest :: GitHub.Address -> RawVersion -> ExceptT LegacyManifestValidationError RegistryM LegacyManifest
fetchLegacyManifest address ref = do
  manifests <- fetchLegacyManifestFiles address ref

  dependencies <- do
    let
      -- Package sets will produce exact versions for dependencies, ie. a
      -- dependency on "strings: 4.1.2" will produce the version range
      -- "strings: >=4.1.2 <4.1.3", which while technically correct is almost
      -- certainly not what the user wants. We instead treat these ranges as
      -- caret ranges, so "strings: 4.1.2" becomes "strings: >=4.1.2 <5.0.0".
      convertSpagoDeps packages = Array.foldl foldFn Map.empty
        where
        foldFn deps name = maybe deps (\{ version } -> Map.insert name (toRange version) deps) (findPackage name)
        findPackage name = Map.lookup name packages
        toRange (RawVersion fixed) = do
          let parsedVersion = Version.parseVersion Version.Lenient fixed
          let bump version = Version.printVersion (Version.bumpHighest version)
          let printRange version = Array.fold [ ">=", fixed, " <", bump version ]
          RawVersionRange $ Either.either (const fixed) printRange parsedVersion

      validateSpagoDeps (SpagoDhallJson { dependencies, packages }) =
        validateDependencies (convertSpagoDeps packages dependencies)

      -- We remove dependencies that don't begin with 'purescript-', as these
      -- indicate JavaScript dependencies.
      convertBowerDeps = Map.mapMaybeWithKey \(RawPackageName name) range -> do
        _ <- String.stripPrefix (String.Pattern "purescript-") name
        pure range

      validateBowerDeps (Bowerfile { dependencies }) =
        validateDependencies (convertBowerDeps dependencies)

    Except.except case manifests of
      This bower -> validateBowerDeps bower
      That spago -> validateSpagoDeps spago
      Both bower spago -> case validateBowerDeps bower of
        Left bowerError -> case validateSpagoDeps spago of
          Left spagoError -> Left $ case bowerError.error, spagoError.error of
            InvalidDependencies a, InvalidDependencies b ->
              { error: InvalidDependencies (a <> b)
              , reason: "Bowerfile and Spago file both report invalid dependencies"
              }
            _, _ -> bowerError
          Right valid -> pure valid
        Right valid -> pure valid

  license <- do
    let unBower (Bowerfile { license }) = Array.mapMaybe NonEmptyString.fromString license
    let unSpago (SpagoDhallJson { license }) = Array.catMaybes [ license ]
    let manifestLicenses = These.these unBower unSpago (\bower spago -> unBower bower <> unSpago spago) manifests
    detectedLicenses <- lift $ detectLicenses address ref
    let licenses = Array.nub $ Array.concat [ detectedLicenses, manifestLicenses ]
    Except.except $ validateLicense licenses

  let
    description = do
      Bowerfile fields <- These.theseLeft manifests
      fields.description

  pure { license, dependencies, description }

type LegacyManifestValidationError = { error :: LegacyManifestError, reason :: String }

data LegacyManifestError
  = NoManifests
  | MissingLicense
  | InvalidLicense (Array String)
  | InvalidDependencies (Array { name :: String, range :: String, error :: String })

instance RegistryJson LegacyManifestError where
  encode = case _ of
    NoManifests -> Json.encode { tag: "NoManifests" }
    MissingLicense -> Json.encode { tag: "MissingLicense" }
    InvalidLicense arr -> Json.encode { tag: "InvalidLicense", value: arr }
    InvalidDependencies arr -> Json.encode { tag: "InvalidDependencies", value: arr }
  decode = Json.decode >=> \obj -> (obj .: "tag") >>= case _ of
    "NoManifests" -> pure NoManifests
    "MissingLicense" -> pure MissingLicense
    "InvalidLicense" -> map InvalidLicense $ obj .: "value"
    "InvalidDependencies" -> map InvalidDependencies $ obj .: "value"
    tag -> Left $ "Unexpected Tag: " <> tag

printLegacyManifestError :: LegacyManifestError -> String
printLegacyManifestError = case _ of
  NoManifests -> "NoManifests"
  MissingLicense -> "MissingLicense"
  InvalidLicense licenses -> "InvalidLicense (" <> String.joinWith ", " licenses <> ")"
  InvalidDependencies errors -> "InvalidDependencies (" <> String.joinWith ", " (map printDepError errors) <> ")"
  where
  printDepError { name, range, error } = "[{ " <> name <> ": " <> range <> "}, " <> error <> "]"

fetchLegacyManifestFiles :: GitHub.Address -> RawVersion -> ExceptT LegacyManifestValidationError RegistryM (These Bowerfile SpagoDhallJson)
fetchLegacyManifestFiles address ref = ExceptT do
  eitherBower <- Except.runExceptT $ fetchBowerfile address ref
  eitherSpago <- Except.runExceptT $ fetchSpagoDhallJson address ref
  pure $ case eitherBower, eitherSpago of
    Left _, Left _ -> Left { error: NoManifests, reason: "No bower.json or spago.dhall files available." }
    Right bower, Left _ -> Right $ This bower
    Left _, Right spago -> Right $ That spago
    Right bower, Right spago -> Right $ Both bower spago

detectLicenses :: GitHub.Address -> RawVersion -> RegistryM (Array NonEmptyString)
detectLicenses address ref = do
  { octokit, cache } <- ask
  let getFile = liftAff <<< Except.runExceptT <<< GitHub.getContent octokit cache address (un RawVersion ref)
  packageJsonFile <- getFile "package.json"
  licenseFile <- getFile "LICENSE"
  let packageJsonInput = { name: "package.json", contents: _ } <$> hush packageJsonFile
  let licenseInput = { name: "LICENSE", contents: _ } <$> hush licenseFile
  liftAff $ Licensee.detectFiles (Array.catMaybes [ packageJsonInput, licenseInput ]) >>= case _ of
    Left err -> log ("Licensee decoding error, ignoring: " <> err) $> []
    Right licenses -> pure $ Array.mapMaybe NonEmptyString.fromString licenses

validateLicense :: Array NonEmptyString -> Either LegacyManifestValidationError License
validateLicense licenses = do
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

    parsedLicenses =
      map (SPDX.parse <<< rewrite)
        $ Array.filter (_ /= "LICENSE")
        $ map NonEmptyString.toString licenses

  case partitionEithers parsedLicenses of
    { fail: [], success: [] } -> Left { error: MissingLicense, reason: "No licenses found." }
    { fail: [], success } -> Right $ SPDX.joinWith SPDX.And success
    { fail } -> Left { error: InvalidLicense fail, reason: "Licenses found, but not valid SPDX identifiers." }

validateDependencies :: Map RawPackageName RawVersionRange -> Either LegacyManifestValidationError (Map PackageName Range)
validateDependencies dependencies = do
  let
    parsePackageName = lmap Parser.printParserError <<< PackageName.parse <<< stripPureScriptPrefix
    parseVersionRange = lmap Parser.printParserError <<< Version.parseRange Version.Lenient

    foldFn (RawPackageName name) acc (RawVersionRange range) = do
      let failWith = { name, range, error: _ }
      case parsePackageName name, parseVersionRange range of
        Left nameErr, Left rangeErr ->
          acc { no = Array.cons (failWith (nameErr <> rangeErr)) acc.no }
        Left nameErr, Right _ ->
          acc { no = Array.cons (failWith nameErr) acc.no }
        Right _, Left rangeErr ->
          acc { no = Array.cons (failWith rangeErr) acc.no }
        Right parsedName, Right parsedRange ->
          acc { yes = Array.cons (Tuple parsedName parsedRange) acc.yes }

    parsedDependencies =
      foldlWithIndex foldFn { no: [], yes: [] } dependencies

  case parsedDependencies of
    { no: [], yes } -> pure $ Map.fromFoldable yes
    { no } -> Left { error: InvalidDependencies no, reason: "Version specifies invalid dependencies." }

-- | The result of calling 'dhall-to-json' on a 'spago.dhall' file and
-- | accompanying 'packages.dhall' file.
newtype SpagoDhallJson = SpagoDhallJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName { version :: RawVersion }
  }

instance RegistryJson SpagoDhallJson where
  encode (SpagoDhallJson fields) = Json.encode fields
  decode json = do
    obj <- Json.decode json
    license <- obj .:? "license"
    dependencies <- fromMaybe [] <$> obj .:? "dependencies"
    packages <- fromMaybe Map.empty <$> obj .:? "packages"
    pure $ SpagoDhallJson { license, dependencies, packages }

-- | Attempt to construct a SpagoDhallJson file from a spago.dhall and
-- | packages.dhall file located in a remote repository at the given ref.
fetchSpagoDhallJson :: GitHub.Address -> RawVersion -> ExceptT GitHub.GitHubError RegistryM SpagoDhallJson
fetchSpagoDhallJson address (RawVersion ref) = do
  { octokit, cache } <- ask
  let getFile = Except.mapExceptT liftAff <<< GitHub.getContent octokit cache address ref
  spagoDhall <- getFile "spago.dhall"
  packagesDhall <- getFile "packages.dhall"
  tmp <- liftEffect Tmp.mkTmpDir
  liftAff $ FSA.writeTextFile UTF8 (Path.concat [ tmp, "packages.dhall" ]) packagesDhall
  dhallJson <- liftAff $ Dhall.dhallToJson { dhall: spagoDhall, cwd: Just tmp }
  Except.except $ case dhallJson of
    Left err -> Left $ GitHub.DecodeError err
    Right json -> case Json.decode json of
      Left err -> Left $ GitHub.DecodeError err
      Right value -> pure value

newtype Bowerfile = Bowerfile
  { description :: Maybe String
  , dependencies :: Map RawPackageName RawVersionRange
  , license :: Array String
  }

derive newtype instance Eq Bowerfile
derive newtype instance Show Bowerfile

instance RegistryJson Bowerfile where
  encode (Bowerfile fields) = Json.encode fields
  decode json = do
    obj <- Json.decode json
    description <- obj .:? "description"
    dependencies <- fromMaybe Map.empty <$> obj .:? "dependencies"
    licenseField <- obj .:? "license"
    license <- case licenseField of
      Nothing -> pure []
      Just jsonValue -> (Json.decode jsonValue <#> Array.singleton) <|> Json.decode jsonValue
    pure $ Bowerfile { description, dependencies, license }

-- | Attempt to construct a Bowerfile from a bower.json file rlocated in a
-- | remote repository at the given ref.
fetchBowerfile :: GitHub.Address -> RawVersion -> ExceptT GitHub.GitHubError RegistryM Bowerfile
fetchBowerfile address (RawVersion ref) = do
  { octokit, cache } <- ask
  bowerfile <- Except.mapExceptT liftAff $ GitHub.getContent octokit cache address ref "bower.json"
  Except.except $ case Json.parseJson (JsonRepair.tryRepair bowerfile) of
    Left err -> Left $ GitHub.DecodeError err
    Right value -> pure value
