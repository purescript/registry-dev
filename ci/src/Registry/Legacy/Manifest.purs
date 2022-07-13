module Registry.Legacy.Manifest where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either as Either
import Data.Map as Map
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.Validation.Semigroup as Validation
import Foreign.Dhall as Dhall
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Licensee as Licensee
import Foreign.SPDX as SPDX
import Foreign.Tmp as Tmp
import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.Cache (Cache)
import Registry.Json ((.:), (.:?))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location, Manifest(..))
import Registry.Version (Version)
import Registry.Version as Version
import Text.Parsing.StringParser as Parser

-- | Parse a legacy manifest into a registry manifest
parseLegacyManifest
  :: PackageName
  -> Location
  -> Version
  -> LegacyManifest
  -> Either (NonEmptyArray LegacyManifestValidationError) Manifest
parseLegacyManifest name location version legacyManifest = Validation.toEither ado
  license <- do
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

    case legacyManifest.license of
      Nothing -> invalid { error: MissingLicense, reason: "No licenses found." }
      Just licenses -> do
        let toArray = map NonEmptyString.toString <<< NonEmptyArray.toArray
        let parsedLicenses = map (SPDX.parse <<< rewrite) $ Array.filter (_ /= "LICENSE") $ toArray licenses
        case partitionEithers parsedLicenses of
          { fail: [], success: [] } -> invalid { error: MissingLicense, reason: "No licenses found." }
          { fail: [], success } -> pure $ SPDX.joinWith SPDX.And success
          { fail } -> invalid { error: InvalidLicense fail, reason: "Licenses found, but not valid SPDX identifiers." }

  dependencies <- do
    let
      parsePackageName = lmap Parser.printParserError <<< PackageName.parse <<< stripPureScriptPrefix
      parseVersionRange = lmap Parser.printParserError <<< Version.parseRange Version.Lenient

      foldFn (RawPackageName key) acc (RawVersionRange val) = do
        let parsedName = parsePackageName key
        let parsedRange = parseVersionRange val
        case parsedName, parsedRange of
          Left nameErr, Left rangeErr -> acc { badName = Array.cons nameErr acc.badName, badRange = Array.cons rangeErr acc.badRange }
          Left nameErr, Right _ -> acc { badName = Array.cons nameErr acc.badName }
          Right _, Left rangeErr -> acc { badRange = Array.cons rangeErr acc.badRange }
          Right name, Right range -> acc { parsed = Array.cons (Tuple name range) acc.parsed }

      parsedDependencies =
        foldlWithIndex foldFn { badName: [], badRange: [], parsed: [] } legacyManifest.dependencies

    case parsedDependencies of
      { badName: [], badRange: [], parsed } -> pure $ Map.fromFoldable parsed
      { badName, badRange } -> ado
        unless (Array.null badName) do
          invalid { error: InvalidDependencyNames badName, reason: "Dependencies have invalid package names." }
        unless (Array.null badRange) do
          invalid { error: InvalidDependencyRanges badRange, reason: "Dependencies have invalid version ranges." }
        in Map.empty

  let description = map NonEmptyString.toString legacyManifest.description

  in Manifest { name, license, location, description, dependencies, version, owners: Nothing, files: Nothing }
  where
  invalid :: forall e a. e -> Validation.V (NonEmptyArray e) a
  invalid = Validation.invalid <<< NonEmptyArray.singleton

type LegacyManifestValidationError = { error :: LegacyManifestError, reason :: String }

data LegacyManifestError
  = MissingLicense
  | InvalidLicense (Array String)
  | InvalidDependencyNames (Array String)
  | InvalidDependencyRanges (Array String)

instance RegistryJson LegacyManifestError where
  encode = case _ of
    MissingLicense -> Json.encode { tag: "MissingLicense" }
    InvalidLicense arr -> Json.encode { tag: "InvalidLicense", value: arr }
    InvalidDependencyNames arr -> Json.encode { tag: "InvalidDependencyNames", value: arr }
    InvalidDependencyRanges arr -> Json.encode { tag: "InvalidDependencyRanges", value: arr }
  decode = Json.decode >=> \obj -> (obj .: "tag") >>= case _ of
    "MissingLicense" -> pure MissingLicense
    "InvalidLicense" -> map InvalidLicense $ obj .: "value"
    "InvalidDependencyNames" -> map InvalidDependencyNames $ obj .: "value"
    "InvalidDependencyRanges" -> map InvalidDependencyRanges $ obj .: "value"
    tag -> Left $ "Unexpected Tag: " <> tag

printLegacyManifestError :: LegacyManifestError -> String
printLegacyManifestError = case _ of
  MissingLicense -> "MissingLicense"
  InvalidLicense licenses -> "InvalidLicense (" <> String.joinWith ", " licenses <> ")"
  InvalidDependencyNames names -> "InvalidDependencyNames (" <> String.joinWith ", " names <> ")"
  InvalidDependencyRanges ranges -> "InvalidDependencyRanges (" <> String.joinWith ", " ranges <> ")"

type LegacyManifest =
  { license :: Maybe (NonEmptyArray NonEmptyString)
  , description :: Maybe NonEmptyString
  , dependencies :: Map RawPackageName RawVersionRange
  }

fetchLegacyManifest :: GitHub.Octokit -> Cache -> GitHub.Address -> RawVersion -> Aff (Either String LegacyManifest)
fetchLegacyManifest octokit cache address ref = Except.runExceptT do
  eitherSpago <- liftAff $ fetchSpagoDhallJson octokit cache address ref
  eitherBower <- liftAff $ fetchBowerfile octokit cache address ref
  let eitherSpagoManifest = map spagoDhallJsonToLegacyManifest eitherSpago
  let eitherBowerManifest = map bowerfileToLegacyManifest eitherBower

  dependencies <- case eitherBowerManifest, eitherSpagoManifest of
    Left bowerErr, Left spagoErr -> throwError $ String.joinWith "\n"
      [ "Failed to fetch legacy manifest from repo " <> show address <> " at ref " <> show ref
      , "  Bower error: " <> GitHub.printGitHubError bowerErr
      , "  Spago error: " <> GitHub.printGitHubError spagoErr
      ]
    _, Right spago -> pure spago.dependencies
    Right bower, _ -> pure bower.dependencies

  license <- liftAff do
    let getFile = Except.runExceptT <<< GitHub.getContent octokit cache address (un RawVersion ref)
    packageJsonFile <- getFile "package.json"
    licenseFile <- getFile "LICENSE"
    detectedLicenses <- do
      let packageJsonInput = { name: "package.json", contents: _ } <$> hush packageJsonFile
      let licenseInput = { name: "LICENSE", contents: _ } <$> hush licenseFile
      Licensee.detectFiles (Array.catMaybes [ packageJsonInput, licenseInput ]) >>= case _ of
        Left err ->
          log ("Licensee decoding error, ignoring: " <> err) $> []
        Right licenses ->
          pure $ Array.mapMaybe NonEmptyString.fromString licenses
    pure $ NonEmptyArray.fromArray $ Array.nub $ Array.concat
      [ detectedLicenses
      , maybe [] NonEmptyArray.toArray $ _.license =<< hush eitherSpagoManifest
      , maybe [] NonEmptyArray.toArray $ _.license =<< hush eitherBowerManifest
      ]

  let description = join (_.description <$> hush eitherBowerManifest)

  pure { dependencies, license, description }

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

-- | Convert a Spago JSON file into the universal legacy manifest format.
spagoDhallJsonToLegacyManifest :: SpagoDhallJson -> LegacyManifest
spagoDhallJsonToLegacyManifest (SpagoDhallJson fields) = do
  let
    description = Nothing
    license = map NonEmptyArray.singleton fields.license
    dependencies = do
      let
        -- Package sets will produce exact versions for dependencies, ie. a
        -- dependency on "strings: 4.1.2" will produce the version range
        -- "strings: >=4.1.2 <4.1.3", which while technically correct is almost
        -- certainly not what the user wants. We instead treat these ranges as
        -- caret ranges, so "strings: 4.1.2" becomes "strings: >=4.1.2 <5.0.0".
        toRange (RawVersion fixed) = do
          let parsedVersion = Version.parseVersion Version.Lenient fixed
          let bump version = Version.printVersion (Version.bumpHighest version)
          RawVersionRange $ Either.either (const fixed) (\version -> Array.fold [ ">=", fixed, " <", bump version ]) parsedVersion
        findPackage name = Map.lookup name fields.packages
        foldFn deps name = maybe deps (\{ version } -> Map.insert name (toRange version) deps) (findPackage name)
      Array.foldl foldFn Map.empty fields.dependencies

  { description, license, dependencies }

-- | Attempt to construct a SpagoDhallJson file from a spago.dhall and
-- | packages.dhall file located in a remote repository at the given ref.
fetchSpagoDhallJson :: GitHub.Octokit -> Cache -> GitHub.Address -> RawVersion -> Aff (Either GitHub.GitHubError SpagoDhallJson)
fetchSpagoDhallJson octokit cache address (RawVersion ref) = Except.runExceptT do
  let getFile = GitHub.getContent octokit cache address ref
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

-- | Convert a Bowerfile into the universal legacy manifest format.
bowerfileToLegacyManifest :: Bowerfile -> LegacyManifest
bowerfileToLegacyManifest (Bowerfile fields) = do
  let
    description = NonEmptyString.fromString =<< fields.description
    license = NonEmptyArray.fromArray $ Array.mapMaybe NonEmptyString.fromString fields.license
    dependencies = fields.dependencies # Map.mapMaybeWithKey \(RawPackageName name) versionRange -> do
      -- We remove dependencies that don't begin with 'purescript-', as these
      -- indicate JavaScript dependencies.
      _ <- String.stripPrefix (String.Pattern "purescript-") name
      pure versionRange

  { description, license, dependencies }

-- | Attempt to construct a Bowerfile from a bower.json file rlocated in a
-- | remote repository at the given ref.
fetchBowerfile :: GitHub.Octokit -> Cache -> GitHub.Address -> RawVersion -> Aff (Either GitHub.GitHubError Bowerfile)
fetchBowerfile octokit cache address (RawVersion ref) = Except.runExceptT do
  bowerfile <- GitHub.getContent octokit cache address ref "bower.json"
  Except.except $ case Json.parseJson (JsonRepair.tryRepair bowerfile) of
    Left err -> Left $ GitHub.DecodeError err
    Right value -> pure value
