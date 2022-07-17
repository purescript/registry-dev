module Registry.Legacy.Manifest where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Either as Either
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
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
import Registry.Cache as Cache
import Registry.Hash (sha256String)
import Registry.Json ((.:), (.:?))
import Registry.Json as Json
import Registry.Legacy.PackageSet (LegacyPackageSet(..), LegacyPackageSetEntry(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM, throwWithComment)
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
toManifest name version location { license, description, dependencies } = do
  let files = Nothing
  let owners = Nothing
  Manifest { name, version, location, license, description, dependencies, files, owners }

fetchLegacyManifest :: Maybe (Map PackageName RawVersion) -> GitHub.Address -> RawVersion -> ExceptT LegacyManifestValidationError RegistryM LegacyManifest
fetchLegacyManifest packageSetsDeps address ref = do
  manifests <- fetchLegacyManifestFiles address ref

  dependencies <- do
    let
      -- Package sets will produce exact versions for dependencies, ie. a
      -- dependency on "strings: 4.1.2" will produce the version range
      -- "strings: >=4.1.2 <4.1.3", which while technically correct is almost
      -- certainly not what the user wants. We instead treat these ranges as
      -- caret ranges, so "strings: 4.1.2" becomes "strings: >=4.1.2 <5.0.0".
      toRange (RawVersion fixed) = do
        let parsedVersion = Version.parseVersion Version.Lenient fixed
        let bump version = Version.printVersion (Version.bumpHighest version)
        let printRange version = Array.fold [ ">=", fixed, " <", bump version ]
        RawVersionRange $ Either.either (const fixed) printRange parsedVersion

      validatePackageSetDeps = case packageSetsDeps of
        Nothing ->
          Left { error: InvalidDependencies [], reason: "No package sets dependencies." }
        Just deps -> do
          let converted = mapKeys (RawPackageName <<< PackageName.print) $ map toRange deps
          validateDependencies converted

      convertSpagoDeps packages = Array.foldl foldFn Map.empty
        where
        foldFn deps name = maybe deps (\{ version } -> Map.insert name (toRange version) deps) (findPackage name)
        findPackage name = Map.lookup name packages

      validateSpagoDeps (SpagoDhallJson { dependencies, packages }) = do
        validateDependencies (convertSpagoDeps packages dependencies)

      -- We remove dependencies that don't begin with 'purescript-', as these
      -- indicate JavaScript dependencies.
      convertBowerDeps = Map.mapMaybeWithKey \(RawPackageName name) range -> do
        _ <- String.stripPrefix (String.Pattern "purescript-") name
        pure range

      validateBowerDeps (Bowerfile { dependencies }) =
        validateDependencies (convertBowerDeps dependencies)

    Except.except case manifests of
      This bower -> validatePackageSetDeps <|> validateBowerDeps bower
      That spago -> validatePackageSetDeps <|> validateSpagoDeps spago
      Both bower spago -> validatePackageSetDeps <|> case validateBowerDeps bower, validateSpagoDeps spago of
        Left bowerError, Left _ -> Left bowerError
        Right bowerDeps, Left _ -> pure bowerDeps
        Left _, Right spagoDeps -> pure spagoDeps
        Right bowerDeps, Right spagoDeps -> pure $ do
          -- When both manifests produce viable dependencies, we first check to
          -- see if one is more up-to-date (e.g. has higher ranges specified).
          -- If so, we take that one. If not, we take the Bower manifest.
          let
            compareToSpago key bowerRange = fromMaybe 0 do
              spagoRange <- Map.lookup key spagoDeps
              let bowerUpperBound = Version.lessThan bowerRange
              let spagoUpperBound = Version.lessThan spagoRange
              if bowerUpperBound >= spagoUpperBound then pure 1 else pure (-1)

          if sum (mapWithIndex compareToSpago bowerDeps) >= 0 then bowerDeps else spagoDeps

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

type LegacyPackageSetEntries = Map PackageName (Map RawVersion (Map PackageName RawVersion))

fetchLegacyPackageSets :: RegistryM LegacyPackageSetEntries
fetchLegacyPackageSets = do
  { octokit, cache } <- ask
  result <- liftAff $ Except.runExceptT $ GitHub.listTags octokit cache { owner: "purescript", repo: "package-sets" }
  tags <- case result of
    Left err -> throwWithComment (GitHub.printGitHubError err)
    Right tags -> pure $ map _.name tags

  let
    convertPackageSet :: LegacyPackageSet -> LegacyPackageSetEntries
    convertPackageSet (LegacyPackageSet packages) =
      map (convertEntry packages) packages

    convertEntry :: Map PackageName LegacyPackageSetEntry -> LegacyPackageSetEntry -> Map RawVersion (Map PackageName RawVersion)
    convertEntry entries (LegacyPackageSetEntry { dependencies, version }) = do
      let
        -- Package sets are only valid if all packages in the set have dependencies that are also in
        -- the set, so this (should) be safe.
        resolveDependencyVersion dep = unsafeFromJust do
          LegacyPackageSetEntry entry <- Map.lookup dep entries
          pure entry.version
        resolveDependencyVersions =
          Array.foldl (\m name -> Map.insert name (resolveDependencyVersion name) m) Map.empty
      Map.singleton version (resolveDependencyVersions dependencies)

  legacySets <- do
    tagKey <- liftEffect do
      tagsSha <- sha256String (String.joinWith " " tags)
      pure ("package-sets-" <> show tagsSha)

    -- It's important that we cache the end result of unioning all package sets
    -- because the package sets are quite large and it's expensive to read them
    -- all into memory and fold over them.
    liftEffect (Cache.readJsonEntry tagKey cache) >>= case _ of
      Left _ -> do
        log $ "CACHE MISS: Building legacy package sets..."
        entries <- for tags \ref -> do
          let setKey = "legacy-package-set__" <> ref
          setEntries <- liftEffect (Cache.readJsonEntry setKey cache) >>= case _ of
            Left _ -> do
              log $ "CACHE MISS: Building legacy package set for " <> ref
              converted <- Except.runExceptT do
                packagesJson <- Except.mapExceptT liftAff $ GitHub.getContent octokit cache { owner: "purescript", repo: "package-sets" } ref "packages.json"
                parsed <- Except.except $ case Json.parseJson packagesJson of
                  Left decodeError -> throwError $ GitHub.DecodeError decodeError
                  Right legacySet -> pure legacySet
                pure $ convertPackageSet parsed
              liftEffect $ Cache.writeJsonEntry setKey converted cache
              pure converted
            Right contents ->
              pure contents.value
          case setEntries of
            Left err -> do
              log $ "Failed to retrieve " <> ref <> " package set:\n" <> GitHub.printGitHubError err
              pure Map.empty
            Right value -> pure value

        let merged = Array.foldl (\m set -> Map.unionWith Map.union set m) Map.empty entries
        liftEffect $ Cache.writeJsonEntry tagKey merged cache
        pure merged

      Right contents ->
        pure contents.value

  pure legacySets
