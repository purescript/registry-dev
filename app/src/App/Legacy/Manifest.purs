-- | Support for parsing legacy manifest formats (bower.json, spago.dhall) and
-- | converting them to the registry's purs.json format.
module Registry.App.Legacy.Manifest
  ( Bowerfile(..)
  , SpagoDhallJson(..)
  , bowerfileCodec
  , bowerfileToPursJson
  , dhallToJson
  , legacyManifestCodec
  , spagoDhallJsonCodec
  , spagoDhallToPursJson
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Node.ChildProcess.Types (Exit(..))
import Node.Library.Execa as Execa
import Registry.App.Legacy.LenientRange as LenientRange
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), RawVersionRange(..), rawPackageNameMapCodec, rawVersionCodec, rawVersionRangeCodec)
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.Owner as Owner
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version as Version

-- | A minimal representation of a bower.json file containing the fields
-- | we need to convert to a purs.json manifest.
newtype Bowerfile = Bowerfile
  { description :: Maybe String
  , dependencies :: Map RawPackageName RawVersionRange
  , license :: Array String
  }

derive instance Newtype Bowerfile _
derive newtype instance Eq Bowerfile

bowerfileCodec :: CJ.Codec Bowerfile
bowerfileCodec = Profunctor.dimap toRep fromRep $ CJ.named "Bowerfile" $ CJ.Record.object
  { description: CJ.Record.optional CJ.string
  , dependencies: CJ.Record.optional dependenciesCodec
  , license: CJ.Record.optional licenseCodec
  }
  where
  toRep (Bowerfile fields) = fields { dependencies = Just fields.dependencies, license = Just fields.license }
  fromRep fields = Bowerfile $ fields { dependencies = fromMaybe Map.empty fields.dependencies, license = fromMaybe [] fields.license }

  dependenciesCodec :: CJ.Codec (Map RawPackageName RawVersionRange)
  dependenciesCodec = rawPackageNameMapCodec rawVersionRangeCodec

  licenseCodec :: CJ.Codec (Array String)
  licenseCodec = Codec.codec' decode encode
    where
    encode = CJ.encode (CJ.array CJ.string)
    decode json = Codec.decode (CJ.array CJ.string) json
      <|> map Array.singleton (Codec.decode CJ.string json)

-- | Convert a Bowerfile to the fields needed for a purs.json manifest.
-- | Returns an error message if the conversion fails.
bowerfileToPursJson
  :: Bowerfile
  -> Either String { license :: License, description :: Maybe String, dependencies :: Map PackageName Range }
bowerfileToPursJson (Bowerfile { description, dependencies, license }) = do
  let
    parsedLicenses = license <#> \rawLicense ->
      lmap (\err -> "  - " <> rawLicense <> ": " <> err) $ License.parse rawLicense
    { fail: parseErrors, success: validLicenses } = partitionEithers parsedLicenses

  parsedLicense <-
    if not (Array.null parseErrors) then do
      Left $ "Invalid SPDX license(s) in bower.json:\n" <> String.joinWith "\n" parseErrors
    else do
      case NonEmptyArray.fromArray validLicenses of
        Nothing -> Left "No valid SPDX license found in bower.json"
        Just multiple -> Right $ License.joinWith License.And multiple

  parsedDeps <- parseDependencies dependencies

  Right { license: parsedLicense, description, dependencies: parsedDeps }

-- | Parse bower.json dependencies (which use package names like "purescript-prelude")
-- | into registry dependencies (which use names like "prelude").
parseDependencies :: Map RawPackageName RawVersionRange -> Either String (Map PackageName Range)
parseDependencies deps = do
  let
    parseDep (Tuple (RawPackageName rawName) (RawVersionRange rawRange)) = do
      let name = fromMaybe rawName $ String.stripPrefix (String.Pattern "purescript-") rawName
      pkgName <- lmap (\_ -> "Invalid package name: " <> name) $ PackageName.parse name
      range <- lmap (\_ -> "Invalid version range for " <> name <> ": " <> rawRange) $ LenientRange.parse rawRange
      Right $ Tuple pkgName (un LenientRange.LenientRange range).range

  parsedPairs <- traverse parseDep (Map.toUnfoldable deps :: Array _)
  Right $ Map.fromFoldable parsedPairs

-- | The result of calling 'dhall-to-json' on a 'spago.dhall' file and
-- | accompanying 'packages.dhall' file.
newtype SpagoDhallJson = SpagoDhallJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName { version :: RawVersion }
  }

derive instance Newtype SpagoDhallJson _

spagoDhallJsonCodec :: CJ.Codec SpagoDhallJson
spagoDhallJsonCodec = Profunctor.dimap toRep fromRep $ CJ.named "SpagoDhallJson" $ CJ.Record.object
  { license: CJ.Record.optional CJ.Common.nonEmptyString
  , dependencies: CJ.Record.optional (CJ.array (Profunctor.wrapIso RawPackageName CJ.string))
  , packages: CJ.Record.optional packageVersionMapCodec
  }
  where
  packageVersionMapCodec :: CJ.Codec (Map RawPackageName { version :: RawVersion })
  packageVersionMapCodec = rawPackageNameMapCodec $ CJ.named "VersionObject" $ CJ.Record.object { version: rawVersionCodec }

  toRep (SpagoDhallJson fields) = fields
    { dependencies = if Array.null fields.dependencies then Nothing else Just fields.dependencies
    , packages = if Map.isEmpty fields.packages then Nothing else Just fields.packages
    }

  fromRep fields = SpagoDhallJson $ fields
    { dependencies = fromMaybe [] fields.dependencies
    , packages = fromMaybe Map.empty fields.packages
    }

-- | Convert a SpagoDhallJson to the fields needed for a purs.json manifest.
-- | Returns an error message if the conversion fails.
spagoDhallToPursJson
  :: SpagoDhallJson
  -> Either String { license :: License, description :: Maybe String, dependencies :: Map PackageName Range }
spagoDhallToPursJson (SpagoDhallJson { license, dependencies, packages }) = do
  parsedLicense <- case license of
    Nothing -> Left "No license found in spago.dhall"
    Just lic -> case License.parse (NonEmptyString.toString lic) of
      Left _ -> Left $ "Invalid SPDX license in spago.dhall: " <> NonEmptyString.toString lic
      Right l -> Right l

  -- Build a map from package names to version ranges by looking up versions in packages
  parsedDeps <- parseDhallDependencies dependencies packages

  Right { license: parsedLicense, description: Nothing, dependencies: parsedDeps }

-- | Parse spago.dhall dependencies by looking up versions in the packages map.
-- | Dependency names may have "purescript-" prefix which should be stripped.
parseDhallDependencies :: Array RawPackageName -> Map RawPackageName { version :: RawVersion } -> Either String (Map PackageName Range)
parseDhallDependencies deps packages = do
  let
    parseDep (RawPackageName rawName) = do
      let name = fromMaybe rawName $ String.stripPrefix (String.Pattern "purescript-") rawName
      pkgName <- lmap (\_ -> "Invalid package name: " <> name) $ PackageName.parse name
      -- Look up version in packages map (try both with and without prefix)
      let
        lookupName = RawPackageName rawName
        lookupNameStripped = RawPackageName name
        lookupNamePrefixed = RawPackageName ("purescript-" <> name)
      case Map.lookup lookupName packages <|> Map.lookup lookupNameStripped packages <|> Map.lookup lookupNamePrefixed packages of
        Nothing -> Left $ "Dependency " <> name <> " not found in packages"
        Just { version: RawVersion rawVersion } -> do
          parsedVersion <- lmap (\_ -> "Invalid version for " <> name <> ": " <> rawVersion) $ LenientVersion.parse rawVersion
          let ver = LenientVersion.version parsedVersion
          Right $ Tuple pkgName (Range.exact ver)

  parsedPairs <- traverse parseDep deps
  Right $ Map.fromFoldable parsedPairs

-- | Convert a string representing a Dhall expression into JSON using the
-- | `dhall-to-json` CLI.
dhallToJson :: { dhall :: String, cwd :: Maybe FilePath } -> Aff (Either String JSON)
dhallToJson { dhall, cwd } = do
  let cmd = "dhall-to-json"
  let args = []
  process <- Execa.execa cmd args (_ { cwd = cwd })
  for_ process.stdin \{ writeUtf8End } -> writeUtf8End dhall
  result <- process.getResult
  pure case result.exit of
    Normally 0 -> lmap CJ.DecodeError.print $ parseJson CJ.json result.stdout
    _ -> Left result.stderr

-- | A codec for parsing legacy purs.json manifests that may not have a `ref`
-- | field. If `ref` is missing, we use the provided fallback (typically the
-- | version prefixed with "v", matching the convention used when building
-- | manifests from legacy sources).
legacyManifestCodec :: String -> CJ.Codec Manifest
legacyManifestCodec fallbackRef = Profunctor.dimap toRep fromRep $ CJ.named "Manifest" $ CJ.object
  $ CJ.recordProp @"name" PackageName.codec
  $ CJ.recordProp @"version" Version.codec
  $ CJ.recordProp @"license" License.codec
  $ CJ.recordPropOptional @"description" (Internal.Codec.limitedString 300)
  $ CJ.recordProp @"location" Location.codec
  $ CJ.recordPropOptional @"ref" CJ.string
  $ CJ.recordPropOptional @"owners" (CJ.Common.nonEmptyArray Owner.codec)
  $ CJ.recordPropOptional @"includeFiles" (CJ.Common.nonEmptyArray CJ.Common.nonEmptyString)
  $ CJ.recordPropOptional @"excludeFiles" (CJ.Common.nonEmptyArray CJ.Common.nonEmptyString)
  $ CJ.recordProp @"dependencies" (Internal.Codec.packageMap Range.codec)
  $ CJ.record
  where
  toRep (Manifest m) = m { ref = Just m.ref }
  fromRep r = Manifest r { ref = fromMaybe fallbackRef r.ref }
