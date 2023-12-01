-- | This module defines the Registry decoder for spago.yaml files, one of the
-- | supported package manager manifest types.
module Registry.App.Manifest.SpagoYaml where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut (JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.Manifest (Manifest(..))
import Registry.Owner as Owner
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version as Version

-- | Attempt to convert a spago.yaml file to a Manifest
spagoYamlToManifest :: SpagoYaml -> Either String Manifest
spagoYamlToManifest config = do
  package@{ name, description, dependencies: spagoDependencies } <- note "No 'package' key found in config." config.package
  publish@{ version, license, owners } <- note "No 'publish' key found under the 'package' key in config." package.publish
  location <- note "No 'location' key found under the 'publish' key in config." publish.location
  let includeFiles = NonEmptyArray.fromArray =<< (Array.mapMaybe NonEmptyString.fromString <$> publish.include)
  let excludeFiles = NonEmptyArray.fromArray =<< (Array.mapMaybe NonEmptyString.fromString <$> publish.exclude)
  let printRangeError packages = "The following packages did not have their ranges specified: " <> String.joinWith ", " (map PackageName.print (Set.toUnfoldable packages))
  dependencies <- lmap printRangeError $ convertSpagoDependencies spagoDependencies
  pure $ Manifest
    { name
    , version
    , description
    , license
    , location
    , owners
    , includeFiles
    , excludeFiles
    , dependencies
    }

-- | Read a spago.yaml file from disk at the specified path.
readSpagoYaml :: forall m. MonadAff m => FilePath -> m (Either String SpagoYaml)
readSpagoYaml = liftAff <<< readYamlFile spagoYamlCodec

-- | A spago.yaml config
type SpagoYaml = { package :: Maybe PackageConfig }

spagoYamlCodec :: JsonCodec SpagoYaml
spagoYamlCodec = CA.Record.object "SpagoYaml"
  { package: CA.Record.optional packageConfigCodec
  }

type PackageConfig =
  { name :: PackageName
  , description :: Maybe String
  , dependencies :: Map PackageName (Maybe SpagoRange)
  , publish :: Maybe PublishConfig
  }

packageConfigCodec :: JsonCodec PackageConfig
packageConfigCodec = CA.Record.object "PackageConfig"
  { name: PackageName.codec
  , description: CA.Record.optional CA.string
  , dependencies: dependenciesCodec
  , publish: CA.Record.optional publishConfigCodec
  }

type PublishConfig =
  { version :: Version
  , license :: License
  , location :: Maybe Location
  , include :: Maybe (Array String)
  , exclude :: Maybe (Array String)
  , owners :: Maybe (NonEmptyArray Owner)
  }

publishConfigCodec :: JsonCodec PublishConfig
publishConfigCodec = CA.Record.object "PublishConfig"
  { version: Version.codec
  , license: License.codec
  , location: CA.Record.optional Location.codec
  , include: CA.Record.optional (CA.array CA.string)
  , exclude: CA.Record.optional (CA.array CA.string)
  , owners: CA.Record.optional (CA.Common.nonEmptyArray Owner.codec)
  }

dependenciesCodec :: JsonCodec (Map PackageName (Maybe SpagoRange))
dependenciesCodec = Profunctor.dimap toJsonRep fromJsonRep $ CA.array dependencyCodec
  where
  -- Dependencies are encoded as an array, where the array can contain either
  -- a package name only (no range), or a package name with "*" (unbounded range),
  --  or a valid Registry range.
  toJsonRep :: Map PackageName (Maybe SpagoRange) -> Array (Either PackageName (Tuple PackageName SpagoRange))
  toJsonRep deps = do
    let convert (Tuple name maybeSpagoRange) = maybe (Left name) (Right <<< Tuple name) maybeSpagoRange
    map convert $ Map.toUnfoldable deps

  fromJsonRep :: Array (Either PackageName (Tuple PackageName SpagoRange)) -> Map PackageName (Maybe SpagoRange)
  fromJsonRep = Map.fromFoldable <<< map (either (\name -> Tuple name Nothing) (map Just))

  -- Pairs of package name & range are encoded as a singleton map in the conversion
  -- from YAML to JSON, so we decode the received map explicitly as a tuple.
  singletonCodec :: JsonCodec (Tuple PackageName SpagoRange)
  singletonCodec = CA.codec' decode encode
    where
    encode (Tuple name range) = CA.encode (Internal.Codec.packageMap spagoRangeCodec) (Map.singleton name range)
    decode json = do
      singleton <- CA.decode (Internal.Codec.packageMap spagoRangeCodec) json
      case Map.toUnfoldable singleton of
        [ Tuple name range ] -> Right (Tuple name range)
        [] -> Left $ TypeMismatch "Expected a singleton map but received an empty one"
        xs -> Left $ TypeMismatch $ "Expected a singleton map but received a map with " <> show (Array.length xs) <> " elements."

  dependencyCodec :: JsonCodec (Either PackageName (Tuple PackageName SpagoRange))
  dependencyCodec = CA.codec' decode encode
    where
    encode = case _ of
      Left name -> CA.encode PackageName.codec name
      Right tuple -> CA.encode singletonCodec tuple

    decode json =
      map Left (CA.decode PackageName.codec json)
        <|> map Right (CA.decode singletonCodec json)

convertSpagoDependencies :: Map PackageName (Maybe SpagoRange) -> Either (Set PackageName) (Map PackageName Range)
convertSpagoDependencies dependencies = do
  let
    convert :: Tuple PackageName (Maybe SpagoRange) -> Either PackageName (Tuple PackageName Range)
    convert (Tuple name maybeSpagoRange) = case maybeSpagoRange of
      Nothing -> Left name
      Just Unbounded -> Left name
      Just (Bounded range) -> Right (Tuple name range)

    partitioned = partitionEithers $ map convert $ Map.toUnfoldable dependencies

  if Array.null partitioned.fail then
    Right $ Map.fromFoldable partitioned.success
  else
    Left $ Set.fromFoldable partitioned.fail

-- | A range specifier in a Spago configuration, which can be either "*"
-- | (an unbounded range) or a valid Registry range.
data SpagoRange = Unbounded | Bounded Range

parseSpagoRange :: String -> Either String SpagoRange
parseSpagoRange = case _ of
  "*" -> Right Unbounded
  range -> Bounded <$> Range.parse range

printSpagoRange :: SpagoRange -> String
printSpagoRange = case _ of
  Unbounded -> "*"
  Bounded range -> Range.print range

spagoRangeCodec :: JsonCodec SpagoRange
spagoRangeCodec = CA.codec' decode encode
  where
  encode = CA.encode CA.string <<< printSpagoRange
  decode = CA.decode CA.string >=> parseSpagoRange >>> lmap (append "SpagoRange: " >>> CA.TypeMismatch)
