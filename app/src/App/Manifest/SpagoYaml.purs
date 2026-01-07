-- | This module defines the Registry decoder for spago.yaml files, one of the
-- | supported package manager manifest types.
module Registry.App.Manifest.SpagoYaml where

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

-- | Attempt to convert a spago.yaml file to a Manifest. The ref parameter is
-- | the Git reference (tag or commit) used to fetch this version's source.
spagoYamlToManifest :: String -> SpagoYaml -> Either String Manifest
spagoYamlToManifest ref config = do
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
    , ref
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

spagoYamlCodec :: CJ.Codec SpagoYaml
spagoYamlCodec = CJ.named "SpagoYaml" $ CJ.Record.object
  { package: CJ.Record.optional packageConfigCodec
  }

type PackageConfig =
  { name :: PackageName
  , description :: Maybe String
  , dependencies :: Map PackageName (Maybe SpagoRange)
  , publish :: Maybe PublishConfig
  }

packageConfigCodec :: CJ.Codec PackageConfig
packageConfigCodec = CJ.named "PackageConfig" $ CJ.Record.object
  { name: PackageName.codec
  , description: CJ.Record.optional CJ.string
  , dependencies: dependenciesCodec
  , publish: CJ.Record.optional publishConfigCodec
  }

type PublishConfig =
  { version :: Version
  , license :: License
  , location :: Maybe Location
  , include :: Maybe (Array String)
  , exclude :: Maybe (Array String)
  , owners :: Maybe (NonEmptyArray Owner)
  }

publishConfigCodec :: CJ.Codec PublishConfig
publishConfigCodec = CJ.named "PublishConfig" $ CJ.Record.object
  { version: Version.codec
  , license: License.codec
  , location: CJ.Record.optional Location.codec
  , include: CJ.Record.optional (CJ.array CJ.string)
  , exclude: CJ.Record.optional (CJ.array CJ.string)
  , owners: CJ.Record.optional (CJ.Common.nonEmptyArray Owner.codec)
  }

dependenciesCodec :: CJ.Codec (Map PackageName (Maybe SpagoRange))
dependenciesCodec = Profunctor.dimap toJsonRep fromJsonRep $ CJ.array dependencyCodec
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
  singletonCodec :: CJ.Codec (Tuple PackageName SpagoRange)
  singletonCodec = Codec.codec' decode encode
    where
    encode (Tuple name range) = CJ.encode (Internal.Codec.packageMap spagoRangeCodec) (Map.singleton name range)
    decode json = do
      singleton <- Codec.decode (Internal.Codec.packageMap spagoRangeCodec) json
      except case Map.toUnfoldable singleton of
        [ Tuple name range ] -> Right (Tuple name range)
        [] -> Left $ CJ.DecodeError.basic "Expected a singleton map but received an empty one"
        xs -> Left $ CJ.DecodeError.basic $ "Expected a singleton map but received a map with " <> show (Array.length xs) <> " elements."

  dependencyCodec :: CJ.Codec (Either PackageName (Tuple PackageName SpagoRange))
  dependencyCodec = Codec.codec' decode encode
    where
    encode = case _ of
      Left name -> CJ.encode PackageName.codec name
      Right tuple -> CJ.encode singletonCodec tuple

    decode json =
      map Left (Codec.decode PackageName.codec json)
        <|> map Right (Codec.decode singletonCodec json)

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

spagoRangeCodec :: CJ.Codec SpagoRange
spagoRangeCodec = CJ.named "SpagoRange" $ Codec.codec' decode encode
  where
  encode = CJ.encode CJ.string <<< printSpagoRange
  decode = Codec.decode CJ.string >=> (parseSpagoRange >>> lmap CJ.DecodeError.basic >>> except)
