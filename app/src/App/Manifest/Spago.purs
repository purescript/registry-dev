module Registry.App.Manifest.Spago
  ( getSpagoManifest
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either as Either
import Data.List as List
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Effect.Aff as Aff
import Foreign.Yaml as Yaml
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS
import Registry.App.Effect.Comment (COMMENT)
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.Manifest (Manifest(..))
import Registry.Manifest as Manifest
import Registry.Owner as Owner
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT)
import Run.Except as Except

getSpagoManifest :: forall r. FilePath -> Run (LOG + COMMENT + EXCEPT String + AFF + EFFECT + r) Manifest
getSpagoManifest spagoYamlPath = do
  readConfig spagoYamlPath >>= case _ of
    Left readErr -> Except.throw $ String.joinWith "\n"
      [ "Could not publish your package - a spago.yaml was present, but it was not possible to read it:"
      , readErr
      ]
    Right config -> do
      -- Once we have the config we are still not entirely sure it fits into a Manifest
      -- e.g. need to make sure all the ranges are present
      case convertSpagoManifest config of
        Left err -> Except.throw $ String.joinWith "\n"
          [ "Could not publish your package - there was an error while converting your spago.yaml into a purs.json manifest:"
          , err
          ]
        Right manifest -> do
          Log.debug "Succesfully converted a spago.yaml into a purs.json manifest"
          Comment.comment $ Array.fold
            [ "Converted your spago.yaml into a purs.json manifest to use for publishing:\n"
            , "```json"
            , printJson Manifest.codec manifest
            , "```"
            ]
          pure manifest

readConfig :: forall r. FilePath -> Run (LOG + COMMENT + EXCEPT String + AFF + EFFECT + r) (Either String SpagoYaml)
readConfig path = liftAff do
  liftEffect (FS.exists path) >>= case _ of
    false -> pure (Left $ "Did not find " <> path <> " file.")
    true -> do
      result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
      pure do
        yamlStr <- lmap Aff.message result
        docAsJson <- lmap (append "YAML: ") (Yaml.yamlParser yamlStr)
        parseSpagoManifest docAsJson

parseSpagoManifest :: Json -> Either String SpagoYaml
parseSpagoManifest = lmap CA.printJsonDecodeError <<< CA.decode spagoYamlCodec

type SpagoYaml =
  { package ::
      Maybe
        { name :: PackageName
        , description :: Maybe String
        , dependencies :: Map PackageName (Maybe Range)
        , publish ::
            Maybe
              { version :: Version
              , license :: License
              , location :: Maybe Location
              , include :: Maybe (Array String)
              , exclude :: Maybe (Array String)
              , owners :: Maybe (NonEmptyArray Owner)
              }
        }
  }

spagoYamlCodec :: CA.JsonCodec SpagoYaml
spagoYamlCodec = CAR.object "SpagoYaml"
  { package: CAR.optional $ CAR.object "PackageConfig"
      { name: PackageName.codec
      , description: CAR.optional CA.string
      , dependencies: dependenciesCodec
      , publish: CAR.optional $ CAR.object "PublishConfig"
          { version: Version.codec
          , license: License.codec
          , location: CAR.optional Location.codec
          , include: CAR.optional (CA.array CA.string)
          , exclude: CAR.optional (CA.array CA.string)
          , owners: CAR.optional (CAC.nonEmptyArray Owner.codec)
          }
      }
  }

dependenciesCodec :: JsonCodec (Map PackageName (Maybe Range))
dependenciesCodec = Profunctor.dimap to from $ CA.array dependencyCodec
  where
  packageSingletonCodec = Internal.Codec.packageMap spagoRangeCodec

  to :: Map PackageName (Maybe Range) -> Array (Either PackageName (Map PackageName Range))
  to deps =
    map
      ( \(Tuple name maybeRange) -> case maybeRange of
          Nothing -> Left name
          Just r -> Right (Map.singleton name r)
      )
      $ Map.toUnfoldable deps :: Array _

  from :: Array (Either PackageName (Map PackageName Range)) -> Map PackageName (Maybe Range)
  from = Map.fromFoldable <<< map
    ( case _ of
        Left name -> Tuple name Nothing
        Right m -> map Just $ unsafeFromJust (List.head (Map.toUnfoldable m))
    )

  dependencyCodec :: JsonCodec (Either PackageName (Map PackageName Range))
  dependencyCodec = CA.codec' decode encode
    where
    encode = case _ of
      Left name -> CA.encode PackageName.codec name
      Right singletonMap -> CA.encode packageSingletonCodec singletonMap

    decode json =
      map Left (CA.decode PackageName.codec json)
        <|> map Right (CA.decode packageSingletonCodec json)

widestRange :: Range
widestRange = Either.fromRight' (\_ -> unsafeCrashWith "Fake range failed")
  $ Range.parse ">=0.0.0 <2147483647.0.0"

spagoRangeCodec :: JsonCodec Range
spagoRangeCodec = CA.prismaticCodec "SpagoRange" rangeParse printSpagoRange CA.string
  where
  rangeParse str =
    if str == "*" then Just widestRange
    else hush $ Range.parse str

printSpagoRange :: Range -> String
printSpagoRange range =
  if range == widestRange then "*"
  else Range.print range

convertSpagoManifest :: SpagoYaml -> Either String Manifest
convertSpagoManifest config = do
  package@{ name, description, dependencies: deps } <- note "Did not find a package in the config" config.package
  publishConfig@{ version, license, owners } <- note "Did not find a `publish` section in the package config" package.publish
  let includeFiles = NonEmptyArray.fromArray =<< (Array.mapMaybe NonEmptyString.fromString <$> publishConfig.include)
  let excludeFiles = NonEmptyArray.fromArray =<< (Array.mapMaybe NonEmptyString.fromString <$> publishConfig.exclude)
  location <- note "Did not find a `location` field in the publish config" publishConfig.location
  let
    checkRange :: Tuple PackageName (Maybe Range) -> Either PackageName (Tuple PackageName Range)
    checkRange (Tuple packageName maybeRange) = case maybeRange of
      Nothing -> Left packageName
      Just r -> Right (Tuple packageName r)
  let { fail: failedPackages, success } = partitionEithers $ map checkRange (Map.toUnfoldable deps :: Array _)
  dependencies <- case failedPackages of
    [] -> Right (Map.fromFoldable success)
    errs -> Left $ "The following packages did not have their ranges specified: " <> String.joinWith ", " (map PackageName.print errs)
  pure $ Manifest
    { version
    , license
    , name
    , location
    , description
    , dependencies
    , owners
    , includeFiles
    , excludeFiles
    }

