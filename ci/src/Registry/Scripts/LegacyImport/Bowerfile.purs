module Registry.Scripts.LegacyImport.Bowerfile
  ( Bowerfile(..)
  , toManifestFields
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Data.String.NonEmpty as NES
import Registry.Json ((.:?))
import Registry.Json as Json
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)

toManifestFields :: Bowerfile -> ManifestFields
toManifestFields (Bowerfile fields) = do
  let
    -- We trim out packages that don't begin with `purescript-`, as these
    -- are JavaScript dependencies being specified in the Bowerfile.
    processNames = Array.mapMaybe \(Tuple (RawPackageName name) version) -> do
      stripped <- String.stripPrefix (String.Pattern "purescript-") name
      pure $ Tuple (RawPackageName stripped) version

  fields { dependencies = Map.fromFoldable $ processNames $ Map.toUnfoldable fields.dependencies }

newtype Bowerfile = Bowerfile ManifestFields

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
      Nothing -> pure Nothing
      Just value -> do
        array <- (Json.decode value <#> Array.singleton) <|> Json.decode value
        pure $ NEA.fromArray $ Array.mapMaybe NES.fromString array
    pure $ Bowerfile { description, license, dependencies }
