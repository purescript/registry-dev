module Registry.Scripts.LegacyImport.SpagoJson
  ( SpagoJson
  , toManifestFields
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String.NonEmpty (NonEmptyString)
import Registry.Json (class RegistryJson, (.:?))
import Registry.Json as Json
import Registry.Scripts.LegacyImport.Error (RawPackageName, RawVersion)
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)

toManifestFields :: SpagoJson -> ManifestFields
toManifestFields (SpagoJson { license, dependencies, packages }) =
  { license: map NEA.singleton license
  , dependencies: do
      let foldFn deps name = maybe deps (\{ version } -> Map.insert name version deps) (Map.lookup name packages)
      Array.foldl foldFn Map.empty dependencies
  , devDependencies: Map.empty
  , description: Nothing
  }

-- | The output of calling `dhall-to-json` on a `spago.dhall` file
newtype SpagoJson = SpagoJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName SpagoPackage
  }

derive newtype instance Eq SpagoJson

type SpagoPackage = { version :: RawVersion }

instance RegistryJson SpagoJson where
  encode (SpagoJson spago) = Json.encode spago
  decode json = do
    obj <- Json.decode json
    license <- obj .:? "license"
    dependencies <- fromMaybe [] <$> obj .:? "dependencies"
    packages <- fromMaybe Map.empty <$> obj .:? "packages"
    pure $ SpagoJson { license, dependencies, packages }
