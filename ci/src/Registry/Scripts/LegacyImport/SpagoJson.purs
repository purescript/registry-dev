module Registry.Scripts.LegacyImport.SpagoJson
  ( SpagoJson
  , license
  , packageDependencies
  ) where

import Registry.Prelude

import Data.Argonaut ((.:?))
import Data.Argonaut as Json
import Data.Array as Array
import Data.Map as Map
import Foreign.Object as Object
import Registry.Scripts.LegacyImport.Error (RawPackageName(..), RawVersion)

license :: SpagoJson -> Maybe String
license (SpagoJson spago) = spago.license

packageDependencies :: SpagoJson -> Map RawPackageName RawVersion
packageDependencies (SpagoJson { dependencies, packages }) = do
  let
    foldFn m name = fromMaybe m do
      version <- Map.lookup name packages
      pure $ Map.insert name version m

  Array.foldl foldFn Map.empty dependencies

newtype SpagoJson = SpagoJson
  { license :: Maybe String
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName RawVersion
  }

derive newtype instance Eq SpagoJson

instance Json.EncodeJson SpagoJson where
  encodeJson (SpagoJson spago) = do
    let
      packagesMap = map { version: _ } spago.packages
      packagesObject = objectFromMap (un RawPackageName) packagesMap

    Json.encodeJson
      { license: spago.license
      , dependencies: spago.dependencies
      , packages: packagesObject
      }

instance Json.DecodeJson SpagoJson where
  decodeJson json = do
    obj <- Json.decodeJson json
    license' <- obj .:? "license"
    dependencies <- fromMaybe mempty <$> obj .:? "dependencies"
    packageObj :: Object { version :: RawVersion } <- fromMaybe Object.empty <$> obj .:? "packages"
    let
      packagesMap = objectToMap (Just <<< RawPackageName) packageObj
      packages = map _.version packagesMap
    pure $ SpagoJson { license: license', dependencies, packages }
