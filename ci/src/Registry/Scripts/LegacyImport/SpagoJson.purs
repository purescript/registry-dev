module Registry.Scripts.LegacyImport.SpagoJson
  ( SpagoJson
  , toManifestFields
  , spagoJsonCodec
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as Common
import Data.Codec.Argonaut.Record as CAR
import Data.Map as Map
import Data.Profunctor (dimap)
import Data.String.NonEmpty (NonEmptyString)
import Foreign.Object as Object
import Registry.Codec (neString)
import Registry.Scripts.LegacyImport.Error (RawPackageName(..), RawVersion(..), rawPackageNameCodec, rawVersionCodec)
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)

toManifestFields :: SpagoJson -> ManifestFields
toManifestFields spago@(SpagoJson { license }) =
  { license: map NEA.singleton license
  , dependencies: packageDependencies spago
  , devDependencies: Object.empty
  , description: Nothing
  }

packageDependencies :: SpagoJson -> Object String
packageDependencies (SpagoJson { dependencies, packages }) = do
  let
    foldFn m name = fromMaybe m do
      version <- Map.lookup name packages
      pure $ Object.insert (un RawPackageName name) (un RawVersion version) m

  Array.foldl foldFn Object.empty dependencies

-- | The output of calling `dhall-to-json` on a `spago.dhall` file
newtype SpagoJson = SpagoJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName RawVersion
  }

derive newtype instance Eq SpagoJson

spagoJsonCodec :: JsonCodec SpagoJson
spagoJsonCodec = dimap (\(SpagoJson a) -> a) SpagoJson $ CAR.object "SpagoJson"
  { license: Common.maybe neString
  , dependencies: CA.array rawPackageNameCodec
  , packages: packagesCodec
  }
  where
  packagesCodec :: JsonCodec (Map RawPackageName RawVersion)
  packagesCodec = dimap packagesToObjVersion objVersionToPackages objVersionCodec

  objVersionCodec :: JsonCodec (Object { version :: RawVersion })
  objVersionCodec = Common.foreignObject
    $ CAR.object "objVersion"
        { version: rawVersionCodec }

  packagesToObjVersion :: Map RawPackageName RawVersion -> Object { version :: RawVersion }
  packagesToObjVersion = objectFromMap (un RawPackageName) <<< map { version: _ }

  objVersionToPackages :: Object { version :: RawVersion } -> Map RawPackageName RawVersion
  objVersionToPackages = map _.version <<< objectToMap (Just <<< RawPackageName)
