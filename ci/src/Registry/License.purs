module Registry.License (LicenseError(..), produceLicense) where

import Prelude

import Data.Argonaut as Json
import Data.Argonaut.Decode.Generic as Json.Decode.Generic
import Data.Argonaut.Encode.Generic as Json.Encode.Generic
import Data.Argonaut.Types.Generic as Json.Generic
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Foreign.SPDX (License)
import Foreign.SPDX as SPDX
import Registry.Prelude (genericShow, partitionEithers)

data LicenseError
  = MissingLicense
  | BadLicense (Array String)

derive instance eqLicenseError :: Eq LicenseError
derive instance ordLicenseError :: Ord LicenseError
derive instance genericLicenseError :: Generic LicenseError _
instance showLicenseError :: Show LicenseError where
  show e = genericShow e

instance Json.EncodeJson LicenseError where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith (Json.Generic.defaultEncoding { unwrapSingleArguments = true })

instance Json.DecodeJson LicenseError where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith (Json.Generic.defaultEncoding { unwrapSingleArguments = true })

produceLicense :: Maybe (NonEmptyArray NonEmptyString) -> Either LicenseError License
produceLicense license = do
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

  case license of
    Nothing -> Left MissingLicense
    Just licenses -> do
      let
        parsed =
          map (SPDX.parse <<< rewrite)
            $ filter (_ /= "LICENSE")
            $ map NES.toString
            $ NEA.toArray licenses
        { fail, success } = partitionEithers parsed

      case fail, success of
        [], [] -> Left MissingLicense
        [], _ -> Right $ SPDX.joinWith SPDX.Or success
        _, _ -> Left $ BadLicense fail