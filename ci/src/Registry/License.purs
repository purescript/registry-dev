module Registry.License (produceLicense) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Foreign.SPDX (License)
import Foreign.SPDX as SPDX
import Registry.Error (mkError)
import Registry.Prelude (partitionEithers)
import Registry.Scripts.LegacyImport.Error (ManifestError(..))

produceLicense :: Maybe (NonEmptyArray NonEmptyString) -> Either (NonEmptyArray ManifestError) License
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
    Nothing -> mkError MissingLicense
    Just licenses -> do
      let
        parsed =
          map (SPDX.parse <<< rewrite)
            $ filter (_ /= "LICENSE")
            $ map NES.toString
            $ NEA.toArray licenses
        { fail, success } = partitionEithers parsed

      case fail, success of
        [], [] -> mkError MissingLicense
        [], _ -> Right $ SPDX.joinWith SPDX.Or success
        _, _ -> mkError $ BadLicense fail