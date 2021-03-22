module Registry.SPDXLicense
  ( SPDXLicense
  , parse
  , print
  , SPDXConjunction(..)
  , joinWith
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.String as String
import Safe.Coerce (coerce)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype SPDXLicense = SPDXLicense String

instance decodeJsonSPDXLicense :: DecodeJson SPDXLicense where
  decodeJson = lmap TypeMismatch <<< parse <=< decodeJson

instance encodeJsonSPDXLicense :: EncodeJson SPDXLicense where
  encodeJson = encodeJson <<< print

-- | Print an SPDX license identifier.
print :: SPDXLicense -> String
print (SPDXLicense license) = license

-- | Parse a string as a SPDX license identifier.
-- |
-- | ```purs
-- | > parse 'BSD-3-Clause'
-- | Right (SPDXLicense ...)
-- |
-- | > parse 'MITT'
-- | Left "Invalid SPDX identifier: MIT. Did you mean MIT?"
-- | ```
parse :: String -> Either String SPDXLicense
parse = runFn3 parseSPDXLicenseIdImpl Left (Right <<< SPDXLicense)

data SPDXConjunction = And | Or

joinWith :: SPDXConjunction -> Array SPDXLicense -> SPDXLicense
joinWith = case _ of
  And -> coerce <<< String.joinWith " AND " <<< coerce
  Or -> coerce <<< String.joinWith " OR " <<< coerce

foreign import parseSPDXLicenseIdImpl :: forall r. Fn3 (String -> r) (String -> r) String r
