module Foreign.SPDX
  ( License
  , parse
  , print
  , SPDXConjunction(..)
  , joinWith
  ) where

import Registry.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.String as String
import Safe.Coerce (coerce)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype License = License String

instance decodeJsonSPDXLicense :: DecodeJson License where
  decodeJson = lmap TypeMismatch <<< parse <=< decodeJson

instance encodeJsonSPDXLicense :: EncodeJson License where
  encodeJson = encodeJson <<< print

-- | Print an SPDX license identifier.
print :: License -> String
print (License license) = license

-- | Parse a string as a SPDX license identifier.
-- |
-- | ```purs
-- | > parse 'BSD-3-Clause'
-- | Right (SPDXLicense ...)
-- |
-- | > parse 'MITT'
-- | Left "Invalid SPDX identifier: MITT. Did you mean MIT?"
-- | ```
parse :: String -> Either String License
parse = runFn3 parseSPDXLicenseIdImpl Left (Right <<< License)

data SPDXConjunction = And | Or

joinWith :: SPDXConjunction -> Array License -> License
joinWith = case _ of
  And -> coerce <<< String.joinWith " AND " <<< coerce
  Or -> coerce <<< String.joinWith " OR " <<< coerce

foreign import parseSPDXLicenseIdImpl :: forall r. Fn3 (String -> r) (String -> r) String r
