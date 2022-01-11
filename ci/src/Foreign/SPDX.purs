module Foreign.SPDX
  ( License
  , licenseCodec
  , parse
  , print
  , SPDXConjunction(..)
  , joinWith
  ) where

import Registry.Prelude

import Data.Codec (basicCodec, decode, encode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Function.Uncurried (Fn3, runFn3)
import Data.String as String
import Safe.Coerce (coerce)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype License = License String

derive newtype instance eqLicense :: Eq License

licenseCodec :: JsonCodec License
licenseCodec = basicCodec dec enc
  where
  enc = encode CA.string <<< print
  dec = lmap TypeMismatch <<< parse <=< decode CA.string

instance Show License where
  show = print

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
