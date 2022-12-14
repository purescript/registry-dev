-- | Implementation of the `License` data type from the registry spec.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#license
-- |
-- | WARNING:
-- | This module relies on the 'spdx-expression-parse' NPM library, which you
-- | must install if you are using parsing code from this module. Please see the
-- | package.json file for exact versions.
module Registry.License
  ( License
  , SPDXConjunction(..)
  , codec
  , joinWith
  , parse
  , print
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either as Either
import Data.Function.Uncurried (Fn3, runFn3)
import Data.String as String
import Safe.Coerce (coerce)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype License = License String

derive newtype instance Eq License

-- | A codec for encoding and decoding a `License` as JSON
codec :: JsonCodec License
codec = CA.prismaticCodec "License" (Either.hush <<< parse) print CA.string

-- | Print an SPDX license identifier as a string.
print :: License -> String
print (License license) = license

foreign import parseSPDXLicenseIdImpl :: forall r. Fn3 (String -> r) (String -> r) String r

-- | Parse a string as a SPDX license identifier.
parse :: String -> Either String License
parse = runFn3 parseSPDXLicenseIdImpl Left (Right <<< License)

-- | A valid conjunction for SPDX license identifiers. AND means that both
-- | licenses must be satisfied; OR means that at least one license must be
-- | satisfied.
data SPDXConjunction = And | Or

derive instance Eq SPDXConjunction

-- | Join multiple license identifiers together with the given SPDX conjunction
-- | to create a new valid SPDX license identifier.
joinWith :: SPDXConjunction -> Array License -> License
joinWith = case _ of
  And -> coerce <<< String.joinWith " AND " <<< coerce
  Or -> coerce <<< String.joinWith " OR " <<< coerce
