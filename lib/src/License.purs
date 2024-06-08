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

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (Except, except)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.String as String
import JSON (JSON)
import Safe.Coerce (coerce)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype License = License String

derive newtype instance Eq License

-- | A codec for encoding and decoding a `License` as JSON
codec :: CJ.Codec License
codec = CJ.named "License" $ Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError License
  decode = except <<< lmap CJ.DecodeError.basic <<< parse <=< Codec.decode CJ.string

  encode :: License -> JSON
  encode = print >>> CJ.encode CJ.string

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
