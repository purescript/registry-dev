module Registry.SPDXLicense
  ( SPDXLicense
  , parse
  , print
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype SPDXLicense = SPDXLicense String

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

-- | Print an SPDX license identifier.
print :: SPDXLicense -> String
print (SPDXLicense license) = license

foreign import parseSPDXLicenseIdImpl :: forall r. Fn3 (String -> r) (String -> r) String r
