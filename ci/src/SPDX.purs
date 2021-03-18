module Registry.SPDX where

-- | Verifies that the input is a valid SPDX license identifier.
-- |
-- | ```purs
-- | > isValidSPDXLicenseId 'BSD-3-Clause'
-- | true
-- |
-- | > isValidSPDXLicenseId 'MITT'
-- | false
-- | ```
foreign import isValidSPDXLicenseId :: String -> Boolean
