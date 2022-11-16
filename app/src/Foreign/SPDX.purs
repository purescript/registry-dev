module Foreign.SPDX (fuzzyMatchLicense) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either as Either
import Data.Function.Uncurried (Fn1)
import Data.Function.Uncurried as Uncurried
import Registry.License (License)
import Registry.License as License

foreign import matchLicenseImpl :: Fn1 String (Array String)

-- | Attempt to fuzzy-match an SPDX identifier. Returns an array of potential
-- | SPDX identifiers that match the input string, or an empty array if none are
-- | similar enough.
fuzzyMatchLicense :: String -> Maybe (NonEmptyArray License)
fuzzyMatchLicense input = case Uncurried.runFn1 matchLicenseImpl input of
  [] -> Nothing
  matches ->
    map (Either.hush <<< License.parse) matches
      # Array.catMaybes
      # NonEmptyArray.fromArray
