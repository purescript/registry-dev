module Test.Registry.Sha256 (spec) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Node.ChildProcess as Process
import Node.Path (FilePath)
import Node.Path as Path
import Registry.Sha256 as Sha256
import Sunde as Sunde
import Test.Spec as Spec
import Test.Utils as Utils
import Test.Utils.Assert as Assert

spec :: Spec.Spec Unit
spec = do
  Spec.it "Hashes a spago.dhall file the same as openssl" do
    hash <- Sha256.hashFile hooksSpago
    hash `Assert.shouldEqual` hooksSpagoHash

  Spec.it "Hashes a LICENSE file the same as openssl" do
    hash <- Sha256.hashFile hooksLicense
    hash `Assert.shouldEqual` hooksLicenseHash

  Spec.it "spago.dhall hash matches the nix hash" do
    hash <- Sha256.hashFile hooksSpago
    nix <- Except.runExceptT $ sha256Nix hooksSpago
    hash `Assert.shouldEqualRight` nix

  Spec.it "LICENSE hash matches the nix hash" do
    hash <- Sha256.hashFile hooksLicense
    nix <- Except.runExceptT $ sha256Nix hooksLicense
    hash `Assert.shouldEqualRight` nix

  Spec.it "Round-trips spago.dhall file" do
    hooksSpagoHash `Assert.shouldEqualRight` Sha256.parse (Sha256.print hooksSpagoHash)

  Spec.it "Round-trips LICENSE file" do
    hooksLicenseHash `Assert.shouldEqualRight` Sha256.parse (Sha256.print hooksLicenseHash)

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/_fixtures/packages/halogen-hooks/spago.dhall | openssl base64 -A
hooksSpagoHash :: Sha256.Sha256
hooksSpagoHash = Utils.fromRight "Failed to parse Sha256" $ Sha256.parse "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="

hooksSpago :: FilePath
hooksSpago = Path.concat [ "test", "_fixtures", "packages", "halogen-hooks", "spago.dhall" ]

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/_fixtures/packages/halogen-hooks/LICENSE | openssl base64 -A
hooksLicenseHash :: Sha256.Sha256
hooksLicenseHash = Utils.fromRight "Failed to parse Sha256" $ Sha256.parse "sha256-wOzNcCq20TAL/LMT1lYIiaoEIFGDBw+yp14bj7qK9v4="

hooksLicense :: FilePath
hooksLicense = Path.concat [ "test", "_fixtures", "packages", "halogen-hooks", "LICENSE" ]

sha256Nix :: FilePath -> ExceptT String Aff Sha256.Sha256
sha256Nix path = ExceptT do
  -- In Nix 2.4 this will become `nix hash file`
  let args = [ "hash-file", "--sri", path ]
  result <- Sunde.spawn { cmd: "nix", args, stdin: Nothing } Process.defaultSpawnOptions
  pure $ case result.exit of
    Process.Normally 0 ->
      Sha256.parse $ String.trim result.stdout
    _ ->
      Left result.stderr
