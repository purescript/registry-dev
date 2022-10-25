module Test.Registry.Hash where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.String as String
import Node.ChildProcess as NodeProcess
import Node.Path as Path
import Registry.Json as Json
import Registry.SRIHash as SRIHash
import Sunde as Process
import Test.Spec as Spec
import Test.Utils (shouldEqual)

testHash :: Spec.Spec Unit
testHash = do
  Spec.describe "Creates a valid sha256 SRI hash" do
    Spec.it "Hashes a spago.dhall file the same as openssl" do
      hash <- SRIHash.hashFile hooksSpago
      hash `shouldEqual` hooksSpagoHash

    Spec.it "Hashes a LICENSE file the same as openssl" do
      hash <- SRIHash.hashFile hooksLicense
      hash `shouldEqual` hooksLicenseHash

    Spec.it "spago.dhall hash matches the nix hash" do
      hash <- SRIHash.hashFile hooksSpago
      nix <- Except.runExceptT $ sha256Nix hooksSpago
      Right hash `shouldEqual` nix

    Spec.it "LICENSE hash matches the nix hash" do
      hash <- SRIHash.hashFile hooksLicense
      nix <- Except.runExceptT $ sha256Nix hooksLicense
      Right hash `shouldEqual` nix

  Spec.describe "Encodes and decodes from JSON" do
    Spec.it "Round-trips spago.dhall file" do
      Json.roundtrip hooksSpagoHash `shouldEqual` Right hooksSpagoHash

    Spec.it "Round-trips LICENSE file" do
      Json.roundtrip hooksLicenseHash `shouldEqual` Right hooksLicenseHash

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/fixtures/halogen-hooks/spago.dhall | openssl base64 -A
hooksSpagoHash :: SRIHash.SRIHash
hooksSpagoHash = unsafeFromRight $ SRIHash.parse "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="

hooksSpago :: FilePath
hooksSpago = Path.concat [ "test", "fixtures", "halogen-hooks", "spago.dhall" ]

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/fixtures/LICENSE | openssl base64 -A
hooksLicenseHash :: SRIHash.SRIHash
hooksLicenseHash = unsafeFromRight $ SRIHash.parse "sha256-wOzNcCq20TAL/LMT1lYIiaoEIFGDBw+yp14bj7qK9v4="

hooksLicense :: FilePath
hooksLicense = Path.concat [ "test", "fixtures", "halogen-hooks", "LICENSE" ]

sha256Nix :: FilePath -> ExceptT String Aff SRIHash.SRIHash
sha256Nix path = ExceptT do
  -- In Nix 2.4 this will become `nix hash file`
  let args = [ "hash-file", "--sri", path ]
  result <- Process.spawn { cmd: "nix", args, stdin: Nothing } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> do
      SRIHash.parse $ String.trim result.stdout
    _ ->
      Left result.stderr
