module Test.Registry.Hash where

import Registry.Prelude

import Control.Monad.Except as Except
import Node.ChildProcess as NodeProcess
import Node.Path as Node.Path
import Registry.Hash as Hash
import Registry.Json as Json
import Sunde as Process
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Text.Parsing.StringParser as SP

testHash :: Spec.Spec Unit
testHash = do
  Spec.describe "Creates a valid sha256 SRI hash" do
    Spec.it "Hashes a spago.dhall file the same as openssl" do
      hash <- Hash.sha256File hooksSpago
      hash `Assert.shouldEqual` hooksSpagoHash

    Spec.it "Hashes a LICENSE file the same as openssl" do
      hash <- Hash.sha256File hooksLicense
      hash `Assert.shouldEqual` hooksLicenseHash

    Spec.it "spago.dhall hash matches the nix hash" do
      hash <- Hash.sha256File hooksSpago
      nix <- Except.runExceptT $ sha256Nix hooksSpago
      Right hash `Assert.shouldEqual` nix

    Spec.it "LICENSE hash matches the nix hash" do
      hash <- Hash.sha256File hooksLicense
      nix <- Except.runExceptT $ sha256Nix hooksLicense
      Right hash `Assert.shouldEqual` nix

  Spec.describe "Encodes and decodes from JSON" do
    Spec.it "Round-trips spago.dhall file" do
      Json.roundtrip hooksSpagoHash `Assert.shouldContain` hooksSpagoHash

    Spec.it "Round-trips LICENSE file" do
      Json.roundtrip hooksLicenseHash `Assert.shouldContain` hooksLicenseHash

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/fixtures/halogen-hooks/spago.dhall | openssl base64 -A
hooksSpagoHash :: Hash.Sha256
hooksSpagoHash = Hash.unsafeSha256 "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="

hooksSpago :: FilePath
hooksSpago = Node.Path.concat [ "test", "fixtures", "halogen-hooks", "spago.dhall" ]

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/fixtures/LICENSE | openssl base64 -A
hooksLicenseHash :: Hash.Sha256
hooksLicenseHash = Hash.unsafeSha256 "sha256-wOzNcCq20TAL/LMT1lYIiaoEIFGDBw+yp14bj7qK9v4="

hooksLicense :: FilePath
hooksLicense = Node.Path.concat [ "test", "fixtures", "halogen-hooks", "LICENSE" ]

sha256Nix :: FilePath -> ExceptT String Aff Hash.Sha256
sha256Nix path = ExceptT do
  -- In Nix 2.4 this will become `nix hash file`
  let args = [ "hash-file", "--sri", path ]
  result <- Process.spawn { cmd: "nix", args, stdin: Nothing } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> do
      let hash = Hash.parseSha256 result.stdout
      pure $ lmap SP.printParserError hash
    _ ->
      pure $ Left result.stderr
