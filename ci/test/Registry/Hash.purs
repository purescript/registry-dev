module Test.Registry.Hash where

import Registry.Prelude

import Control.Monad.Except as Except
import Node.ChildProcess as NodeProcess
import Node.Path as Node.Path
import Registry.Hash as Hash
import Sunde as Process
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Text.Parsing.StringParser as SP

testHash :: Spec.Spec Unit
testHash = do
  Spec.describe "Creates a valid sha256 SRI hash" do
    Spec.it "Hashes a text file the same as openssl" do
      hash <- Hash.sha256File hooksSpago
      hash `Assert.shouldEqual` hooksSpagoHash

    Spec.it "Hashes a tarball the same as openssl" do
      hash <- Hash.sha256File hooksTarball
      hash `Assert.shouldEqual` hooksTarballHash

    Spec.it "Text file matches the nix hash" do
      hash <- Hash.sha256File hooksSpago
      nix <- Except.runExceptT $ sha256Nix hooksSpago
      Right hash `Assert.shouldEqual` nix

    Spec.it "Tarball file matches the nix hash" do
      hash <- Hash.sha256File hooksTarball
      nix <- Except.runExceptT $ sha256Nix hooksTarball
      Right hash `Assert.shouldEqual` nix

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/fixtures/halogen-hooks/spago.dhall | openssl base64 -A
hooksSpagoHash :: Hash.Sha256
hooksSpagoHash = Hash.Sha256 "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="

hooksSpago :: FilePath
hooksSpago = Node.Path.concat [ "test", "fixtures", "halogen-hooks", "spago.dhall" ]

-- Test hash produced by `openssl`:
-- openssl dgst -sha256 -binary < test/fixtures/halogen-hooks-0.5.0.tar.gz | openssl base64 -A
hooksTarballHash :: Hash.Sha256
hooksTarballHash = Hash.Sha256 "sha256-3nz2p8KZYRMbHFTuSk4kCVO47k0rZqn3qbtes/ebp9M="

hooksTarball :: FilePath
hooksTarball = Node.Path.concat [ "test", "fixtures", "halogen-hooks-0.5.0.tar.gz" ]

sha256Nix :: FilePath -> ExceptT String Aff Hash.Sha256
sha256Nix path = ExceptT do
  let args = [ "hash-file", "--sri", path ]
  result <- Process.spawn { cmd: "nix", args, stdin: Nothing } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> do
      let hash = Hash.parseSha256 result.stdout
      pure $ lmap SP.printParserError hash
    _ ->
      pure $ Left result.stderr
