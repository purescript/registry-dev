module Test.Registry.App.Auth (spec) where

import Registry.App.Prelude

import Data.Newtype (over)
import Data.String as String
import Registry.App.Auth as Auth
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..))
import Registry.PackageName as PackageName
import Registry.SSH (Signature(..))
import Registry.Test.Assert as Assert
import Registry.Version as Version
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Verifies correct payloads" do
    Auth.verifyPayload pacchettibotti [ validOwner ] validPayload >>= case _ of
      Left err -> Assert.fail err
      Right _ -> mempty

  Spec.it "Fails to verify when public key is incorrect" do
    let badPublicKey = over Owner (_ { public = (un Owner pacchettibotti).public }) validOwner
    Auth.verifyPayload pacchettibotti [ badPublicKey ] validPayload >>= case _ of
      Left err -> verifyError err "Malformed OpenSSH public key"
      Right _ -> Assert.fail "Verified an invalid public key."

  Spec.it "Fails to verify when signature is incorrect" do
    let
      badSignature = Signature ""
      badSignatureData = validPayload { signature = badSignature }

    Auth.verifyPayload pacchettibotti [ validOwner ] badSignatureData >>= case _ of
      Left err -> verifyError err "None of the owner keys are suitable to verify the payload."
      Right _ -> Assert.fail "Verified an incorrect SSH signature for the payload."

  Spec.it "Fails to verify when payload is incorrect" do
    let badRawPayload = validPayload { rawPayload = "{}" }
    Auth.verifyPayload pacchettibotti [ validOwner ] badRawPayload >>= case _ of
      Left err -> verifyError err "None of the owner keys are suitable to verify the payload."
      Right _ -> Assert.fail "Verified with a modified payload."

  Spec.it "Signs and verifies payload" do
    case Auth.signPayload { rawPayload, privateKey: String.joinWith "\n" privateKey } of
      Left err -> Assert.fail err
      Right signature -> Assert.shouldEqual signature rawPayloadSignature
  where
  verifyError err intended =
    unless (err == intended) do
      Assert.fail $ String.joinWith "\n"
        [ "Received error message:"
        , "  " <> err
        , "  but expected error message:"
        , "  " <> intended
        ]

pacchettibotti :: Owner
pacchettibotti = Owner
  { id: Just "pacchettibotti@purescript.org"
  , keytype: "ssh-rsa"
  , public: "AAAAB3NzaC1yc2EAAAADAQABAAACAQDOHWfcD2vlrcaEngneZ2TlHjnjLKoQCuy9R95F1qrfRIE0N6xH7eHMuJGFIvqeuKivSXWLUKQslf2XIA7n0PEX0vmzzM7JZNvOkIFoOinBfCKqAx1dIle7yYPAUZPrzBidyLv+4aCJ+zu819yHA5tfoZB87+N0QAZcEptYw3taWHZGTZdNpgIgcpDGEnUihuQ9eYbdePokWbDsSgBT7AMjpAPTN5Yvg27jNNm6/WdooY7O9tP4Xdheb7GUIabKeNDX4sK0hBWVcS4SVTMVV8ifflKWXboJqIhXvUHcn1Te4o193aC+VgDFyzIAhPiZjfI/Fnha9XVPOXZMotIkJ0xiH7jzFljYshExCiecEDbLnV67Z/CEzVmw7kYTYs2+fpJ6cnJGHWIfaU2cz3C+empn3kZ6So+CM/8oHVpt0UjhTuqIav29OlouAxcv8eLPHPmTINyiaZ7b+slZBsNcUgW5r54sJvsXyzx9DQN+jkfwtucxN3JQcnIhZcYvD9KSZtHRtz9iLOYLL6Pimbb4K9l98+Br4G40Mjby3ElsAwtPGOLdimyZAD2t2eyDu64kOm2zS9jS6JJ9/8uKxlSyenUBxQ+ITwn1enEd4qq1qpnUT/F7PKqv9SSn6UBoMXq+uTFa5rXevOMhl7whAZjttyFokYQsacy6kbvlLMOWcsLqMw=="
  }

validOwner :: Owner
validOwner = Owner
  { id: Nothing
  , keytype
  , public: publicKey
  }

validPayload :: AuthenticatedData
validPayload =
  { signature: rawPayloadSignature
  , payload: Unpublish
      { name
      , version
      , reason: "Committed bad credentials"
      }
  , rawPayload
  }

name :: PackageName
name = unsafeFromRight $ PackageName.parse "foo"

version :: Version
version = unsafeFromRight $ Version.parse "1.2.3"

keytype :: String
keytype = "ssh-ed25519"

publicKey :: String
publicKey = "AAAAC3NzaC1lZDI1NTE5AAAAIF6B6R9yp8yFM3wYcOhGO0EyZAefKWkvsTET+KeaegY/"

rawPayload :: String
rawPayload = """{ "packageName": "foo", "unpublishVersion": "1.2.3", "unpublishReason": "Committed bad credentials" }"""

rawPayloadSignature :: Signature
rawPayloadSignature = Signature "353588b6b6fe475c2be4afd72e4420acdfa66226234a0a09948cffa451e6554c5bced1e2f3e3aac668cb382317a939dde6348d33491ce31eca537011f6ab9e04"

privateKey :: Array String
privateKey =
  [ "-----BEGIN OPENSSH PRIVATE KEY-----"
  , "b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW"
  , "QyNTUxOQAAACBegekfcqfMhTN8GHDoRjtBMmQHnylpL7ExE/inmnoGPwAAAJBFbMLrRWzC"
  , "6wAAAAtzc2gtZWQyNTUxOQAAACBegekfcqfMhTN8GHDoRjtBMmQHnylpL7ExE/inmnoGPw"
  , "AAAEByKyrnWOpHm54P8vEjlePyM+NKnEJGx74p3YTVd1V10l6B6R9yp8yFM3wYcOhGO0Ey"
  , "ZAefKWkvsTET+KeaegY/AAAACW93bmVyQGZvbwECAwQ="
  , "-----END OPENSSH PRIVATE KEY-----"
  ]
