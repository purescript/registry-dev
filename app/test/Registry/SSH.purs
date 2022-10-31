module Test.Registry.SSH (spec) where

import Registry.Prelude

import Data.Array.NonEmpty as NEA
import Data.Newtype (over)
import Data.String as String
import Registry.Operation (AuthenticatedData(..), AuthenticatedOperation(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.SSH as SSH
import Registry.Schema (Owner(..))
import Registry.Version (Version)
import Registry.Version as Version
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Verifies correct payloads" do
    SSH.verifyPayload (NEA.singleton validOwner) validPayload >>= case _ of
      Left err -> Assert.fail err
      Right _ -> mempty

  Spec.it "Fails to verify when public key is incorrect" do
    let badPublicKey = over Owner (_ { public = "" }) validOwner
    SSH.verifyPayload (NEA.singleton badPublicKey) validPayload >>= case _ of
      Left err -> verifyError err "allowed_signers:1: invalid key"
      Right _ -> Assert.fail "Verified an invalid public key."

  Spec.it "Fails to verify when signature is incorrect" do
    let
      badSignature =
        [ "-----BEGIN SSH SIGNATURE-----"
        , "U1NIU0lHAAAAAQAAADMAAAALc3NoLWVkMjU1MTkAAAAgEAD8tAhhDMvn3Ydoy40uk7Ga7s"
        , "rV9CGhaPk8UG3B5NAAAAAEZmlsZQAAAAAAAAAGc2hhNTEyAAAAUwAAAAtzc2gtZWQyNTUx"
        , "OQAAAECB+9OSWkoZ5rHVg22oB8Ks56Jg3einV9tRw+6ypes1ZsmwdvJB2XjuvGPZG0iXdb"
        , "/9KJFfXFSvUgP5PpuaAfkM"
        , "-----END SSH SIGNATURE-----"
        ]

      badSignatureData = over AuthenticatedData (_ { signature = badSignature }) validPayload

    SSH.verifyPayload (NEA.singleton validOwner) badSignatureData >>= case _ of
      Left err -> verifyError err "Signature verification failed: incorrect signature"
      Right _ -> Assert.fail "Verified an incorrect SSH signature for the payload."

  Spec.it "Fails to verify when email is incorrect in authenticated data" do
    let badEmailData = over AuthenticatedData (_ { email = "test@bar" }) validPayload
    SSH.verifyPayload (NEA.singleton validOwner) badEmailData >>= case _ of
      Left err -> verifyError err ""
      Right _ -> Assert.fail "Verified with an incorrect email address."

  Spec.it "Fails to verify when email is incorrect in owner field" do
    let badEmailOwner = over Owner (_ { email = "test@bar" }) validOwner
    SSH.verifyPayload (NEA.singleton badEmailOwner) validPayload >>= case _ of
      Left err -> verifyError err ""
      Right _ -> Assert.fail "Verified with an incorrect email address."

  Spec.it "Fails to verify when payload is incorrect" do
    let badRawPayload = over AuthenticatedData (_ { rawPayload = "{}" }) validPayload
    SSH.verifyPayload (NEA.singleton validOwner) badRawPayload >>= case _ of
      Left err -> verifyError err "Signature verification failed: incorrect signature"
      Right _ -> Assert.fail "Verified with a modified payload."

  Spec.it "Signs and verifies payload" do
    SSH.signPayload { publicKey, rawPayload, privateKey: String.joinWith "\n" privateKey } >>= case _ of
      Left err -> Assert.fail $ "Failed to sign raw payload: " <> err
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

validOwner :: Owner
validOwner = Owner
  { email
  , keytype
  , public: publicKey
  }

validPayload :: AuthenticatedData
validPayload = AuthenticatedData
  { email
  , signature: rawPayloadSignature
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

email :: String
email = "test@foo"

keytype :: String
keytype = "ssh-ed25519"

publicKey :: String
publicKey = "AAAAC3NzaC1lZDI1NTE5AAAAIF6B6R9yp8yFM3wYcOhGO0EyZAefKWkvsTET+KeaegY/"

rawPayload :: String
rawPayload = """{ "packageName": "foo", "unpublishVersion": "1.2.3", "unpublishReason": "Committed bad credentials" }"""

rawPayloadSignature :: Array String
rawPayloadSignature =
  [ "-----BEGIN SSH SIGNATURE-----"
  , "U1NIU0lHAAAAAQAAADMAAAALc3NoLWVkMjU1MTkAAAAgXoHpH3KnzIUzfBhw6EY7QTJkB5"
  , "8paS+xMRP4p5p6Bj8AAAAEZmlsZQAAAAAAAAAGc2hhNTEyAAAAUwAAAAtzc2gtZWQyNTUx"
  , "OQAAAED8x4+yJphe3ELjnrVZLmPct/4w0R1KBryG2NnnRLY9sjQlrjMYGp+Gx6X0PQ7ylq"
  , "xqhPdvut0pccR4WiMDjeIC"
  , "-----END SSH SIGNATURE-----"
  ]

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
