-- | Test fixtures for E2E tests.
-- | Contains package operation data used across multiple test suites.
module Test.E2E.Support.Fixtures
  ( PackageFixture
  , effect
  , console
  , prelude
  , slug
  , unsafeCoerce
  , effectPublishData
  , effectPublishDataDifferentLocation
  , consolePublishData
  , slugPublishData
  , unsafeCoercePublishData
  , failingTransferData
  , nonexistentTransferData
  , trusteeAuthenticatedData
  , effectUnpublishData
  , effectTransferData
  , nonexistentUnpublishData
  , preludeUnpublishData
  , signUnpublish
  , signTransfer
  , packageSetAddRequest
  , packageSetCompilerChangeRequest
  , packageSetRemoveRequest
  , signPackageSet
  , invalidJsonIssueEvent
  ) where

import Registry.App.Prelude

import Data.Codec.JSON as CJ
import Data.Map as Map
import JSON as JSON
import Registry.Location (Location(..))
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PackageSetOperation(..), PackageSetUpdateRequest, TransferData, UnpublishData)
import Registry.Operation as Operation
import Registry.PackageName (PackageName)
import Registry.SSH as SSH
import Registry.Test.Utils as Utils
import Registry.Version (Version)

type PackageFixture = { name :: PackageName, version :: Version }

-- | effect@4.0.0 fixture package
effect :: PackageFixture
effect = { name: Utils.unsafePackageName "effect", version: Utils.unsafeVersion "4.0.0" }

-- | console@6.1.0 fixture package
console :: PackageFixture
console = { name: Utils.unsafePackageName "console", version: Utils.unsafeVersion "6.1.0" }

-- | prelude@6.0.1 fixture package
prelude :: PackageFixture
prelude = { name: Utils.unsafePackageName "prelude", version: Utils.unsafeVersion "6.0.1" }

-- | slug@3.0.0 fixture package (uses spago.dhall manifest format)
slug :: PackageFixture
slug = { name: Utils.unsafePackageName "slug", version: Utils.unsafeVersion "3.0.0" }

-- | Standard publish data for effect@4.0.0, used by E2E tests.
-- | This matches the fixtures in app/fixtures/github-packages/effect-4.0.0
effectPublishData :: Operation.PublishData
effectPublishData =
  { name: effect.name
  , location: Just $ GitHub
      { owner: "purescript"
      , repo: "purescript-effect"
      , subdir: Nothing
      }
  , ref: "v4.0.0"
  , compiler: Just $ Utils.unsafeVersion "0.15.10"
  , resolutions: Nothing
  , version: effect.version
  }

-- | Publish data for effect@99.0.0 with a DIFFERENT location.
-- | Uses a non-existent version to avoid duplicate job detection,
-- | but still targets an existing package to test location conflicts.
effectPublishDataDifferentLocation :: Operation.PublishData
effectPublishDataDifferentLocation =
  effectPublishData
    { location = Just $ GitHub
        { owner: "someone-else"
        , repo: "purescript-effect"
        , subdir: Nothing
        }
    , version = Utils.unsafeVersion "99.0.0"
    , ref = "v99.0.0"
    }

-- | Publish data for console@6.1.0, used for concurrency tests.
-- | Console depends on effect ^4.0.0 and prelude ^6.0.0.
-- | This matches the fixtures in app/fixtures/github-packages/console-6.1.0
consolePublishData :: Operation.PublishData
consolePublishData =
  { name: console.name
  , location: Just $ GitHub
      { owner: "purescript"
      , repo: "purescript-console"
      , subdir: Nothing
      }
  , ref: "v6.1.0"
  , compiler: Just $ Utils.unsafeVersion "0.15.10"
  , resolutions: Nothing
  , version: console.version
  }

-- | Publish data for slug@3.0.0, used for spago.dhall manifest format test.
-- | Slug depends on prelude only and uses spago.dhall as its manifest format.
-- | This matches the fixtures in app/fixtures/github-packages/slug-3.0.0
slugPublishData :: Operation.PublishData
slugPublishData =
  { name: slug.name
  , location: Just $ GitHub
      { owner: "purescript"
      , repo: "purescript-slug"
      , subdir: Nothing
      }
  , ref: "v3.0.0"
  , compiler: Just $ Utils.unsafeVersion "0.15.10"
  , resolutions: Nothing
  , version: slug.version
  }

-- | Publish data for unsafe-coerce@6.0.0, used by package set tests.
-- | Has no dependencies. Published first to create the tarball before adding to package set.
unsafeCoercePublishData :: Operation.PublishData
unsafeCoercePublishData =
  { name: unsafeCoerce.name
  , location: Just $ GitHub
      { owner: "purescript"
      , repo: "purescript-unsafe-coerce"
      , subdir: Nothing
      }
  , ref: "v6.0.0"
  , compiler: Just $ Utils.unsafeVersion "0.15.10"
  , resolutions: Nothing
  , version: unsafeCoerce.version
  }

-- | Unpublish data for effect@4.0.0, used for publish-then-unpublish tests.
effectUnpublishData :: UnpublishData
effectUnpublishData =
  { name: effect.name
  , version: effect.version
  , reason: "Testing unpublish flow"
  }

-- | Transfer data for effect, used for transfer tests.
-- | Transfers effect to a different GitHub owner.
effectTransferData :: TransferData
effectTransferData =
  { name: effect.name
  , newLocation: GitHub
      { owner: "new-owner"
      , repo: "purescript-effect"
      , subdir: Nothing
      }
  }

-- | Unpublish data for a nonexistent package.
-- | Used to test error handling when unpublishing an unknown package.
nonexistentUnpublishData :: UnpublishData
nonexistentUnpublishData =
  { name: Utils.unsafePackageName "nonexistent-package"
  , version: Utils.unsafeVersion "1.0.0"
  , reason: "Testing error handling for unknown package"
  }

-- | Unpublish data for prelude@6.0.1.
-- | This package was published long ago (in fixtures), so it should fail
-- | the 48-hour time limit check.
preludeUnpublishData :: UnpublishData
preludeUnpublishData =
  { name: prelude.name
  , version: prelude.version
  , reason: "Testing 48-hour limit enforcement"
  }

-- | Sign an unpublish operation using the given private key.
-- | The private key should be the base64-decoded PACCHETTIBOTTI_ED25519 env var.
signUnpublish :: String -> UnpublishData -> Either String AuthenticatedData
signUnpublish privateKey unpublishData = do
  let rawPayload = JSON.print $ CJ.encode Operation.unpublishCodec unpublishData
  private <- SSH.parsePrivateKey { key: privateKey, passphrase: Nothing }
    # lmap SSH.printPrivateKeyParseError
  let signature = SSH.sign private rawPayload
  pure
    { payload: Unpublish unpublishData
    , rawPayload
    , signature
    }

-- | Authenticated transfer data for prelude, which has no owners in fixtures.
-- | Used to test failure scenarios in E2E tests - will fail because no owners
-- | are listed to verify the signature against.
failingTransferData :: AuthenticatedData
failingTransferData = do
  let
    transferPayload :: TransferData
    transferPayload =
      { name: prelude.name
      , newLocation: GitHub
          { owner: "someone-else"
          , repo: "purescript-prelude"
          , subdir: Nothing
          }
      }

    rawPayload :: String
    rawPayload = JSON.print $ CJ.encode Operation.transferCodec transferPayload

  { payload: Transfer transferPayload
  , rawPayload
  , signature: SSH.Signature "invalid-signature-for-testing"
  }

-- | Authenticated data with an intentionally invalid signature.
-- | When submitted by a trustee (packaging-team-user), pacchettibotti will re-sign it.
-- | If re-signing works, the job succeeds; if not, signature verification fails.
-- | Uses prelude@6.0.1 which exists in app/fixtures/registry/metadata/prelude.json.
trusteeAuthenticatedData :: AuthenticatedData
trusteeAuthenticatedData = do
  let
    unpublishPayload :: UnpublishData
    unpublishPayload =
      { name: prelude.name
      , version: prelude.version
      , reason: "Testing trustee re-signing"
      }
    rawPayload = JSON.print $ CJ.encode Operation.unpublishCodec unpublishPayload

  { payload: Unpublish unpublishPayload
  , rawPayload
  , signature: SSH.Signature "invalid-signature-for-testing"
  }

-- | Transfer data for a nonexistent package.
-- | Used to test error handling when transferring an unknown package.
-- | Job should fail with "has not been published before" error.
nonexistentTransferData :: TransferData
nonexistentTransferData =
  { name: Utils.unsafePackageName "nonexistent-package"
  , newLocation: GitHub
      { owner: "someone"
      , repo: "purescript-nonexistent"
      , subdir: Nothing
      }
  }

-- | Sign a transfer operation using the given private key.
-- | The private key should be the base64-decoded PACCHETTIBOTTI_ED25519 env var.
signTransfer :: String -> TransferData -> Either String AuthenticatedData
signTransfer privateKey transferData = do
  let rawPayload = JSON.print $ CJ.encode Operation.transferCodec transferData
  private <- lmap SSH.printPrivateKeyParseError $ SSH.parsePrivateKey { key: privateKey, passphrase: Nothing }
  let signature = SSH.sign private rawPayload
  pure
    { payload: Transfer transferData
    , rawPayload
    , signature
    }

-- | unsafe-coerce@6.0.0 fixture package (exists in registry-index but not in package set)
unsafeCoerce :: PackageFixture
unsafeCoerce = { name: Utils.unsafePackageName "unsafe-coerce", version: Utils.unsafeVersion "6.0.0" }

-- | Package set request to add unsafe-coerce@6.0.0.
-- | This is an unauthenticated request (no signature) since adding packages
-- | doesn't require trustee authentication.
packageSetAddRequest :: PackageSetUpdateRequest
packageSetAddRequest =
  let
    payload = PackageSetUpdate
      { compiler: Nothing
      , packages: Map.singleton unsafeCoerce.name (Just unsafeCoerce.version)
      }
    rawPayload = JSON.print $ CJ.encode Operation.packageSetOperationCodec payload
  in
    { payload, rawPayload, signature: Nothing }

-- | Package set request to change the compiler version.
-- | This requires authentication (pacchettibotti signature) since changing
-- | the compiler is a restricted operation.
packageSetCompilerChangeRequest :: PackageSetUpdateRequest
packageSetCompilerChangeRequest =
  let
    payload = PackageSetUpdate
      { compiler: Just (Utils.unsafeVersion "0.15.11")
      , packages: Map.empty
      }
    rawPayload = JSON.print $ CJ.encode Operation.packageSetOperationCodec payload
  in
    { payload, rawPayload, signature: Nothing }

-- | Package set request to remove a package.
-- | This requires authentication (pacchettibotti signature) since removing
-- | packages is a restricted operation.
packageSetRemoveRequest :: PackageSetUpdateRequest
packageSetRemoveRequest =
  let
    payload = PackageSetUpdate
      { compiler: Nothing
      , packages: Map.singleton effect.name Nothing
      }
    rawPayload = JSON.print $ CJ.encode Operation.packageSetOperationCodec payload
  in
    { payload, rawPayload, signature: Nothing }

-- | Sign a package set update request using the given private key.
-- | The private key should be the base64-decoded PACCHETTIBOTTI_ED25519 env var.
signPackageSet :: String -> PackageSetUpdateRequest -> Either String PackageSetUpdateRequest
signPackageSet privateKey request = do
  private <- SSH.parsePrivateKey { key: privateKey, passphrase: Nothing }
    # lmap SSH.printPrivateKeyParseError
  let signature = SSH.sign private request.rawPayload
  pure request { signature = Just signature }

-- | GitHub issue event with invalid JSON in the body.
-- | Used to test that malformed JSON is handled gracefully with an error comment.
-- | Note: The inner JSON has a trailing comma (`"v1.0.0",}`) which is intentionally
-- | malformed to trigger a parse error.
invalidJsonIssueEvent :: String
invalidJsonIssueEvent =
  """{"sender":{"login":"packaging-team-user"},"issue":{"number":101,"body":"```json\n{\"name\": \"effect\", \"ref\": \"v1.0.0\",}\n```"}}"""
