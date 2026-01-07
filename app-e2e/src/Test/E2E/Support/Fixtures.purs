-- | Test fixtures for E2E tests.
-- | Contains package operation data used across multiple test suites.
module Test.E2E.Support.Fixtures
  ( PackageFixture
  , effect
  , console
  , prelude
  , effectPublishData
  , effectPublishDataDifferentLocation
  , consolePublishData
  , failingTransferData
  , nonexistentTransferData
  , trusteeAuthenticatedData
  , effectUnpublishData
  , effectTransferData
  , nonexistentUnpublishData
  , preludeUnpublishData
  , signUnpublish
  , signTransfer
  , invalidJsonIssueEvent
  ) where

import Registry.App.Prelude

import Data.Codec.JSON as CJ
import JSON as JSON
import Registry.Location (Location(..))
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), TransferData, UnpublishData)
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
  , compiler: Utils.unsafeVersion "0.15.10"
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
  , compiler: Utils.unsafeVersion "0.15.10"
  , resolutions: Nothing
  , version: console.version
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

-- | GitHub issue event with invalid JSON in the body.
-- | Used to test that malformed JSON is handled gracefully with an error comment.
-- | Note: The inner JSON has a trailing comma (`"v1.0.0",}`) which is intentionally
-- | malformed to trigger a parse error.
invalidJsonIssueEvent :: String
invalidJsonIssueEvent =
  """{"sender":{"login":"packaging-team-user"},"issue":{"number":101,"body":"```json\n{\"name\": \"effect\", \"ref\": \"v1.0.0\",}\n```"}}"""
