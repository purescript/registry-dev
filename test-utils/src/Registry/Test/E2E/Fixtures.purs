-- | Test fixtures for E2E tests.
-- | Contains package operation data used across multiple test suites.
module Registry.Test.E2E.Fixtures
  ( effectPublishData
  , failingTransferData
  , trusteeAuthenticatedData
  ) where

import Prelude

import Data.Codec.JSON as CJ
import Data.Maybe (Maybe(..))
import JSON as JSON
import Registry.Location as Location
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), TransferData, UnpublishData)
import Registry.Operation as Operation
import Registry.SSH (Signature(..))
import Registry.Test.Utils as Utils

-- | Standard publish data for effect@4.0.0, used by E2E tests.
-- | This matches the fixtures in app/fixtures/github-packages/effect-4.0.0
effectPublishData :: Operation.PublishData
effectPublishData =
  { name: Utils.unsafePackageName "effect"
  , location: Just $ Location.GitHub
      { owner: "purescript"
      , repo: "purescript-effect"
      , subdir: Nothing
      }
  , ref: "v4.0.0"
  , compiler: Utils.unsafeVersion "0.15.9"
  , resolutions: Nothing
  , version: Utils.unsafeVersion "4.0.0"
  }

-- | Authenticated transfer data for prelude, which has no owners in fixtures.
-- | Used to test failure scenarios in E2E tests - will fail because no owners
-- | are listed to verify the signature against.
failingTransferData :: AuthenticatedData
failingTransferData =
  let
    transferPayload :: TransferData
    transferPayload =
      { name: Utils.unsafePackageName "prelude"
      , newLocation: Location.GitHub
          { owner: "someone-else"
          , repo: "purescript-prelude"
          , subdir: Nothing
          }
      }
    rawPayload = JSON.print $ CJ.encode Operation.transferCodec transferPayload
  in
    { payload: Transfer transferPayload
    , rawPayload
    , signature: Signature "invalid-signature-for-testing"
    }

-- | Authenticated data with an intentionally invalid signature.
-- | When submitted by a trustee (packaging-team-user), pacchettibotti will re-sign it.
-- | If re-signing works, the job succeeds; if not, signature verification fails.
-- | Uses prelude@6.0.1 which exists in app/fixtures/registry/metadata/prelude.json.
trusteeAuthenticatedData :: AuthenticatedData
trusteeAuthenticatedData =
  let
    unpublishPayload :: UnpublishData
    unpublishPayload =
      { name: Utils.unsafePackageName "prelude"
      , version: Utils.unsafeVersion "6.0.1"
      , reason: "Testing trustee re-signing"
      }
    rawPayload = JSON.print $ CJ.encode Operation.unpublishCodec unpublishPayload
  in
    { payload: Unpublish unpublishPayload
    , rawPayload
    , signature: Signature "invalid-signature-for-testing"
    }
