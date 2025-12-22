-- | Test fixtures for E2E tests.
-- | Contains package operation data used across multiple test suites.
module Registry.Test.E2E.Fixtures
  ( effectPublishData
  , failingPublishData
  , trusteeAuthenticatedData
  ) where

import Prelude

import Data.Codec.JSON as CJ
import Data.Maybe (Maybe(..))
import JSON as JSON
import Registry.Location as Location
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PublishData, UnpublishData)
import Registry.Operation as Operation
import Registry.SSH (Signature(..))
import Registry.Test.Utils as Utils

-- | Standard publish data for effect@4.0.0, used by E2E tests.
-- | This matches the fixtures in app/fixtures/github-packages/effect-4.0.0
effectPublishData :: PublishData
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

-- | Publish data for prelude@6.0.1, which already exists in metadata fixtures.
-- | Used to test failure scenarios (duplicate publish) in E2E tests.
failingPublishData :: PublishData
failingPublishData =
  { name: Utils.unsafePackageName "prelude"
  , location: Just $ Location.GitHub
      { owner: "purescript"
      , repo: "purescript-prelude"
      , subdir: Nothing
      }
  , ref: "v6.0.1"
  , compiler: Utils.unsafeVersion "0.15.9"
  , resolutions: Nothing
  , version: Utils.unsafeVersion "6.0.1"
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
