-- | Core types for E2E tests.
-- |
-- | This module defines the shared environment and monad types used by all
-- | E2E test helpers. It's kept separate to avoid circular dependencies
-- | between Env, Client, and WireMock modules.
module Test.E2E.Support.Types
  ( TestEnv
  , ClientConfig
  , WireMockConfig
  , E2E
  , E2ESpec
  ) where

import Registry.App.Prelude

import Control.Monad.Reader (ReaderT)
import Effect.Aff (Milliseconds)
import Test.Spec (SpecT)

-- | Configuration for the E2E test client
type ClientConfig =
  { baseUrl :: String
  , pollInterval :: Milliseconds
  , maxPollAttempts :: Int
  }

-- | Configuration for connecting to WireMock admin API
type WireMockConfig =
  { baseUrl :: String
  }

-- | The shared test environment available to all E2E helpers.
-- | Constructed once at startup from environment variables.
type TestEnv =
  { clientConfig :: ClientConfig
  , githubWireMock :: WireMockConfig
  , storageWireMock :: WireMockConfig
  , stateDir :: String
  , privateKey :: String
  }

-- | The base monad for E2E test helpers.
-- | All Client, Env, and WireMock functions operate in this monad.
type E2E = ReaderT TestEnv Aff

-- | The spec type for E2E tests.
-- | Test modules export `spec :: E2ESpec` instead of `spec :: Spec Unit`.
type E2ESpec = SpecT E2E Unit Identity Unit
