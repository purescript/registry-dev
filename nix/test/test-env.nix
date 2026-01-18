# Process-compose based test environment for the registry server.
# This module provides a process-compose configuration for running
# the registry server with mocked external services for local development
# and testing.
#
# Usage:
#   nix run .#test-env                    # Start with log streaming (default)
#   nix run .#test-env -- --tui           # Start with interactive TUI
#   nix run .#test-env -- --detached      # Run detached (background)
#
# All arguments are passed directly to process-compose. See `process-compose --help`.
#
# When detached, manage with:
#   process-compose -p 9006 attach        # Attach TUI to running instance
#   process-compose -p 9006 down          # Stop all services
#
# Environment variables:
#   STATE_DIR     - Override the state directory (default: temp dir, cleaned on exit)
#   SERVER_PORT   - Exported automatically for E2E tests to discover the server
{
  pkgs,
  lib,
  rootPath,
}:
let
  testConfig = import ./config.nix {
    inherit
      pkgs
      lib
      rootPath
      ;
  };

  inherit (testConfig)
    ports
    combinedWiremockRoot
    serverStartScript
    publishPayload
    ;

  mkWiremockProcess = name: port: {
    command = "${pkgs.wiremock}/bin/wiremock --port ${toString port} --root-dir ${combinedWiremockRoot}/${name} --disable-banner";
    readiness_probe = {
      http_get = {
        host = "127.0.0.1";
        inherit port;
        path = "/__admin";
      };
      initial_delay_seconds = 1;
      period_seconds = 1;
    };
    shutdown = {
      signal = 15;
      timeout_seconds = 5;
    };
  };

  processComposeConfig = {
    version = "0.5";
    processes = {
      wiremock-github = mkWiremockProcess "github" ports.github;
      # Unified storage WireMock instance for S3 + bucket + Pursuit with stateful scenarios
      wiremock-storage = mkWiremockProcess "storage" ports.storage;
      wiremock-healthchecks = mkWiremockProcess "healthchecks" ports.healthchecks;

      registry-server = {
        command = "${serverStartScript}/bin/start-server";
        depends_on = {
          wiremock-github.condition = "process_healthy";
          wiremock-storage.condition = "process_healthy";
          wiremock-healthchecks.condition = "process_healthy";
        };
        readiness_probe = {
          http_get = {
            host = "127.0.0.1";
            port = ports.server;
            path = "/api/v1/jobs";
          };
          initial_delay_seconds = 2;
          period_seconds = 1;
        };
        shutdown = {
          signal = 15;
          timeout_seconds = 5;
        };
      };
    };
  };

  processComposeYaml = pkgs.writeText "process-compose.yaml" (builtins.toJSON processComposeConfig);

  # The state directory is fixed (not configurable) to avoid mismatch between
  # the test-env and spago-test-e2e shells.
  stateDir = testConfig.testEnv.STATE_DIR;

  testEnvScript = pkgs.writeShellScriptBin "test-env" ''
    set -e

    # Clean up previous test state and create fresh directory
    rm -rf ${stateDir}
    mkdir -p ${stateDir}

    # Export all test environment variables, PATH, and GIT_BINARY
    ${testConfig.testRuntimeExports}

    exec ${pkgs.process-compose}/bin/process-compose up \
      -f ${processComposeYaml} \
      --ordered-shutdown \
      -t=false \
      "$@"
  '';

in
{
  inherit
    testEnvScript
    processComposeYaml
    publishPayload
    ports
    ;

  # Re-export commonly-used items from testConfig for convenience.
  # This avoids verbose paths like `testEnv.testConfig.testBuildInputs`.
  inherit (testConfig)
    testEnv
    testRuntimeInputs
    testRuntimeExports
    testBuildInputs
    wiremockStartScript
    serverStartScript
    setupGitFixtures
    ;

  # Full testConfig still available for less common access patterns
  inherit testConfig;
}
