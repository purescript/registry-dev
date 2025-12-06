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
      wiremock-s3 = mkWiremockProcess "s3" ports.s3;
      wiremock-bucket = mkWiremockProcess "bucket" ports.bucket;
      wiremock-pursuit = mkWiremockProcess "pursuit" ports.pursuit;
      wiremock-healthchecks = mkWiremockProcess "healthchecks" ports.healthchecks;

      registry-server = {
        command = "${serverStartScript}/bin/start-server";
        depends_on = {
          wiremock-github.condition = "process_healthy";
          wiremock-s3.condition = "process_healthy";
          wiremock-bucket.condition = "process_healthy";
          wiremock-pursuit.condition = "process_healthy";
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

  testEnvScript = pkgs.writeShellScriptBin "test-env" ''
    set -e

    export SERVER_PORT="${toString ports.server}"

    if [ -z "''${STATE_DIR:-}" ]; then
      STATE_DIR="$(mktemp -d)"
      export STATE_DIR
      echo "Using temporary directory: $STATE_DIR"
      trap 'echo "Cleaning up $STATE_DIR..."; rm -rf "$STATE_DIR"' EXIT
    else
      export STATE_DIR
    fi

    mkdir -p "$STATE_DIR"

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
  # This avoids verbose paths like `testEnv.testConfig.wiremockStartScript`.
  inherit (testConfig)
    wiremockStartScript
    serverStartScript
    setupGitFixtures
    envVars
    envFile
    ;

  # Full testConfig still available for less common access patterns
  inherit testConfig;
}
