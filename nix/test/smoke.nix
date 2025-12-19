# VM-based smoke test for the registry server deployment.
#
# This test verifies that the registry server can be deployed to a NixOS VM
# and that the systemd services start correctly. It does NOT test the full
# API workflow (that's covered by the E2E tests in app-e2e).
#
# Use this to verify:
# - The NixOS module configuration is valid
# - systemd services start and stay running
# - The server responds to basic HTTP requests
# - Database migrations run successfully
# - The job executor starts without errors
{
  pkgs,
  lib,
  overlays,
  rootPath,
}:

if pkgs.stdenv.isDarwin then
  pkgs.runCommand "smoke-skip" { } ''
    echo "Smoke tests require Linux VMs, skipping on macOS" > $out
  ''
else
  let
    testConfig = import ./config.nix { inherit pkgs lib rootPath; };
    envVars = testConfig.testEnv;
    stateDir = "/var/lib/registry-server";
    repoFixturesDir = "${stateDir}/repo-fixtures";
  in
  pkgs.testers.nixosTest {
    name = "registry-smoke";

    testScript = ''
      import time

      # Start the registry VM
      registry.start()

      # Wait for the server systemd service to be active
      registry.wait_for_unit("server.service", timeout=60)

      # Verify the server is responding to HTTP requests
      registry.wait_until_succeeds(
          "curl --fail-with-body http://localhost:${envVars.SERVER_PORT}/api/v1/jobs",
          timeout=30
      )

      # Verify we get a valid JSON response (empty array for jobs)
      result = registry.succeed(
          "curl -s http://localhost:${envVars.SERVER_PORT}/api/v1/jobs"
      )
      assert result.strip() == "[]", f"Expected empty jobs array, got: {result}"

      # Verify the database was created and migrations ran
      registry.succeed("test -f ${stateDir}/db/registry.sqlite3")

      # Check that the service is still running (didn't crash)
      registry.succeed("systemctl is-active server.service")

      # Give the job executor a moment to start and potentially fail
      time.sleep(2)

      # Check that the job executor started successfully and didn't fail
      logs = registry.succeed("journalctl -u server.service --no-pager")
      assert "Job executor failed:" not in logs, f"Job executor failed on startup. Logs:\n{logs}"
      assert "Starting Job Executor" in logs, f"Job executor did not start. Logs:\n{logs}"

      print("✓ Smoke test passed: server deployed and responding")
    '';

    nodes.registry = {
      imports = [
        (rootPath + "/nix/registry-server.nix")
      ];

      # Apply the git mock overlay on top of the standard overlays
      nixpkgs.overlays = overlays ++ [ testConfig.gitMockOverlay ];

      virtualisation = {
        graphics = false;
        # Give the VM enough memory for the server
        memorySize = 2048;
      };

      # Set up git fixtures before the server starts
      systemd.services.setup-git-fixtures = {
        description = "Set up git fixtures for smoke test";
        wantedBy = [ "server.service" ];
        before = [ "server.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          ${testConfig.setupGitFixtures}/bin/setup-git-fixtures ${repoFixturesDir}
        '';
      };

      services.registry-server = {
        enable = true;
        host = "localhost";
        port = lib.toInt envVars.SERVER_PORT;
        enableCerts = false;
        inherit stateDir;
        envVars = envVars // {
          REPO_FIXTURES_DIR = repoFixturesDir;
        };
      };
    };
  }
