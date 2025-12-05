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
  in
  pkgs.testers.nixosTest {
    name = "registry-smoke";

    testScript = ''
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

      print("✓ Smoke test passed: server deployed and responding")
    '';

    nodes.registry = {
      imports = [
        (rootPath + "/nix/registry-server.nix")
      ];

      nixpkgs.overlays = overlays;

      virtualisation = {
        graphics = false;
        # Give the VM enough memory for the server
        memorySize = 2048;
      };

      services.registry-server = {
        enable = true;
        host = "localhost";
        port = lib.toInt envVars.SERVER_PORT;
        enableCerts = false;
        inherit stateDir envVars;
      };
    };
  }
