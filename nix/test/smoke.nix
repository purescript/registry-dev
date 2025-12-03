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
  overlays,
  rootPath,
}:

if pkgs.stdenv.isDarwin then
  pkgs.runCommand "smoke-skip" { } ''
    echo "Smoke tests require Linux VMs, skipping on macOS" > $out
  ''
else
  let
    stateDir = "/var/lib/registry-server";

    # Smoke test needs placeholder secrets to start the server.
    # Production URLs come from PureScript Constants, not env vars.
    # HEALTHCHECKS_URL is optional - not setting it disables healthcheck pinging.
    envVars = {
      # Required secrets - use placeholders for smoke test
      GITHUB_TOKEN = "ghp_test_placeholder_token_not_real";
      PACCHETTIBOTTI_TOKEN = "ghp_test_placeholder_token_not_real";
      SPACES_KEY = "test_spaces_key_placeholder";
      SPACES_SECRET = "test_spaces_secret_placeholder";
      # Base64-encoded placeholder SSH keys (valid format but not real keys)
      PACCHETTIBOTTI_ED25519 = "dGVzdC1wbGFjZWhvbGRlci1wcml2YXRlLWtleS1mb3ItdGVzdGluZy1vbmx5Cg==";
      PACCHETTIBOTTI_ED25519_PUB = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSVBsYWNlaG9sZGVyS2V5Rm9yVGVzdGluZ09ubHkwMDAwMDAwMDAwMDAgcGFjY2hldHRpYm90dGlAcHVyZXNjcmlwdC5vcmcK";
    };

    # Port 8080 matches the registry-server.nix NixOS module default.
    # This tests the production deployment configuration.
    # (config.nix uses port 9000 for test-env to avoid conflicts during development)
    serverPort = 8080;
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
          "curl --fail-with-body http://localhost:${toString serverPort}/api/v1/jobs",
          timeout=30
      )

      # Verify we get a valid JSON response (empty array for jobs)
      result = registry.succeed(
          "curl -s http://localhost:${toString serverPort}/api/v1/jobs"
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
        port = serverPort;
        enableCerts = false;
        inherit stateDir envVars;
      };
    };
  }
