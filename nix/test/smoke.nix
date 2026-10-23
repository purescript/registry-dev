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
# - Startup matrix jobs complete successfully
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
    dhallTypes = "${pkgs.registry-server}/bin/types";
    mockUnits = lib.mapAttrsToList (name: _: "wiremock-${name}.service") testConfig.wiremockConfigs;
  in
  pkgs.testers.nixosTest {
    name = "registry-smoke";

    testScript = ''
      import json
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

      # The fixture Prelude lacks the newest test compiler (0.15.11), so startup
      # must enqueue both dependency-free packages and process their dependants.
      # Include completed jobs: otherwise a failed job disappears from the list.
      expected = {("prelude", "6.0.1", "0.15.11"), ("type-equality", "4.0.1", "0.15.11")}
      deadline = time.monotonic() + 120
      while True:
          jobs = json.loads(registry.succeed(
              "curl --fail --silent http://localhost:${envVars.SERVER_PORT}/api/v1/jobs?include_completed=true"
          ))
          assert isinstance(jobs, list), f"Expected jobs array, got: {jobs}"
          failed = [job for job in jobs if job.get("finishedAt") and not job["success"]]
          assert not failed, f"Startup jobs failed: {json.dumps(failed, indent=2)}"
          matrix = {
              (job["packageName"], job["packageVersion"], job["compilerVersion"])
              for job in jobs if job["jobType"] == "matrix"
          }
          if expected <= matrix and all(job.get("finishedAt") and job["success"] for job in jobs):
              break
          assert time.monotonic() < deadline, f"Startup jobs did not complete: {json.dumps(jobs, indent=2)}"
          time.sleep(1)
      print(f"✓ All {len(jobs)} startup jobs succeeded: {json.dumps(jobs)}")

      # Verify the database was created and migrations ran
      registry.succeed("test -f ${stateDir}/db/registry.sqlite3")

      # Check that the service is still running (didn't crash)
      registry.succeed("systemctl is-active server.service")

      # A stale state-dir value must not override deployment-managed schema paths.
      pid = registry.succeed("systemctl show --value --property MainPID server.service").strip()
      registry.succeed(
          f"tr '\\0' '\\n' < /proc/{pid}/environ | grep -Fx 'DHALL_TYPES=${dhallTypes}'"
      )
      registry.succeed("test -f ${dhallTypes}/v1/Manifest.dhall")

      # Check that the job executor started successfully and didn't fail
      logs = registry.succeed("journalctl -u server.service --no-pager")
      assert "Job executor failed:" not in logs, f"Job executor failed on startup. Logs:\n{logs}"
      assert "Starting Job Executor" in logs, f"Job executor did not start. Logs:\n{logs}"

      print("✓ Smoke test passed: server deployed and responding")
    '';

    nodes.registry = {
      imports = [
        (rootPath + "/nix/registry-server.nix")
        {
          systemd.services = lib.mapAttrs' (
            name: cfg:
            lib.nameValuePair "wiremock-${name}" {
              serviceConfig.ExecStart = "${pkgs.wiremock}/bin/wiremock --port ${toString cfg.port} --root-dir ${cfg.rootDir} --disable-banner";
              postStart = ''
                until ${pkgs.curl}/bin/curl --fail --silent http://localhost:${toString cfg.port}/__admin > /dev/null; do
                  sleep 0.5
                done
              '';
            }
          ) testConfig.wiremockConfigs;
        }
      ];

      # The S3 SDK uses virtual-hosted bucket URLs, as in the integration test.
      networking.hosts."127.0.0.1" = [ "purescript-registry.localhost" ];
      systemd.services.server = {
        requires = mockUnits;
        after = mockUnits;
      };

      # Apply the git mock overlay on top of the standard overlays
      nixpkgs.overlays = overlays ++ [ testConfig.testOverlay ];

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
          mkdir -p ${stateDir}
          echo 'DHALL_TYPES=/defunct/types' > ${stateDir}/.env
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
          DHALL_TYPES = dhallTypes;
        };
      };
    };
  }
