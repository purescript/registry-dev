{
  pkgs,
  spagoSrc,
  # Test support module from test-env.nix. Named 'testSupport' to avoid confusion
  # with testSupport.testEnv (the environment variables attribute set).
  testSupport,
}:
if pkgs.stdenv.isDarwin then
  pkgs.runCommand "integration-skip" { } ''
    echo "Integration tests require nss_wrapper (Linux only)." > $out
    echo "Use 'nix run .#test-env' for local testing on macOS." >> $out
  ''
else
  let
    e2eTestRunner = pkgs.stdenv.mkDerivation {
      name = "registry-e2e-tests";
      src = spagoSrc;
      nativeBuildInputs = [ pkgs.esbuild ];
      buildPhase = ''
        ln -s ${pkgs.registry-package-lock}/node_modules .
        cp -r ${pkgs.registry-spago-lock}/output .
        cat > entrypoint.js << 'EOF'
        import { main } from "./output/Test.E2E.Main/index.js";
        main();
        EOF
        esbuild entrypoint.js --bundle --outfile=e2e-tests.js --platform=node --packages=external
        cat > healthcheck-test.js << 'EOF'
        import assert from "node:assert/strict";
        import { createServer } from "node:http";
        import { report } from "./output/Registry.App.Server.Healthcheck/index.js";
        import { runAff_ } from "./output/Effect.Aff/index.js";
        import { Left, Right } from "./output/Data.Either/index.js";

        const execute = aff => new Promise((resolve, reject) =>
          runAff_(result => () => result instanceof Left ? reject(result.value0) : resolve(result.value0))(aff)());
        async function main() {
          let received, hang = false;
          const server = createServer(async (req, res) => {
            let requestBody = "";
            for await (const chunk of req) requestBody += chunk;
            received = { method: req.method, url: req.url, body: requestBody };
            if (!hang) res.end("OK");
          });
          await new Promise(resolve => server.listen(0, "127.0.0.1", resolve));
          const url = `http://127.0.0.1:''${server.address().port}/check`;
          try {
            await execute(report(url)(new Left("Executor paused after repeated job resets")));
            assert.equal(received.method, "POST");
            assert.equal(received.url, "/check/fail");
            assert.match(received.body, /paused after repeated job resets/);
            hang = true;
            // The outer deadline makes a broken timeout fail rather than hang CI.
            await assert.rejects(Promise.race([
              execute(report(url)(new Right(undefined))),
              new Promise((_, reject) => setTimeout(() => reject(new Error("outer deadline")), 15000).unref())
            ]), /timed out/);
            console.log("Healthchecks reporting tests passed");
          } finally {
            server.closeAllConnections();
            await new Promise(resolve => server.close(resolve));
          }
        }
        main().catch(error => { console.error(error); process.exit(1); });
        EOF
        esbuild healthcheck-test.js --bundle --outfile=healthcheck-tests.js --platform=node --packages=external
      '';
      installPhase = ''
        mkdir -p $out
        cp e2e-tests.js healthcheck-tests.js $out/
      '';
    };

    ports = testSupport.ports;
  in
  pkgs.runCommand "e2e-integration"
    {
      nativeBuildInputs = [
        pkgs.nodejs
        pkgs.curl
        pkgs.jq
        pkgs.sqlite
        pkgs.nss_wrapper
      ]
      ++ testSupport.testBuildInputs;
      NODE_PATH = "${pkgs.registry-package-lock}/node_modules";
      # Use nss_wrapper to resolve S3 bucket subdomain in the Nix sandbox.
      # The AWS SDK uses virtual-hosted style URLs (bucket.endpoint/key), so
      # purescript-registry.localhost must resolve to 127.0.0.1.
      NSS_WRAPPER_HOSTS = pkgs.writeText "hosts" ''
        127.0.0.1 localhost
        127.0.0.1 purescript-registry.localhost
      '';
      LD_PRELOAD = "${pkgs.nss_wrapper}/lib/libnss_wrapper.so";
    }
    ''
      set -e
      export HOME=$TMPDIR
      export STATE_DIR=$TMPDIR/state
      export REPO_FIXTURES_DIR="$STATE_DIR/repo-fixtures"

      # Export test environment variables, PATH, and GIT_BINARY
      ${testSupport.testRuntimeExports}

      mkdir -p $STATE_DIR

      # Exercise reporting against a local HTTP server, including a hung request.
      node ${e2eTestRunner}/healthcheck-tests.js

      # Start wiremock services
      echo "Starting WireMock services..."
      start-wiremock &
      WIREMOCK_PID=$!

      # Wait for wiremock (github, storage, healthchecks)
      for port in ${toString ports.github} ${toString ports.storage} ${toString ports.healthchecks}; do
        until curl -s "http://localhost:$port/__admin" > /dev/null 2>&1; do
          sleep 0.5
        done
      done
      echo "WireMock ready"

      # Start server
      echo "Starting registry server..."
      start-server &
      SERVER_PID=$!

      # Wait for server with timeout
      echo "Waiting for server..."
      timeout=60
      elapsed=0
      until curl -s "http://localhost:${toString ports.server}/api/v1/jobs" > /dev/null 2>&1; do
        sleep 1
        elapsed=$((elapsed + 1))
        if [ $elapsed -ge $timeout ]; then
          echo "ERROR: Server failed to start within ''${timeout}s"
          exit 1
        fi
      done
      echo "Server ready"

      # Run E2E tests while the server's startup allowance elapses.
      echo "Running E2E tests..."
      node ${e2eTestRunner}/e2e-tests.js

      # Verify the actual server reports operational health, not just HTTP
      # liveness. The first report follows a one-minute startup allowance.
      echo "Waiting for an operational Healthchecks report..."
      elapsed=0
      until curl --fail --silent "http://localhost:${toString ports.healthchecks}/__admin/requests" \
        | jq -e 'any(.requests[]; .request.method == "POST" and .request.url == "/" and (.request.body | contains("executor operational")))' > /dev/null; do
        sleep 1
        elapsed=$((elapsed + 1))
        if [ $elapsed -ge 90 ]; then
          echo "ERROR: No operational Healthchecks report within 90s"
          exit 1
        fi
      done

      echo "E2E tests passed!" > $out
    ''
