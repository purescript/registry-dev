{
  pkgs,
  spagoSrc,
  testEnv,
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
      '';
      installPhase = ''
        mkdir -p $out
        cp e2e-tests.js $out/
      '';
    };

    ports = testEnv.ports;
  in
  pkgs.runCommand "e2e-integration"
    {
      nativeBuildInputs = [
        pkgs.nodejs
        pkgs.curl
        pkgs.jq
        pkgs.git
        pkgs.nss_wrapper
        testEnv.wiremockStartScript
        testEnv.serverStartScript
        testEnv.setupGitFixtures
      ];
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

      # Export test environment variables for E2E test runners
      ${testEnv.testConfig.testRunnerExports}

      mkdir -p $STATE_DIR

      # Start wiremock services
      echo "Starting WireMock services..."
      start-wiremock &
      WIREMOCK_PID=$!

      # Wait for wiremock (github, bucket, s3, pursuit)
      for port in ${toString ports.github} ${toString ports.bucket} ${toString ports.s3} ${toString ports.pursuit}; do
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

      # Run E2E tests
      echo "Running E2E tests..."
      node ${e2eTestRunner}/e2e-tests.js

      echo "E2E tests passed!" > $out
    ''
