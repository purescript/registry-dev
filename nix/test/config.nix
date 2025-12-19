# Shared test configuration for integration tests and container-based test environment.
# This file is the single source of truth for ports, wiremock mappings, fixtures, and
# server startup logic.
#
# This file provides test env overrides that point to local mock services.
{
  pkgs,
  lib,
  rootPath,
}:
let
  # Parse .env.example for shared defaults
  nixLib = import ../lib.nix { inherit lib; };
  envDefaults = nixLib.parseEnvFile (builtins.readFile (rootPath + "/.env.example"));

  # Port configuration for test services and server
  # Server port comes from .env.example; mock service ports are offsets from it
  serverPort = lib.toInt envDefaults.SERVER_PORT;
  ports = {
    server = serverPort;
    github = serverPort + 1;
    bucket = serverPort + 2;
    s3 = serverPort + 3;
    pursuit = serverPort + 4;
    healthchecks = serverPort + 5;
  };

  # Default state directory for tests
  defaultStateDir = "/var/lib/registry-server";

  # Mock service URLs for test environment
  mockUrls = {
    github = "http://localhost:${toString ports.github}";
    s3 = "http://localhost:${toString ports.s3}";
    bucket = "http://localhost:${toString ports.bucket}";
    pursuit = "http://localhost:${toString ports.pursuit}";
    healthchecks = "http://localhost:${toString ports.healthchecks}";
  };

  # Complete test environment - starts with .env.example defaults which include
  # mock secrets, then overrides external services with mock URLs. The DATABASE_URL
  # and REPO_FIXTURES_DIR vars are derived from STATE_DIR at runtime so those are
  # implemented in the script directly.
  testEnv = envDefaults // {
    # Mock service URLs (override production endpoints)
    GITHUB_API_URL = mockUrls.github;
    S3_API_URL = mockUrls.s3;
    S3_BUCKET_URL = mockUrls.bucket;
    PURSUIT_API_URL = mockUrls.pursuit;
    HEALTHCHECKS_URL = mockUrls.healthchecks;
  };

  envToExports =
    env:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: ''export ${name}="${value}"'') env);

  # Git mock that redirects URLs to local fixtures; this is necessary because otherwise
  # commands would reach out to GitHub or the other package origins.
  gitMock = pkgs.writeShellScriptBin "git" ''
    export GIT_BINARY="${pkgs.git}/bin/git"
    exec ${pkgs.nodejs}/bin/node ${./git-mock.mjs} "$@"
  '';

  # Apply git mock overlay to get registry packages with mocked git.
  # Using pkgs.extend avoids a second nixpkgs instantiation (more efficient).
  # This substitutes gitMock for git in registry-runtime-deps, which causes
  # registry-server to be rebuilt with the mock baked into its PATH wrapper.
  gitMockOverlay = _: prev: {
    registry-runtime-deps = map (
      pkg: if pkg == prev.git then gitMock else pkg
    ) prev.registry-runtime-deps;
  };

  registryPkgs = pkgs.extend gitMockOverlay;

  # Helper to create GitHub contents API response, as it returns base64-encoded content
  base64Response =
    {
      url,
      fileName,
      filePath,
    }:
    {
      request = {
        method = "GET";
        inherit url;
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          type = "file";
          encoding = "base64";
          name = fileName;
          path = fileName;
          # Base64 encode the file content using Nix builtins
          content = builtins.readFile (
            pkgs.runCommand "base64-${fileName}" { } ''
              base64 -w 0 ${filePath} > $out
            ''
          );
        };
      };
    };

  effectBase64Response =
    fileName:
    base64Response {
      url = "/repos/purescript/purescript-effect/contents/${fileName}?ref=v4.0.0";
      inherit fileName;
      filePath = rootPath + "/app/fixtures/github-packages/effect-4.0.0/${fileName}";
    };

  # Helper to create a 404 response for files we expect not to exist
  effect404Response = fileName: {
    request = {
      method = "GET";
      url = "/repos/purescript/purescript-effect/contents/${fileName}?ref=v4.0.0";
    };
    response = {
      status = 404;
      headers."Content-Type" = "application/json";
      jsonBody = {
        message = "Not Found";
        documentation_url = "https://docs.github.com/rest/repos/contents#get-repository-content";
      };
    };
  };

  # GitHub API wiremock mappings
  githubMappings = [
    (effectBase64Response "bower.json")
    (effectBase64Response "LICENSE")
    # Expected 404s for manifest files that don't exist in the effect package
    (effect404Response "spago.yaml")
    (effect404Response "spago.dhall")
    (effect404Response "purs.json")
    (effect404Response "package.json")
    {
      request = {
        method = "GET";
        url = "/repos/purescript/package-sets/tags";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          name = "psc-0.15.10-20230105";
          commit = {
            sha = "090897c992b2b310b1456506308db789672adac1";
            url = "https://api.github.com/repos/purescript/package-sets/commits/090897c992b2b310b1456506308db789672adac1";
          };
        };
      };
    }
  ];

  # S3 API wiremock mappings (serves package tarballs)
  s3Mappings = [
    {
      request = {
        method = "GET";
        url = "/prelude/6.0.1.tar.gz";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/octet-stream";
        bodyFileName = "prelude-6.0.1.tar.gz";
      };
    }
    {
      request = {
        method = "GET";
        url = "/type-equality/4.0.1.tar.gz";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/octet-stream";
        bodyFileName = "type-equality-4.0.1.tar.gz";
      };
    }
  ];

  s3Files = [
    {
      name = "prelude-6.0.1.tar.gz";
      path = rootPath + "/app/fixtures/registry-storage/prelude-6.0.1.tar.gz";
    }
    {
      name = "type-equality-4.0.1.tar.gz";
      path = rootPath + "/app/fixtures/registry-storage/type-equality-4.0.1.tar.gz";
    }
  ];

  # S3 Bucket API wiremock mappings (handles upload/list operations)
  # The AWS SDK uses virtual-hosted style URLs by default, where the bucket name
  # is in the hostname (purescript-registry.localhost:9002) and the path contains
  # only the key. For example: GET /?prefix=effect/ instead of GET /purescript-registry?prefix=effect/
  bucketMappings = [
    # List objects - virtual-hosted style (bucket in hostname, path is just /?prefix=...)
    {
      request = {
        method = "GET";
        urlPattern = "/\\?.*prefix=.*";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/xml";
        body = ''<ListBucketResult><Contents><Key>prelude/6.0.1.tar.gz</Key><Size>16298</Size><ETag>"abc123"</ETag></Contents><Contents><Key>type-equality/4.0.1.tar.gz</Key><Size>2184</Size><ETag>"def456"</ETag></Contents></ListBucketResult>'';
      };
    }
    # Upload effect@4.0.0 - virtual-hosted style (path is /effect/4.0.0.tar.gz)
    {
      request = {
        method = "PUT";
        urlPattern = "/effect/4\\.0\\.0\\.tar\\.gz.*";
      };
      response = {
        status = 200;
        headers."ETag" = ''"abc123"'';
        headers."Content-Type" = "application/xml";
        body = "";
      };
    }
    # Fail upload for prelude (to test error handling)
    {
      request = {
        method = "PUT";
        urlPattern = "/prelude/6\\.0\\.1\\.tar\\.gz.*";
      };
      response.status = 500;
    }
  ];

  # Healthchecks API wiremock mappings (simple ping endpoint)
  healthchecksMappings = [
    {
      request = {
        method = "GET";
        urlPattern = "/.*";
      };
      response = {
        status = 200;
        body = "OK";
      };
    }
  ];

  # Pursuit API wiremock mappings
  pursuitMappings = [
    {
      request = {
        method = "GET";
        url = "/packages/purescript-prelude/available-versions";
      };
      response = {
        status = 200;
        body = ''[["6.0.1","https://pursuit.purescript.org/packages/purescript-prelude/6.0.1"]]'';
      };
    }
    {
      request = {
        method = "GET";
        url = "/packages/purescript-effect/available-versions";
      };
      response = {
        status = 200;
        body = ''[]'';
      };
    }
    {
      request = {
        method = "GET";
        url = "/packages/purescript-type-equality/available-versions";
      };
      response = {
        status = 200;
        body = ''[["4.0.1","https://pursuit.purescript.org/packages/purescript-type-equality/4.0.1"]]'';
      };
    }
    {
      request = {
        method = "POST";
        url = "/packages";
      };
      response.status = 201;
    }
  ];

  # Wiremock root directory builder
  mkWiremockRoot =
    {
      name,
      mappings,
      files ? [ ],
    }:
    let
      mappingsFormat = pkgs.formats.json { };
      mappingsJson = mappingsFormat.generate "mappings.json" { inherit mappings; };
    in
    pkgs.runCommand "wiremock-${name}" { } ''
      mkdir -p $out/{mappings,__files}
      cp ${mappingsJson} $out/mappings/mappings.json
      ${lib.concatMapStrings (f: "cp ${f.path} $out/__files/${f.name}\n") files}
    '';

  # All wiremock configurations
  wiremockConfigs = {
    github = {
      port = ports.github;
      rootDir = mkWiremockRoot {
        name = "github";
        mappings = githubMappings;
      };
    };
    s3 = {
      port = ports.s3;
      rootDir = mkWiremockRoot {
        name = "s3";
        mappings = s3Mappings;
        files = s3Files;
      };
    };
    bucket = {
      port = ports.bucket;
      rootDir = mkWiremockRoot {
        name = "bucket";
        mappings = bucketMappings;
      };
    };
    pursuit = {
      port = ports.pursuit;
      rootDir = mkWiremockRoot {
        name = "pursuit";
        mappings = pursuitMappings;
      };
    };
    healthchecks = {
      port = ports.healthchecks;
      rootDir = mkWiremockRoot {
        name = "healthchecks";
        mappings = healthchecksMappings;
      };
    };
  };

  # Combined wiremock root directory with all service mappings
  # Uses mapAttrsToList to avoid manual listing of each service
  combinedWiremockRoot = pkgs.runCommand "wiremock-combined" { } ''
    mkdir -p $out
    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (name: cfg: ''
        mkdir -p $out/${name}
        cp -r ${cfg.rootDir}/* $out/${name}/
      '') wiremockConfigs
    )}
  '';

  # Script to set up git fixtures
  setupGitFixtures = pkgs.writeShellApplication {
    name = "setup-git-fixtures";
    runtimeInputs = [ pkgs.git ];
    text = ''
      FIXTURES_DIR="''${1:-${defaultStateDir}/repo-fixtures}"

      # Run git as pacchettibotti
      gitbot() {
        GIT_AUTHOR_NAME="pacchettibotti" GIT_AUTHOR_EMAIL="pacchettibotti@purescript.org" \
        GIT_COMMITTER_NAME="pacchettibotti" GIT_COMMITTER_EMAIL="pacchettibotti@purescript.org" \
          git "$@"
      }

      # Remove any existing fixtures (they may have wrong permissions from nix store copy)
      rm -rf "$FIXTURES_DIR/purescript" 2>/dev/null || true
      mkdir -p "$FIXTURES_DIR/purescript"

      # Copy fixtures and make writable (nix store files are read-only)
      cp -r ${rootPath}/app/fixtures/{registry-index,registry,package-sets} "$FIXTURES_DIR/purescript/"
      cp -r ${rootPath}/app/fixtures/github-packages/effect-4.0.0 "$FIXTURES_DIR/purescript/purescript-effect"
      chmod -R u+w "$FIXTURES_DIR/purescript"

      for repo in "$FIXTURES_DIR"/purescript/*/; do
        cd "$repo"
        git init -b master && git add .
        gitbot commit -m "Fixture commit"
        git config receive.denyCurrentBranch ignore
      done

      gitbot -C "$FIXTURES_DIR/purescript/package-sets" tag -m "psc-0.15.9-20230105" psc-0.15.9-20230105
      gitbot -C "$FIXTURES_DIR/purescript/purescript-effect" tag -m "v4.0.0" v4.0.0
    '';
  };

  # Publish payload for testing
  publishPayload = pkgs.writeText "publish-effect.json" (
    builtins.toJSON {
      name = "effect";
      ref = "v4.0.0";
      compiler = "0.15.9";
      location = {
        githubOwner = "purescript";
        githubRepo = "purescript-effect";
      };
    }
  );

  # Script to start all wiremock instances (used by CI integration check and test-env.nix)
  # Uses mapAttrsToList to avoid manual listing of each service
  wiremockStartScript = pkgs.writeShellScriptBin "start-wiremock" ''
    set -e
    echo "Starting WireMock services..."

    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (name: cfg: ''
        ${pkgs.wiremock}/bin/wiremock \
          --port ${toString cfg.port} \
          --root-dir ${combinedWiremockRoot}/${name} \
          --disable-banner &
        echo "${name} mock started on port ${toString cfg.port}"
      '') wiremockConfigs
    )}

    echo "All WireMock services started. Waiting..."
    wait
  '';

  # Script to start the registry server with test configuration.
  # Used by CI integration check and test-env.nix.
  #
  # STATE_DIR must be set by the caller. All other env vars are derived from it
  # or set to test values directly here. No external envFile needed.
  serverStartScript = pkgs.writeShellScriptBin "start-server" ''
    set -e

    # Set all test environment variables (from envDefaults + mock URLs).
    ${envToExports testEnv}

    # STATE_DIR is required
    if [ -z "''${STATE_DIR:-}" ]; then
      echo "ERROR: STATE_DIR must be set"
      exit 1
    fi

    # Runtime paths (derived from STATE_DIR, can't be set statically)
    export DATABASE_URL="sqlite:$STATE_DIR/db/registry.sqlite3"
    export REPO_FIXTURES_DIR="$STATE_DIR/repo-fixtures"

    # PATH setup for runtime deps and git mock
    export PATH="${lib.makeBinPath registryPkgs.registry-runtime-deps}:$PATH"
    export PATH="${gitMock}/bin:$PATH"
    export GIT_BINARY="${pkgs.git}/bin/git"

    mkdir -p "$STATE_DIR/db"

    # Set up git fixtures if needed
    if [ ! -d "$REPO_FIXTURES_DIR/purescript" ]; then
      echo "Setting up git fixtures..."
      ${setupGitFixtures}/bin/setup-git-fixtures "$REPO_FIXTURES_DIR"
    fi

    # Run database migrations
    echo "Running database migrations..."
    cd ${registryPkgs.registry-server}/bin
    ${pkgs.dbmate}/bin/dbmate up

    # Change to STATE_DIR so the server's relative 'scratch' path works correctly
    cd "$STATE_DIR"

    echo "Starting registry server on port ${toString ports.server}..."
    exec ${registryPkgs.registry-server}/bin/registry-server
  '';

in
{
  inherit
    ports
    defaultStateDir
    mockUrls
    testEnv
    envToExports
    gitMock
    gitMockOverlay
    wiremockConfigs
    combinedWiremockRoot
    setupGitFixtures
    publishPayload
    wiremockStartScript
    serverStartScript
    # For custom wiremock setups
    githubMappings
    s3Mappings
    s3Files
    bucketMappings
    pursuitMappings
    mkWiremockRoot
    ;
}
