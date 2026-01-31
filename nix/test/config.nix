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
    # Single storage WireMock instance for bucket + s3 + pursuit (merged for stateful scenarios)
    storage = serverPort + 2;
    healthchecks = serverPort + 3;
  };

  # Fixed state directory for tests - not configurable to avoid mismatch between
  # test-env and spago-test-e2e shells. The test-env script cleans this up on start.
  stateDir = "/tmp/registry-test-env";

  # Mock service URLs for test environment
  # All storage-related APIs (s3, bucket, pursuit) now share a single WireMock instance
  mockUrls = {
    registry = "http://localhost:${toString ports.server}/api";
    github = "http://localhost:${toString ports.github}";
    storage = "http://localhost:${toString ports.storage}";
    healthchecks = "http://localhost:${toString ports.healthchecks}";
  };

  # Valid ED25519 test keypair for pacchettibotti (used for signing authenticated operations).
  # These are test-only keys, not used in production.
  testKeys = {
    # ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHXE9ia5mQG5dPyS6pirU9PSWFP8hPglwChJERBpMoki pacchettibotti@purescript.org
    public = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUhYRTlpYTVtUUc1ZFB5UzZwaXJVOVBTV0ZQOGhQZ2x3Q2hKRVJCcE1va2kgcGFjY2hldHRpYm90dGlAcHVyZXNjcmlwdC5vcmcK";
    # OpenSSH format private key
    private = "LS0tLS1CRUdJTiBPUEVOU1NIIFBSSVZBVEUgS0VZLS0tLS0KYjNCbGJuTnphQzFyWlhrdGRqRUFBQUFBQkc1dmJtVUFBQUFFYm05dVpRQUFBQUFBQUFBQkFBQUFNd0FBQUF0emMyZ3RaVwpReU5UVXhPUUFBQUNCMXhQWW11WmtCdVhUOGt1cVlxMVBUMGxoVC9JVDRKY0FvU1JFUWFUS0pJZ0FBQUtBMVFMT3NOVUN6CnJBQUFBQXR6YzJndFpXUXlOVFV4T1FBQUFDQjF4UFltdVprQnVYVDhrdXFZcTFQVDBsaFQvSVQ0SmNBb1NSRVFhVEtKSWcKQUFBRUJ1dUErV2NqODlTcjR2RUZnU043ZVF5SGFCWlYvc0F2YVhvVGRKa2lwanlYWEU5aWE1bVFHNWRQeVM2cGlyVTlQUwpXRlA4aFBnbHdDaEpFUkJwTW9raUFBQUFIWEJoWTJOb1pYUjBhV0p2ZEhScFFIQjFjbVZ6WTNKcGNIUXViM0puCi0tLS0tRU5EIE9QRU5TU0ggUFJJVkFURSBLRVktLS0tLQo=";
  };

  # Complete test environment - starts with .env.example defaults which include
  # mock secrets, then overrides external services with mock URLs.
  # All storage-related APIs share a single WireMock instance for stateful scenarios.
  testEnv = envDefaults // {
    # State directory and derived paths
    STATE_DIR = stateDir;
    REPO_FIXTURES_DIR = "${stateDir}/repo-fixtures";
    DATABASE_URL = "sqlite:${stateDir}/db/registry.sqlite3";
    # Mock service URLs (override production endpoints)
    REGISTRY_API_URL = mockUrls.registry;
    GITHUB_API_URL = mockUrls.github;
    # All storage-related APIs share a single base URL for stateful scenarios
    S3_API_URL = mockUrls.storage;
    S3_BUCKET_URL = mockUrls.storage;
    PURSUIT_API_URL = mockUrls.storage;
    HEALTHCHECKS_URL = mockUrls.healthchecks;
    PACCHETTIBOTTI_ED25519_PUB = testKeys.public;
    PACCHETTIBOTTI_ED25519 = testKeys.private;
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

  # Test overlay: mocks git and limits compilers for faster tests.
  # Using pkgs.extend avoids a second nixpkgs instantiation (more efficient).
  testOverlay = _: prev: {
    # Substitute gitMock for git in registry-runtime-deps
    registry-runtime-deps = map (
      pkg: if pkg == prev.git then gitMock else pkg
    ) prev.registry-runtime-deps;

    # Limit to 2 compilers for faster matrix job tests.
    # These versions match the compilers referenced in app/fixtures.
    registry-supported-compilers = lib.filterAttrs (
      name: _: name == "purs-0_15_10" || name == "purs-0_15_11"
    ) prev.registry-supported-compilers;
  };

  registryPkgs = pkgs.extend testOverlay;

  # Centralized test runtime dependencies - use this in nativeBuildInputs
  # to ensure all required binaries are available
  testRuntimeInputs = registryPkgs.registry-runtime-deps ++ [ gitMock ];

  # Centralized test runtime exports - use this in shell scripts to set up
  # the complete test environment including PATH and GIT_BINARY.
  # This is the single source of truth for test Git overrides.
  testRuntimeExports = ''
    ${envToExports testEnv}
    export PATH="${lib.makeBinPath testRuntimeInputs}:$PATH"
    export GIT_BINARY="${pkgs.git}/bin/git"
  '';

  # Complete build inputs for integration tests - combines runtime inputs with
  # orchestration scripts. Use this in nativeBuildInputs for test derivations.
  testBuildInputs = testRuntimeInputs ++ [
    wiremockStartScript
    serverStartScript
    setupGitFixtures
  ];

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

  # Console package helpers (console@6.1.0)
  consoleBase64Response =
    fileName:
    base64Response {
      url = "/repos/purescript/purescript-console/contents/${fileName}?ref=v6.1.0";
      inherit fileName;
      filePath = rootPath + "/app/fixtures/github-packages/console-6.1.0/${fileName}";
    };

  console404Response = fileName: {
    request = {
      method = "GET";
      url = "/repos/purescript/purescript-console/contents/${fileName}?ref=v6.1.0";
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

  # Unsafe-coerce package helpers (unsafe-coerce@6.0.0)
  unsafeCoerceBase64Response =
    fileName:
    base64Response {
      url = "/repos/purescript/purescript-unsafe-coerce/contents/${fileName}?ref=v6.0.0";
      inherit fileName;
      filePath = rootPath + "/app/fixtures/github-packages/unsafe-coerce-6.0.0/${fileName}";
    };

  unsafeCoerce404Response = fileName: {
    request = {
      method = "GET";
      url = "/repos/purescript/purescript-unsafe-coerce/contents/${fileName}?ref=v6.0.0";
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

  # Type-equality package helpers (type-equality@4.0.2)
  # Note: Uses purescript owner (actual location) not old-owner (metadata location)
  typeEqualityBase64Response =
    fileName:
    base64Response {
      url = "/repos/purescript/purescript-type-equality/contents/${fileName}?ref=v4.0.2";
      inherit fileName;
      filePath = rootPath + "/app/fixtures/github-packages/type-equality-4.0.1/${fileName}";
    };

  typeEquality404Response = fileName: {
    request = {
      method = "GET";
      url = "/repos/purescript/purescript-type-equality/contents/${fileName}?ref=v4.0.2";
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
    # Console package (console@6.1.0)
    (consoleBase64Response "bower.json")
    (consoleBase64Response "LICENSE")
    (console404Response "spago.yaml")
    (console404Response "spago.dhall")
    (console404Response "purs.json")
    (console404Response "package.json")
    # Unsafe-coerce package (unsafe-coerce@6.0.0)
    (unsafeCoerceBase64Response "bower.json")
    (unsafeCoerce404Response "LICENSE")
    (unsafeCoerce404Response "spago.yaml")
    (unsafeCoerce404Response "spago.dhall")
    (unsafeCoerce404Response "purs.json")
    (unsafeCoerce404Response "package.json")
    # Type-equality package (type-equality@4.0.2 for legacy imports test)
    (typeEqualityBase64Response "bower.json")
    (typeEqualityBase64Response "LICENSE")
    (typeEquality404Response "spago.yaml")
    (typeEquality404Response "spago.dhall")
    (typeEquality404Response "purs.json")
    (typeEquality404Response "package.json")
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
    # Tags for prelude package (only v6.0.1 which is already published)
    {
      request = {
        method = "GET";
        url = "/repos/purescript/purescript-prelude/tags";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = [
          {
            name = "v6.0.1";
            commit = {
              sha = "abc123def456";
              url = "https://api.github.com/repos/purescript/purescript-prelude/commits/abc123def456";
            };
          }
        ];
      };
    }
    # Commits endpoint for prelude - return empty (no recent commits)
    {
      request = {
        method = "GET";
        urlPattern = "/repos/purescript/purescript-prelude/commits\\?since=.*";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = [ ];
      };
    }
    # Commits endpoint for type-equality - return the v4.0.2 commit sha
    # This makes the DailyImporter detect that v4.0.2 is a recent commit
    {
      request = {
        method = "GET";
        urlPattern = "/repos/purescript/purescript-type-equality/commits\\?since=.*";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = [
          { sha = "type-eq-sha-402"; }
        ];
      };
    }
    # Tags for type-equality package (used by two scheduler tests):
    # 1. Transfer detection: metadata says purescript, commit URLs point to new-owner
    # 2. Legacy imports: v4.0.2 is a new version not yet published
    {
      request = {
        method = "GET";
        url = "/repos/purescript/purescript-type-equality/tags";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = [
          {
            name = "v4.0.1";
            commit = {
              sha = "type-eq-sha-401";
              # Points to new owner - scheduler detects this transfer
              url = "https://api.github.com/repos/new-owner/purescript-type-equality/commits/type-eq-sha-401";
            };
          }
          {
            name = "v4.0.2";
            commit = {
              sha = "type-eq-sha-402";
              # New version not yet published - scheduler detects for legacy import
              url = "https://api.github.com/repos/new-owner/purescript-type-equality/commits/type-eq-sha-402";
            };
          }
        ];
      };
    }
    # Accept issue comment creation (used by GitHubIssue workflow)
    {
      request = {
        method = "POST";
        urlPattern = "/repos/purescript/registry/issues/[0-9]+/comments";
      };
      response = {
        status = 201;
        headers."Content-Type" = "application/json";
        jsonBody = {
          id = 1;
          body = "ok";
        };
      };
    }
    # Accept issue closing (used by GitHubIssue workflow)
    {
      request = {
        method = "PATCH";
        urlPattern = "/repos/purescript/registry/issues/[0-9]+";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          id = 1;
          state = "closed";
        };
      };
    }
    # GitHub Teams API for trustee verification (used by GitHubIssue workflow)
    {
      request = {
        method = "GET";
        urlPattern = "/orgs/purescript/teams/packaging/members.*";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        # Return packaging-team-user as a packaging team member for trustee re-signing tests
        jsonBody = [
          {
            login = "packaging-team-user";
            id = 1;
          }
        ];
      };
    }
  ];

  # Fixture directory for storage (tarballs)
  storageFixturesDir = rootPath + "/app/fixtures/registry-storage";

  # Parse tarball filename into package name and version
  # e.g. "effect-4.0.0.tar.gz" -> { name = "effect"; version = "4.0.0"; fileName = "effect-4.0.0.tar.gz"; }
  # e.g. "type-equality-4.0.1.tar.gz" -> { name = "type-equality"; version = "4.0.1"; ... }
  parseTarball =
    fileName:
    let
      base = lib.removeSuffix ".tar.gz" fileName;
      parts = lib.splitString "-" base;
      # Version is the last part; name is everything before
      version = lib.last parts;
      name = lib.concatStringsSep "-" (lib.init parts);
    in
    {
      inherit name version fileName;
    };

  # List all .tar.gz files in storage fixtures
  storageTarballs = map parseTarball (
    builtins.filter (f: lib.hasSuffix ".tar.gz" f) (
      builtins.attrNames (builtins.readDir storageFixturesDir)
    )
  );

  # Metadata fixtures directory (to determine which package versions are "published")
  metadataFixturesDir = rootPath + "/app/fixtures/registry/metadata";
  metadataFiles = builtins.attrNames (builtins.readDir metadataFixturesDir);

  # Parse metadata files to get the actual published versions (not just package names)
  # Returns a set like { "prelude-6.0.1" = true; "type-equality-4.0.1" = true; }
  publishedVersions = lib.foldl' (
    acc: fileName:
    let
      packageName = lib.removeSuffix ".json" fileName;
      metadata = builtins.fromJSON (builtins.readFile (metadataFixturesDir + "/${fileName}"));
      versions = builtins.attrNames (metadata.published or { });
    in
    acc // lib.genAttrs (map (v: "${packageName}-${v}") versions) (_: true)
  ) { } metadataFiles;

  # ============================================================================
  # UNIFIED STORAGE MAPPINGS WITH WIREMOCK SCENARIOS
  # ============================================================================
  #
  # All storage-related APIs (S3 downloads, bucket uploads, Pursuit) are now served
  # by a single WireMock instance with stateful scenarios. This enables proper
  # read-after-write semantics - when a test publishes a package, subsequent
  # downloads will succeed.
  #
  # Scenario design:
  # - One scenario per package-version (e.g., "effect-4.0.0")
  # - WireMock scenarios always start at state "Started"
  # - Published versions (version exists in metadata.published): "Started" means Present
  #   - After DELETE, transitions to "Deleted" state (404 on GET)
  # - Unpublished versions (new version not in metadata): "Started" means Absent (404)
  #   - After PUT upload, transitions to "Present" state
  #   - After DELETE, transitions to "Deleted" state (404 on GET)
  #
  # State machine:
  #   Published:   Started(Present) --DELETE--> Deleted(404)
  #   Unpublished: Started(404) --PUT--> Present(200) --DELETE--> Deleted(404)
  #
  # Reset between tests via POST /__admin/scenarios/reset
  # ============================================================================

  # Generate S3 GET mappings with scenario support
  s3Mappings = lib.concatMap (
    pkg:
    let
      scenario = "${pkg.name}-${pkg.version}";
      isPublished = publishedVersions ? "${pkg.name}-${pkg.version}";
      tarPath = "/${pkg.name}/${pkg.version}.tar.gz";
    in
    if isPublished then
      # Published package: tarball available in Started state, 404 in Deleted state
      [
        {
          request = {
            method = "GET";
            url = tarPath;
          };
          response = {
            status = 200;
            headers."Content-Type" = "application/octet-stream";
            bodyFileName = pkg.fileName;
          };
          scenarioName = scenario;
          requiredScenarioState = "Started";
        }
        {
          request = {
            method = "GET";
            url = tarPath;
          };
          response = {
            status = 404;
            body = "Not Found";
          };
          scenarioName = scenario;
          requiredScenarioState = "Deleted";
        }
      ]
    else
      # Unpublished package: 404 in Started, 200 in Present, 404 in Deleted
      [
        {
          request = {
            method = "GET";
            url = tarPath;
          };
          response = {
            status = 404;
            body = "Not Found";
          };
          scenarioName = scenario;
          requiredScenarioState = "Started";
        }
        {
          request = {
            method = "GET";
            url = tarPath;
          };
          response = {
            status = 200;
            headers."Content-Type" = "application/octet-stream";
            bodyFileName = pkg.fileName;
          };
          scenarioName = scenario;
          requiredScenarioState = "Present";
        }
        {
          request = {
            method = "GET";
            url = tarPath;
          };
          response = {
            status = 404;
            body = "Not Found";
          };
          scenarioName = scenario;
          requiredScenarioState = "Deleted";
        }
      ]
  ) storageTarballs;

  # Generate s3Files list from fixtures (tarballs for bodyFileName references)
  s3Files = map (pkg: {
    name = pkg.fileName;
    path = storageFixturesDir + "/${pkg.fileName}";
  }) storageTarballs;

  # Generate bucket PUT/DELETE/listObjects mappings with scenario support
  # The AWS SDK uses virtual-hosted style URLs by default, where the bucket name
  # is in the hostname (purescript-registry.localhost:9002) and the path contains
  # only the key.
  bucketMappings =
    # Generate per-package listObjects mappings with scenario support
    (lib.concatMap (
      pkg:
      let
        scenario = "${pkg.name}-${pkg.version}";
        isPublished = publishedVersions ? "${pkg.name}-${pkg.version}";
        escapedName = lib.replaceStrings [ "-" ] [ "\\-" ] pkg.name;
        listUrlPattern = "/\\?.*prefix=${escapedName}.*";
        presentContents = ''<Contents><Key>${pkg.name}/${pkg.version}.tar.gz</Key><Size>1000</Size><ETag>"abc123"</ETag></Contents>'';
      in
      if isPublished then
        # Published package: listObjects returns contents in Started, empty in Deleted
        [
          {
            request = {
              method = "GET";
              urlPattern = listUrlPattern;
            };
            response = {
              status = 200;
              headers."Content-Type" = "application/xml";
              body = "<ListBucketResult>${presentContents}</ListBucketResult>";
            };
            scenarioName = scenario;
            requiredScenarioState = "Started";
          }
          {
            request = {
              method = "GET";
              urlPattern = listUrlPattern;
            };
            response = {
              status = 200;
              headers."Content-Type" = "application/xml";
              body = "<ListBucketResult></ListBucketResult>";
            };
            scenarioName = scenario;
            requiredScenarioState = "Deleted";
          }
        ]
      else
        # Unpublished package: listObjects returns empty in Started, contents in Present, empty in Deleted
        [
          {
            request = {
              method = "GET";
              urlPattern = listUrlPattern;
            };
            response = {
              status = 200;
              headers."Content-Type" = "application/xml";
              body = "<ListBucketResult></ListBucketResult>";
            };
            scenarioName = scenario;
            requiredScenarioState = "Started";
          }
          {
            request = {
              method = "GET";
              urlPattern = listUrlPattern;
            };
            response = {
              status = 200;
              headers."Content-Type" = "application/xml";
              body = "<ListBucketResult>${presentContents}</ListBucketResult>";
            };
            scenarioName = scenario;
            requiredScenarioState = "Present";
          }
          {
            request = {
              method = "GET";
              urlPattern = listUrlPattern;
            };
            response = {
              status = 200;
              headers."Content-Type" = "application/xml";
              body = "<ListBucketResult></ListBucketResult>";
            };
            scenarioName = scenario;
            requiredScenarioState = "Deleted";
          }
        ]
    ) storageTarballs)
    ++ (
      # Generate PUT/DELETE mappings for all packages with scenario support
      lib.concatMap (
        pkg:
        let
          scenario = "${pkg.name}-${pkg.version}";
          isPublished = publishedVersions ? "${pkg.name}-${pkg.version}";
          escapedVersion = lib.replaceStrings [ "." ] [ "\\." ] pkg.version;
          urlPattern = "/${pkg.name}/${escapedVersion}\\.tar\\.gz.*";
        in
        if isPublished then
          # Published package: PUT fails (already exists), DELETE transitions to Deleted
          [
            {
              request = {
                method = "PUT";
                urlPattern = urlPattern;
              };
              response = {
                status = 500;
                body = "Package already published";
              };
              scenarioName = scenario;
              requiredScenarioState = "Started";
            }
            # DELETE in Started state (package exists) transitions to Deleted
            {
              request = {
                method = "DELETE";
                urlPattern = urlPattern;
              };
              response = {
                status = 204;
              };
              scenarioName = scenario;
              requiredScenarioState = "Started";
              newScenarioState = "Deleted";
            }
            # DELETE in Deleted state fails (already deleted)
            {
              request = {
                method = "DELETE";
                urlPattern = urlPattern;
              };
              response = {
                status = 404;
                body = "Not Found";
              };
              scenarioName = scenario;
              requiredScenarioState = "Deleted";
            }
          ]
        else
          # Unpublished package: PUT succeeds and transitions to Present, DELETE transitions to Deleted
          [
            {
              request = {
                method = "PUT";
                urlPattern = urlPattern;
              };
              response = {
                status = 200;
                headers."ETag" = ''"abc123"'';
                headers."Content-Type" = "application/xml";
                body = "";
              };
              scenarioName = scenario;
              requiredScenarioState = "Started";
              newScenarioState = "Present";
            }
            # PUT in Present state fails (already uploaded)
            {
              request = {
                method = "PUT";
                urlPattern = urlPattern;
              };
              response = {
                status = 500;
                body = "Package already uploaded";
              };
              scenarioName = scenario;
              requiredScenarioState = "Present";
            }
            # DELETE in Started state fails (doesn't exist yet)
            {
              request = {
                method = "DELETE";
                urlPattern = urlPattern;
              };
              response = {
                status = 404;
                body = "Not Found";
              };
              scenarioName = scenario;
              requiredScenarioState = "Started";
            }
            # DELETE in Present state (after publish) transitions to Deleted
            {
              request = {
                method = "DELETE";
                urlPattern = urlPattern;
              };
              response = {
                status = 204;
              };
              scenarioName = scenario;
              requiredScenarioState = "Present";
              newScenarioState = "Deleted";
            }
            # DELETE in Deleted state fails (already deleted)
            {
              request = {
                method = "DELETE";
                urlPattern = urlPattern;
              };
              response = {
                status = 404;
                body = "Not Found";
              };
              scenarioName = scenario;
              requiredScenarioState = "Deleted";
            }
          ]
      ) storageTarballs
    );

  # Pursuit API mappings with scenario support
  pursuitMappings =
    (lib.concatMap (
      pkg:
      let
        scenario = "${pkg.name}-${pkg.version}";
        isPublished = publishedVersions ? "${pkg.name}-${pkg.version}";
        versionsUrl = "/packages/purescript-${pkg.name}/available-versions";
        publishedVersionsBody = ''[["${pkg.version}","https://pursuit.purescript.org/packages/purescript-${pkg.name}/${pkg.version}"]]'';
      in
      if isPublished then
        # Published package: versions available in Started, empty in Deleted
        [
          {
            request = {
              method = "GET";
              url = versionsUrl;
            };
            response = {
              status = 200;
              body = publishedVersionsBody;
            };
            scenarioName = scenario;
            requiredScenarioState = "Started";
          }
          {
            request = {
              method = "GET";
              url = versionsUrl;
            };
            response = {
              status = 200;
              body = "[]";
            };
            scenarioName = scenario;
            requiredScenarioState = "Deleted";
          }
        ]
      else
        # Unpublished package: empty in Started, has version in Present, empty in Deleted
        [
          {
            request = {
              method = "GET";
              url = versionsUrl;
            };
            response = {
              status = 200;
              body = "[]";
            };
            scenarioName = scenario;
            requiredScenarioState = "Started";
          }
          {
            request = {
              method = "GET";
              url = versionsUrl;
            };
            response = {
              status = 200;
              body = publishedVersionsBody;
            };
            scenarioName = scenario;
            requiredScenarioState = "Present";
          }
          {
            request = {
              method = "GET";
              url = versionsUrl;
            };
            response = {
              status = 200;
              body = "[]";
            };
            scenarioName = scenario;
            requiredScenarioState = "Deleted";
          }
        ]
    ) storageTarballs)
    ++ [
      # Accept documentation uploads (POST /packages)
      {
        request = {
          method = "POST";
          url = "/packages";
        };
        response.status = 201;
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

  # Combined storage mappings (S3 + bucket + Pursuit)
  storageMappings = s3Mappings ++ bucketMappings ++ pursuitMappings;
  storageFiles = s3Files;

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

  # All WireMock configurations.
  # Add new WireMock services here; both test-env.nix and integration.nix
  # derive their processes from this attribute set automatically.
  wiremockConfigs = {
    github = {
      port = ports.github;
      rootDir = mkWiremockRoot {
        name = "github";
        mappings = githubMappings;
      };
    };
    # Single storage WireMock instance with stateful scenarios
    storage = {
      port = ports.storage;
      rootDir = mkWiremockRoot {
        name = "storage";
        mappings = storageMappings;
        files = storageFiles;
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
    runtimeInputs = [
      pkgs.git
      pkgs.jq
    ];
    text = ''
      FIXTURES_DIR="''${1:-${stateDir}/repo-fixtures}"

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
      cp -r ${rootPath}/app/fixtures/github-packages/console-6.1.0 "$FIXTURES_DIR/purescript/purescript-console"
      cp -r ${rootPath}/app/fixtures/github-packages/unsafe-coerce-6.0.0 "$FIXTURES_DIR/purescript/purescript-unsafe-coerce"
      cp -r ${rootPath}/app/fixtures/github-packages/type-equality-4.0.1 "$FIXTURES_DIR/purescript/purescript-type-equality"
      chmod -R u+w "$FIXTURES_DIR/purescript"

      # Set type-equality publishedTime to current time for package set update test
      # This makes type-equality appear as a "recent upload" so the scheduler will
      # detect it and enqueue a package set update job
      current_time=$(date -u +"%Y-%m-%dT%H:%M:%S.000Z")
      jq --arg time "$current_time" \
        '.published["4.0.1"].publishedTime = $time' \
        "$FIXTURES_DIR/purescript/registry/metadata/type-equality.json" > temp.json && \
        mv temp.json "$FIXTURES_DIR/purescript/registry/metadata/type-equality.json"

      for repo in "$FIXTURES_DIR"/purescript/*/; do
        cd "$repo"
        git init -b master && git add .
        gitbot commit -m "Fixture commit"
        git config receive.denyCurrentBranch ignore
        # Tag the initial commit so we can reset to it for test isolation
        gitbot tag -m "initial-fixture" initial-fixture
      done

      gitbot -C "$FIXTURES_DIR/purescript/package-sets" tag -m "psc-0.15.9-20230105" psc-0.15.9-20230105
      gitbot -C "$FIXTURES_DIR/purescript/purescript-effect" tag -m "v4.0.0" v4.0.0
      gitbot -C "$FIXTURES_DIR/purescript/purescript-console" tag -m "v6.1.0" v6.1.0
      gitbot -C "$FIXTURES_DIR/purescript/purescript-unsafe-coerce" tag -m "v6.0.0" v6.0.0
      gitbot -C "$FIXTURES_DIR/purescript/purescript-type-equality" tag -m "v4.0.1" v4.0.1
      # Create a new commit for v4.0.2 so it's on a different commit than v4.0.1
      # (the registry rejects publishing when multiple version tags point to the same commit)
      gitbot -C "$FIXTURES_DIR/purescript/purescript-type-equality" commit --allow-empty -m "v4.0.2 release"
      gitbot -C "$FIXTURES_DIR/purescript/purescript-type-equality" tag -m "v4.0.2" v4.0.2
    '';
  };

  # Publish payload for testing
  publishPayload = pkgs.writeText "publish-effect.json" (
    builtins.toJSON {
      name = "effect";
      ref = "v4.0.0";
      compiler = "0.15.10";
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

    # Set all test environment variables, PATH, and GIT_BINARY
    ${testRuntimeExports}

    # STATE_DIR is required
    if [ -z "''${STATE_DIR:-}" ]; then
      echo "ERROR: STATE_DIR must be set"
      exit 1
    fi

    # Runtime paths (derived from STATE_DIR, can't be set statically)
    export DATABASE_URL="sqlite:$STATE_DIR/db/registry.sqlite3"
    export REPO_FIXTURES_DIR="$STATE_DIR/repo-fixtures"

    mkdir -p "$STATE_DIR/db"

    # Always recreate git fixtures to ensure clean state
    # (the setupGitFixtures script handles cleanup internally)
    echo "Setting up git fixtures..."
    ${setupGitFixtures}/bin/setup-git-fixtures "$REPO_FIXTURES_DIR"

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
    stateDir
    mockUrls
    testEnv
    testOverlay
    testRuntimeInputs
    testRuntimeExports
    testBuildInputs
    wiremockConfigs
    combinedWiremockRoot
    publishPayload
    wiremockStartScript
    serverStartScript
    setupGitFixtures
    # For custom wiremock setups
    githubMappings
    storageMappings
    storageFiles
    mkWiremockRoot
    ;
}
