# VM-based integration test for the registry server. This test deploys the actual service
# to a NixOS VM that matches our deploy environment, and then executes the core publishing
# workflow. The registry relies on several external services and tools that we don't
# control, so the APIs are mocked with WireMock and the Git commands are mocked with a
# wrapper CLI tool called `git-mock`.
#
# The integration test is set up such that the `prelude` package is already published to
# the registry, and the user is now publishing the `effect` package. This can be seen in
# the WireMock and Git fixture setup below.
{
  pkgs,
  lib,
  overlays,
  rootPath,
}:

if pkgs.stdenv.isDarwin then
  pkgs.runCommand "integration-skip" { } ''
    echo "Integration tests require Linux VMs, skipping on macOS" > $out
  ''
else
  let
    # Port configuration - single source of truth
    ports = {
      server = 8080;
      github = 9001;
      bucket = 9002;
      s3 = 9003;
      pursuit = 9004;
    };

    stateDir = "/var/lib/registry-server";

    # Git mock that redirects URLs to local fixtures; this is necessary because otherwise
    # commands would reach out to GitHub or the other package origins.
    gitMock = pkgs.writeShellScriptBin "git" ''
      export GIT_BINARY="${pkgs.git}/bin/git"
      exec ${pkgs.nodejs}/bin/node ${./git-mock.mjs} "$@"
    '';

    # WireMock NixOS module to make it easy to mock HTTP services the registry depends on.
    wiremockModule =
      { service }:
      {
        pkgs,
        config,
        lib,
        ...
      }:
      let
        cfg = config.services."wiremock-${service}";
        mappingsFormat = pkgs.formats.json { };
        rootDir =
          let
            mappingsJson = mappingsFormat.generate "mappings.json" { mappings = cfg.mappings; };
          in
          pkgs.runCommand "wiremock-root" { } ''
            mkdir -p $out/{mappings,__files}
            cp ${mappingsJson} $out/mappings/mappings.json
            ${lib.concatMapStrings (f: "cp ${f.path} $out/__files/${f.name}\n") cfg.files}
          '';
      in
      {
        options.services."wiremock-${service}" = {
          enable = lib.mkEnableOption "WireMock";
          port = lib.mkOption {
            type = lib.types.int;
            default = 8080;
          };
          files = lib.mkOption {
            type = lib.types.listOf (
              lib.types.submodule {
                options = {
                  name = lib.mkOption { type = lib.types.str; };
                  path = lib.mkOption { type = lib.types.path; };
                };
              }
            );
            default = [ ];
          };
          mappings = lib.mkOption {
            type = mappingsFormat.type;
            default = [ ];
          };
        };

        config = lib.mkIf cfg.enable {
          systemd.services."wiremock-${service}" = {
            description = "WireMock ${service}";
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              ExecStart = "${pkgs.wiremock}/bin/wiremock --port ${toString cfg.port} --root-dir ${rootDir} --disable-banner";
              Type = "simple";
            };
          };
        };
      };

    parseEnv = import ../lib/parseEnv.nix { inherit lib; };
    envVars = parseEnv (rootPath + "/.env.example") // {
      GITHUB_API_URL = "http://localhost:${toString ports.github}";
      S3_API_URL = "http://localhost:${toString ports.s3}";
      S3_BUCKET_URL = "http://localhost:${toString ports.bucket}";
      PURSUIT_API_URL = "http://localhost:${toString ports.pursuit}";
      REPO_FIXTURES_DIR = "${stateDir}/repo-fixtures";
    };

    setupGitFixtures = pkgs.writeShellScriptBin "setup-git-fixtures" ''
      set -e
      mkdir -p ${stateDir}/repo-fixtures/purescript
      git config --global user.email "pacchettibotti@purescript.org"
      git config --global user.name "pacchettibotti"
      git config --global init.defaultBranch "master"

      cp -r ${rootPath}/app/fixtures/{registry-index,registry,package-sets} ${stateDir}/repo-fixtures/purescript/
      cp -r ${rootPath}/app/fixtures/github-packages/effect-4.0.0 ${stateDir}/repo-fixtures/purescript/purescript-effect

      for repo in ${stateDir}/repo-fixtures/purescript/*/; do
        cd "$repo"
        git init && git add . && git commit -m "Fixture commit"
        git config receive.denyCurrentBranch ignore
      done

      git -C ${stateDir}/repo-fixtures/purescript/package-sets tag -m "psc-0.15.4-20230105" psc-0.15.4-20230105
      git -C ${stateDir}/repo-fixtures/purescript/purescript-effect tag -m "v4.0.0" v4.0.0
    '';

    publishPayload = pkgs.writeText "publish-effect.json" (
      builtins.toJSON {
        name = "effect";
        ref = "v4.0.0";
        compiler = "0.15.4";
        location = {
          githubOwner = "purescript";
          githubRepo = "purescript-effect";
        };
      }
    );
  in
  pkgs.testers.nixosTest {
    name = "registry-integration";

    testScript = ''
      import json
      import time

      # Start registry and set up git fixtures
      registry.start()
      registry.succeed("${setupGitFixtures}/bin/setup-git-fixtures")

      # Wait for all services to be ready
      registry.wait_for_unit("wiremock-github-api.service")
      registry.wait_for_unit("wiremock-s3-api.service")
      registry.wait_for_unit("wiremock-bucket-api.service")
      registry.wait_for_unit("wiremock-pursuit-api.service")
      registry.wait_for_unit("server.service")

      # Start client and wait for API
      client.start()
      client.wait_until_succeeds(
          "curl --fail-with-body http://registry/api/v1/jobs",
          timeout=20
      )

      # Publish a package
      result = json.loads(client.succeed(
          "curl -s -X POST -d @${publishPayload} -H 'Content-Type: application/json' "
          "http://registry/api/v1/publish"
      ))

      job_id = result["jobId"]
      assert len(job_id) == 36, f"Expected job ID, got: {result}"
      print(f"Job created: {job_id}")

      # Poll for completion
      for attempt in range(20):
          time.sleep(3)
          poll = json.loads(client.succeed(
              f"curl -s 'http://registry/api/v1/jobs/{job_id}"
              "?since=2023-01-01T00:00:00Z&level=DEBUG'"
          ))

          if "finishedAt" in poll:
              assert poll["success"], f"Job failed: {poll}"
              print("✓ Job completed successfully")
              break
      else:
          raise Exception("Job did not complete in time")
    '';

    # This section defines the machine, configuring the Wiremock instances to
    # mock external APIs, overriding Git with the mocked version, and setting
    # up the actual Wiremock data to return. The machine is based on the
    # same registry-server Nix module we deploy.
    nodes.client.virtualisation.graphics = false;
    nodes.registry = {
      imports = [
        (wiremockModule { service = "github-api"; })
        (wiremockModule { service = "s3-api"; })
        (wiremockModule { service = "bucket-api"; })
        (wiremockModule { service = "pursuit-api"; })
        (rootPath + "/nix/registry-server.nix")
      ];

      # We replace Git in registry-runtime-deps with our custom mocked Git which
      # prevents reaching out over the network. We override registry-runtime-deps
      # to substitute the mock, which causes registry-server to be rebuilt with it.
      nixpkgs.overlays = overlays ++ [
        (_: prev: {
          registry-runtime-deps = map (
            pkg: if pkg == prev.git then gitMock else pkg
          ) prev.registry-runtime-deps;
        })
      ];

      virtualisation.graphics = false;

      # Finally, we define the running services on the machine: the registry,
      # and then the various wiremock servers.
      services.registry-server = {
        enable = true;
        host = "localhost";
        port = ports.server;
        enableCerts = false;
        inherit stateDir envVars;
      };

      # GitHub API mock - returns base64-encoded content like the real API
      services.wiremock-github-api = {
        enable = true;
        port = ports.github;
        mappings =
          let
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
                fileName = fileName;
                filePath = rootPath + "/app/fixtures/github-packages/effect-4.0.0/${fileName}";
              };
          in
          [
            (effectBase64Response "bower.json")
            (effectBase64Response "LICENSE")

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
      };

      # S3 API mock - serves package tarballs
      services.wiremock-s3-api = {
        enable = true;
        port = ports.s3;
        files = [
          {
            name = "prelude-6.0.1.tar.gz";
            path = rootPath + "/app/fixtures/registry-storage/prelude-6.0.1.tar.gz";
          }
          {
            name = "type-equality-4.0.1.tar.gz";
            path = rootPath + "/app/fixtures/registry-storage/type-equality-4.0.1.tar.gz";
          }
        ];
        mappings = [
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
      };

      # S3 Bucket API mock - handles upload/list operations
      services.wiremock-bucket-api = {
        enable = true;
        port = ports.bucket;
        mappings = [
          {
            request.method = "GET";
            response = {
              status = 200;
              body = ''<ListBucketResult><Contents><Key>prelude/6.0.1.tar.gz</Key><Size>16298</Size><ETag>"abc123"</ETag></Contents><Contents><Key>type-equality/4.0.1.tar.gz</Key><Size>2184</Size><ETag>"def456"</ETag></Contents></ListBucketResult>'';
            };
          }
          {
            request = {
              method = "PUT";
              url = "/effect/4.0.0.tar.gz?x-id=PutObject";
            };
            response = {
              status = 200;
              body = ''<ETag>"abc123"</ETag>'';
            };
          }
          {
            request = {
              method = "PUT";
              url = "/prelude/6.0.1.tar.gz?x-id=PutObject";
            };
            response.status = 500;
          }
        ];
      };

      # Pursuit API mock - documentation hosting
      services.wiremock-pursuit-api = {
        enable = true;
        port = ports.pursuit;
        mappings = [
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
              method = "POST";
              url = "/packages";
            };
            response.status = 201;
          }
        ];
      };
    };

  }
