{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";

    slimlock.url = "github:thomashoneyman/slimlock";
    slimlock.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    purescript-overlay,
    slimlock,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];

    # Users authorized to deploy to the registry.
    deployers = import ./nix/deployers.nix;

    # We can't import from remote urls in dhall when running in CI or other
    # network-restricted environments, so we fetch the repository and use the
    # local path instead.
    DHALL_PRELUDE = "${
      builtins.fetchGit {
        url = "https://github.com/dhall-lang/dhall-lang";
        rev = "e35f69d966f205fdc0d6a5e8d0209e7b600d90b3";
      }
    }/Prelude/package.dhall";

    DHALL_TYPES = ./types;

    registryOverlay = final: prev: rec {
      nodejs = prev.nodejs-18_x;

      # We don't want to force everyone to update their configs if they aren't
      # normally on flakes.
      nixFlakes = prev.writeShellScriptBin "nixFlakes" ''
        exec ${prev.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
      '';

      # Detects arguments to 'git' containing a URL and replaces them with a
      # local filepath. This is a drop-in replacement for 'git' that should be
      # used in offline / test environments when we only want fixture data.
      gitMock = let
        nodeScript = script:
          prev.writeScript "node-cmd" ''
            ${nodejs}/bin/node -e "${script}" "$@"
          '';

        mock = nodeScript ''
          const { URL } = require('url');
          const { spawn } = require('child_process');

          const repoFixturesDir = process.env.REPO_FIXTURES_DIR;
          if (!repoFixturesDir) {
            throw new Error('REPO_FIXTURES_DIR is not set, but is required.');
          }

          // Replace any URL arguments with the local fixtures path.
          function replaceIfUrl(arg) {
            try {
              const url = new URL(arg);
              const path = url.pathname.replace(/\.git$/, ''');
              const file = 'file://' + repoFixturesDir + path;
              console.log(file);
              return file;
            } catch (e) {
              // Not a URL, ignore
            }
            return arg;
          }

          const args = process.argv.slice(1);
          const modified = [];
          for (let i = 0; i < args.length; i++) {
            const arg = args[i];
            modified.push(replaceIfUrl(arg));
          }

          const git = spawn('${prev.git}/bin/git', modified);

          git.stdout.on('data', (data) => {
            console.log(data.toString('utf8'));
          });

          git.stderr.on('data', (data) => {
            console.error(data.toString('utf8'));
          });

          git.on('close', (code) => {
            if (code !== 0) {
              throw new Error('git exited with code ' + code);
            }
          });
        '';
      in
        prev.writeShellScriptBin "git" ''
          exec ${mock} "$@"
        '';

      # Packages associated with the registry, ie. in this repository.
      registry = let
        spago-lock = prev.purix.buildSpagoLock {
          src = ./.;
          corefn = true;
        };

        package-lock =
          (prev.slimlock.buildPackageLock {
            src = ./.;
            omit = ["dev" "peer"];
          })
          # better-sqlite3 relies on node-gyp and python3 in the build environment, so
          # we add those to the native build inputs.
          .overrideAttrs (finalAttrs: prevAttrs: {
            nativeBuildInputs =
              (prevAttrs.nativeBuildInputs
                or []
                ++ [prev.python3 prev.nodePackages.node-gyp])
              ++ (
                if prev.stdenv.isDarwin
                then [prev.darwin.cctools]
                else []
              );
          });

        # Produces a list of all PureScript binaries supported by purescript-overlay,
        # ie. those from 0.13 onwards, callable using the naming convention
        # `purs-MAJOR_MINOR_PATCH`.
        #   $ purs-0_14_0 --version
        #   0.14.0
        #
        # To add a new compiler to the list, just update the flake:
        #   $ nix flake update
        supportedCompilers = prev.lib.filterAttrs (name: _: (builtins.match "^purs-[0-9]+_[0-9]+_[0-9]+$" name != null)) prev.purs-bin;

        # An attrset containing all the PureScript binaries we want to make
        # available.
        compilers = prev.symlinkJoin {
          name = "purs-compilers";
          paths = prev.lib.mapAttrsToList (name: drv:
            prev.writeShellScriptBin name ''
              exec ${drv}/bin/purs "$@"
            '')
          supportedCompilers;
        };

        purs-versions = prev.writeShellScriptBin "purs-versions" ''
          echo ${prev.lib.concatMapStringsSep " " (x: prev.lib.removePrefix "purs-" (builtins.replaceStrings ["_"] ["."] x)) (prev.lib.attrNames supportedCompilers)}
        '';
      in {
        apps = prev.callPackages ./app {
          inherit compilers purs-versions package-lock spago-lock;
        };
        scripts = prev.callPackages ./scripts {
          inherit compilers purs-versions package-lock spago-lock;
        };
        inherit purs-versions compilers package-lock spago-lock;
      };
    };
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          purescript-overlay.overlays.default
          slimlock.overlays.default
          registryOverlay
        ];
      };

      # We can't run 'spago test' in our flake checks because it tries to
      # write to a cache and I can't figure out how to disable it. Instead
      # we supply it as a shell script.
      #
      # Once we can run 'spago test --offline' or something similar, then this
      # should just be a normal derivation that links the node_modules, copies
      # the output dir locally, and runs 'spago test'.
      #
      # $ nix develop --command run-tests-script
      run-tests-script = pkgs.writeShellScriptBin "run-tests-script" ''
        set -euo pipefail
        WORKDIR=$(mktemp -d)
        cp spago.yaml spago.lock $WORKDIR
        cp -a app foreign lib scripts types $WORKDIR
        ln -s ${pkgs.registry.package-lock}/js/node_modules $WORKDIR/node_modules
        pushd $WORKDIR
        ${pkgs.spago-unstable}/bin/spago test
        popd
      '';

      mkAppOutput = drv: {
        type = "app";
        program = "${drv}/bin/${drv.name}";
      };

      # A full set of environment variables, each set to their default values
      # according to the env.example file, or to the values explicitly set below
      # (e.g. DHALL_PRELUDE and DHALL_TYPES).
      defaultEnv = parseEnv ./.env.example // {inherit DHALL_PRELUDE DHALL_TYPES;};

      # Parse a .env file, skipping empty lines and comments, into Nix attrset
      parseEnv = path: let
        # Filter out lines only containing whitespace or comments
        lines = pkgs.lib.splitString "\n" (builtins.readFile path);
        noEmpties = builtins.filter (line: builtins.match "^[[:space:]]*$" line == null) lines;
        noComments = builtins.filter (line: builtins.match "^#.*$" line == null) noEmpties;
        toKeyPair = line: let
          parts = pkgs.lib.splitString "=" line;
        in {
          name = builtins.head parts;
          value = pkgs.lib.concatStrings (builtins.tail parts);
        };
      in
        builtins.listToAttrs (builtins.map toKeyPair noComments);

      # Print an attrset of env vars { ENV_VAR = "value"; } as a newline-delimited
      # string of "ENV_VAR=value" lines.
      printEnv = env: pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (name: value: "${name}=${value}") env);

      # Allows you to run a local VM with the registry server, mimicking the
      # actual deployment.
      run-vm = let
        vm-machine = nixpkgs.lib.nixosSystem {
          system = builtins.replaceStrings ["darwin"] ["linux"] system;
          modules = [
            {
              nixpkgs.overlays = [
                purescript-overlay.overlays.default
                slimlock.overlays.default
                registryOverlay
              ];
            }
            ./nix/test-vm.nix
            {
              services.registry-server = {
                enable = true;
                host = "localhost";
                port = 8080;
                enableCerts = false;
                # Note: the default credentials are not valid, so you cannot
                # actually publish packages, etc. without overriding the relevant
                # env vars below.
                envVars = defaultEnv;
              };
            }
          ];
        };
      in
        pkgs.writeShellScript "run-vm.sh" ''
          export NIX_DISK_IMAGE=$(mktemp -u -t nixos.qcow2.XXXXXXX)
          trap "rm -f $NIX_DISK_IMAGE" EXIT
          ${vm-machine.config.system.build.vm}/bin/run-registry-vm
        '';
    in rec {
      packages = pkgs.registry.apps // pkgs.registry.scripts;

      apps =
        pkgs.lib.mapAttrs (_: drv: mkAppOutput drv) packages
        // {
          default.type = "app";
          default.program = "${run-vm}";
        };

      checks = {
        check-format = pkgs.stdenv.mkDerivation {
          name = "check-format";
          src = ./.;
          buildInputs = [pkgs.purs-tidy];
          buildPhase = ''
            set -e
            purs-tidy check app foreign lib scripts
          '';
          installPhase = ''
            mkdir $out
          '';
        };

        # This script verifies that
        # - all the dhall we have in the repo actually compiles
        # - all the example manifests actually typecheck as Manifests
        verify-dhall = pkgs.stdenv.mkDerivation rec {
          name = "verify-dhall";
          src = ./.;
          inherit DHALL_PRELUDE DHALL_TYPES;
          buildInputs = [pkgs.dhall pkgs.dhall-json];
          buildPhase = ''
            set -euo pipefail

            mkdir -p cache/dhall
            export XDG_CACHE_HOME="$PWD/cache"

            for FILE in $(find ./types/v1 -iname "*.dhall")
            do
              echo "Typechecking ''${FILE}";
              dhall <<< "./''${FILE}" > /dev/null
            done

            for FILE in $(find ./lib/fixtures/manifests -iname "*.json")
            do
              echo "Conforming ''${FILE} to the Manifest type"
              cat "''${FILE}" | json-to-dhall --records-loose --unions-strict "./types/v1/Manifest.dhall" > /dev/null
            done
          '';

          installPhase = ''
            mkdir $out
          '';
        };

        # This is an integration test that will run the server and allow us to
        # test it by sending API requests. You can run only this check with:
        # nix build .#checks.${your-system}.integration
        integration =
          if pkgs.stdenv.isDarwin
          then
            pkgs.runCommand "integration-disabled" {} ''
              mkdir $out
              echo "Integration tests are not supported on macOS systems, skipping..."
              exit 0
            ''
          else let
            serverPort = 8080;
            githubPort = 9001;
            bucketPort = 9002;
            s3Port = 9003;
            pursuitPort = 9004;
            stateDir = "/var/lib/registry-server";
            envVars =
              defaultEnv
              // {
                # We override all remote APIs with their local wiremock ports
                GITHUB_API_URL = "http://localhost:${toString githubPort}";
                S3_API_URL = "http://localhost:${toString s3Port}";
                S3_BUCKET_URL = "http://localhost:${toString bucketPort}";
                PURSUIT_API_URL = "http://localhost:${toString pursuitPort}";

                # We add an extra env var for the mock git applicaiton to know
                # where the fixtures are.
                REPO_FIXTURES_DIR = "${stateDir}/repo-fixtures";
              };
          in
            pkgs.nixosTest {
              name = "server integration test";
              nodes = {
                registry = {
                  imports = [
                    (import ./nix/wiremock.nix {service = "github-api";})
                    (import ./nix/wiremock.nix {service = "s3-api";})
                    (import ./nix/wiremock.nix {service = "bucket-api";})
                    (import ./nix/wiremock.nix {service = "pursuit-api";})
                    ./nix/module.nix
                  ];
                  config = {
                    nixpkgs.overlays = [
                      # We need to ensure that the server is using the mock git
                      # binary instead of the real one. We do not, however, want
                      # to override 'git' in nixpkgs because that would make us
                      # rebuild everything that depends on git.
                      (_: prev: {registry.apps.server = prev.registry.apps.server.override {git = prev.gitMock;};})
                    ];

                    virtualisation.graphics = false;

                    services.registry-server = {
                      enable = true;
                      host = "localhost";
                      port = serverPort;
                      enableCerts = false;
                      stateDir = stateDir;
                      envVars = envVars;
                    };

                    services.wiremock-github-api = {
                      enable = true;
                      port = githubPort;
                      mappings = [
                        {
                          request = {
                            method = "GET";
                            url = "/repos/purescript/purescript-effect/contents/bower.json?ref=v4.0.0";
                          };
                          response = {
                            status = 200;
                            headers."Content-Type" = "application/json";
                            jsonBody = {
                              type = "file";
                              encoding = "base64";
                              content = "ewogICJuYW1lIjogInB1cmVzY3JpcHQtZWZmZWN0IiwKICAiaG9tZXBhZ2Ui\nOiAiaHR0cHM6Ly9naXRodWIuY29tL3B1cmVzY3JpcHQvcHVyZXNjcmlwdC1l\nZmZlY3QiLAogICJsaWNlbnNlIjogIkJTRC0zLUNsYXVzZSIsCiAgInJlcG9z\naXRvcnkiOiB7CiAgICAidHlwZSI6ICJnaXQiLAogICAgInVybCI6ICJodHRw\nczovL2dpdGh1Yi5jb20vcHVyZXNjcmlwdC9wdXJlc2NyaXB0LWVmZmVjdC5n\naXQiCiAgfSwKICAiaWdub3JlIjogWwogICAgIioqLy4qIiwKICAgICJib3dl\ncl9jb21wb25lbnRzIiwKICAgICJub2RlX21vZHVsZXMiLAogICAgIm91dHB1\ndCIsCiAgICAidGVzdCIsCiAgICAiYm93ZXIuanNvbiIsCiAgICAicGFja2Fn\nZS5qc29uIgogIF0sCiAgImRlcGVuZGVuY2llcyI6IHsKICAgICJwdXJlc2Ny\naXB0LXByZWx1ZGUiOiAiXjYuMC4wIgogIH0KfQo=\n";
                            };
                          };
                        }
                        {
                          request = {
                            method = "GET";
                            url = "/repos/purescript/purescript-effect/contents/LICENSE?ref=v4.0.0";
                          };
                          response = {
                            status = 200;
                            headers."Content-Type" = "application/json";
                            jsonBody = {
                              type = "file";
                              encoding = "base64";
                              content = "Q29weXJpZ2h0IDIwMTggUHVyZVNjcmlwdAoKUmVkaXN0cmlidXRpb24gYW5k\nIHVzZSBpbiBzb3VyY2UgYW5kIGJpbmFyeSBmb3Jtcywgd2l0aCBvciB3aXRo\nb3V0IG1vZGlmaWNhdGlvbiwKYXJlIHBlcm1pdHRlZCBwcm92aWRlZCB0aGF0\nIHRoZSBmb2xsb3dpbmcgY29uZGl0aW9ucyBhcmUgbWV0OgoKMS4gUmVkaXN0\ncmlidXRpb25zIG9mIHNvdXJjZSBjb2RlIG11c3QgcmV0YWluIHRoZSBhYm92\nZSBjb3B5cmlnaHQgbm90aWNlLCB0aGlzCmxpc3Qgb2YgY29uZGl0aW9ucyBh\nbmQgdGhlIGZvbGxvd2luZyBkaXNjbGFpbWVyLgoKMi4gUmVkaXN0cmlidXRp\nb25zIGluIGJpbmFyeSBmb3JtIG11c3QgcmVwcm9kdWNlIHRoZSBhYm92ZSBj\nb3B5cmlnaHQgbm90aWNlLAp0aGlzIGxpc3Qgb2YgY29uZGl0aW9ucyBhbmQg\ndGhlIGZvbGxvd2luZyBkaXNjbGFpbWVyIGluIHRoZSBkb2N1bWVudGF0aW9u\nIGFuZC9vcgpvdGhlciBtYXRlcmlhbHMgcHJvdmlkZWQgd2l0aCB0aGUgZGlz\ndHJpYnV0aW9uLgoKMy4gTmVpdGhlciB0aGUgbmFtZSBvZiB0aGUgY29weXJp\nZ2h0IGhvbGRlciBub3IgdGhlIG5hbWVzIG9mIGl0cyBjb250cmlidXRvcnMK\nbWF5IGJlIHVzZWQgdG8gZW5kb3JzZSBvciBwcm9tb3RlIHByb2R1Y3RzIGRl\ncml2ZWQgZnJvbSB0aGlzIHNvZnR3YXJlIHdpdGhvdXQKc3BlY2lmaWMgcHJp\nb3Igd3JpdHRlbiBwZXJtaXNzaW9uLgoKVEhJUyBTT0ZUV0FSRSBJUyBQUk9W\nSURFRCBCWSBUSEUgQ09QWVJJR0hUIEhPTERFUlMgQU5EIENPTlRSSUJVVE9S\nUyAiQVMgSVMiIEFORApBTlkgRVhQUkVTUyBPUiBJTVBMSUVEIFdBUlJBTlRJ\nRVMsIElOQ0xVRElORywgQlVUIE5PVCBMSU1JVEVEIFRPLCBUSEUgSU1QTElF\nRApXQVJSQU5USUVTIE9GIE1FUkNIQU5UQUJJTElUWSBBTkQgRklUTkVTUyBG\nT1IgQSBQQVJUSUNVTEFSIFBVUlBPU0UgQVJFCkRJU0NMQUlNRUQuIElOIE5P\nIEVWRU5UIFNIQUxMIFRIRSBDT1BZUklHSFQgSE9MREVSIE9SIENPTlRSSUJV\nVE9SUyBCRSBMSUFCTEUgRk9SCkFOWSBESVJFQ1QsIElORElSRUNULCBJTkNJ\nREVOVEFMLCBTUEVDSUFMLCBFWEVNUExBUlksIE9SIENPTlNFUVVFTlRJQUwg\nREFNQUdFUwooSU5DTFVESU5HLCBCVVQgTk9UIExJTUlURUQgVE8sIFBST0NV\nUkVNRU5UIE9GIFNVQlNUSVRVVEUgR09PRFMgT1IgU0VSVklDRVM7CkxPU1Mg\nT0YgVVNFLCBEQVRBLCBPUiBQUk9GSVRTOyBPUiBCVVNJTkVTUyBJTlRFUlJV\nUFRJT04pIEhPV0VWRVIgQ0FVU0VEIEFORCBPTgpBTlkgVEhFT1JZIE9GIExJ\nQUJJTElUWSwgV0hFVEhFUiBJTiBDT05UUkFDVCwgU1RSSUNUIExJQUJJTElU\nWSwgT1IgVE9SVAooSU5DTFVESU5HIE5FR0xJR0VOQ0UgT1IgT1RIRVJXSVNF\nKSBBUklTSU5HIElOIEFOWSBXQVkgT1VUIE9GIFRIRSBVU0UgT0YgVEhJUwpT\nT0ZUV0FSRSwgRVZFTiBJRiBBRFZJU0VEIE9GIFRIRSBQT1NTSUJJTElUWSBP\nRiBTVUNIIERBTUFHRS4K\n";
                            };
                          };
                        }
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

                    services.wiremock-s3-api = {
                      enable = true;
                      port = s3Port;
                      files = [
                        {
                          name = "prelude-6.0.1.tar.gz";
                          path = ./app/fixtures/registry-storage/prelude-6.0.1.tar.gz;
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
                      ];
                    };

                    services.wiremock-bucket-api = {
                      enable = true;
                      port = bucketPort;
                      mappings = [
                        {
                          request = {
                            method = "GET";
                          };
                          response = {
                            status = 200;
                            body = "<ListBucketResult><Contents><Key>prelude/6.0.1.tar.gz</Key><Size>16298</Size><ETag>\"abc123\"</ETag></Contents></ListBucketResult>";
                          };
                        }
                        # We don't expect that effect-4.0.0 has been uploaded.
                        {
                          request = {
                            method = "PUT";
                            url = "/effect/4.0.0.tar.gz";
                          };
                          response = {
                            status = 200;
                            body = "<ETag>\"abc123\"</ETag>";
                          };
                        }
                        # But we do expect that prelude has been uploaded and
                        # can't be uploaded again.
                        {
                          request = {
                            method = "PUT";
                            url = "/prelude/6.0.1.tar.gz";
                          };
                          response = {
                            status = 500;
                          };
                        }
                      ];
                    };

                    services.wiremock-pursuit-api = {
                      enable = true;
                      port = pursuitPort;
                      mappings = [
                        # Already-published packages, ie. the registry-storage
                        # tarballs.
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
                        # The result of publishing a package, which we hardcode
                        # to 201 (success) for now.
                        {
                          request = {
                            method = "POST";
                            url = "/packages";
                          };
                          response = {
                            status = 201;
                          };
                        }
                      ];
                    };
                  };
                };
                client = {config = {virtualisation.graphics = false;};};
              };

              # Test scripts are written in Python:
              # https://nixos.org/manual/nixos/stable/index.html#sec-nixos-tests
              #
              # Note that the python file will be linted, and the test will fail if
              # the script fails the lint — if you see an unexpected failure, check
              # the nix log for errors.
              testScript = let
                setupGitFixtures = pkgs.writeShellScriptBin "setup-git-fixtures" ''
                  set -e

                  mkdir -p ${envVars.REPO_FIXTURES_DIR}/purescript

                  git config --global user.email "pacchettibotti@purescript.org"
                  git config --global user.name "pacchettibotti"
                  git config --global init.defaultBranch "master"

                  # First the registry-index repo
                  cp -r ${./app/fixtures/registry-index} ${envVars.REPO_FIXTURES_DIR}/purescript/registry-index

                  # Then the registry repo
                  cp -r ${./app/fixtures/registry} ${envVars.REPO_FIXTURES_DIR}/purescript/registry

                  # Finally, the legacy package-sets repo
                  cp -r ${./app/fixtures/package-sets} ${envVars.REPO_FIXTURES_DIR}/purescript/package-sets

                  # Next, we set up arbitrary Git repos that should be available
                  cp -r ${./app/fixtures/github-packages/effect-4.0.0} ${envVars.REPO_FIXTURES_DIR}/purescript/purescript-effect

                  # Then we initialize the repos
                  for REPO in ${envVars.REPO_FIXTURES_DIR}/purescript/*/
                  do
                    pushd $REPO
                    echo "Initializing $REPO"
                    git init
                    git add .
                    git commit -m "Fixture commit"
                    # Necessary so you can push to the upstream on the same branch
                    # as you are currently on. Wrecks the tree for the upstream,
                    # but this is acceptable for testing.
                    git config receive.denyCurrentBranch ignore
                    popd
                  done

                  # Then we fixup the repos that need tags
                  pushd ${envVars.REPO_FIXTURES_DIR}/purescript/package-sets
                  git tag -m "psc-0.15.4-20230105" psc-0.15.4-20230105
                  popd

                  pushd ${envVars.REPO_FIXTURES_DIR}/purescript/purescript-effect
                  git tag -m "v4.0.0" v4.0.0
                  popd
                '';

                publish_effect = pkgs.writeText "publish-effect-4.0.0.json" ''
                  {
                    "name": "effect",
                    "ref": "v4.0.0",
                    "compiler": "0.15.4",
                    "location": {
                      "githubOwner": "purescript",
                      "githubRepo": "purescript-effect"
                    }
                  }
                '';
              in ''
                import json
                import time

                ##########
                #
                # SETUP
                #
                ##########

                # We set up the git fixtures
                registry.start()
                print(registry.succeed("${setupGitFixtures}/bin/setup-git-fixtures"))

                # We wait for the server to start up and for the client to be able to reach it.
                registry.wait_for_unit("wiremock-github-api.service")
                registry.wait_for_unit("wiremock-s3-api.service")
                registry.wait_for_unit("wiremock-bucket-api.service")
                registry.wait_for_unit("wiremock-pursuit-api.service")
                registry.wait_for_unit("server.service")

                # Give time for all the various services to come up...
                client.start()
                client.wait_until_succeeds("${pkgs.curl}/bin/curl --fail-with-body http://registry/api/v1/jobs", timeout=20)

                ##########
                #
                # TESTS
                #
                ##########

                # First we initiate the call to publish
                print("POST /publish")
                publish_result = json.loads(client.succeed("${pkgs.curl}/bin/curl -L -X POST -d '@${publish_effect}' http://registry/api/v1/publish --header 'Content-Type:application/json'"))
                print(publish_result)
                job_id = publish_result['jobId']
                assert len(job_id) == 36, f"POST /publish should return a 36-char job id, but returned {publish_result}"

                # Then we poll for job results, expecting an eventual 'success'.
                try_count = 0
                delay_seconds = 3
                prev_timestamp = "2023-07-29T00:00:00.000Z"
                log_level = "DEBUG"
                while True:
                  print(f"Requesting job information for job {job_id}")
                  poll_result = json.loads(client.succeed(f"${pkgs.curl}/bin/curl -L http://registry/api/v1/jobs/{job_id}?since={prev_timestamp}&level={log_level}"))
                  print(poll_result)
                  if "finishedAt" in poll_result:
                    print("Job has completed!")
                    success = poll_result['success']
                    assert success, f"GET /jobs/{job_id} should return success, but it returned {poll_result}"
                    break
                  elif (try_count * delay_seconds) > 60:
                    raise ValueError(f"Cancelling publish request after {try_count * delay_seconds} seconds, this is too long...")
                  else:
                    print(f"Job is still ongoing, retrying in {delay_seconds} seconds...")
                    time.sleep(delay_seconds)
                    try_count = try_count + 1
              '';
            };
      };

      devShells = {
        default = pkgs.mkShell {
          name = "registry-dev";
          packages = with pkgs; [
            # All stable PureScript compilers
            registry.compilers
            registry.purs-versions

            # TODO: Hacky, remove when I can run spago test in a pure env
            run-tests-script

            # Deployment
            colmena

            # Project tooling
            nixFlakes
            nixfmt
            git
            bash
            nodejs
            jq
            licensee
            coreutils
            gzip
            gnutar
            dhall
            dhall-json
            dbmate

            # Development tooling
            purs
            spago-unstable
            purs-tidy-unstable
            purs-backend-es-unstable
          ];
        };
      };
    })
    # Separated because this is not supported for all systems.
    // {
      # Deployment specification for the registry server
      colmena = {
        meta = {
          nixpkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [
              purescript-overlay.overlays.default
              slimlock.overlays.default
              registryOverlay
            ];
          };
        };
        # The registry server
        registry = {
          lib,
          modulesPath,
          ...
        }: let
          host = "registry.purescript.org";
        in {
          deployment.targetHost = host;
          deployment.buildOnTarget = true;

          # We import the server module and also the digital ocean configuration
          # necessary to run in a DO droplet.
          imports =
            lib.optional (builtins.pathExists ./do-userdata.nix)
            ./do-userdata.nix
            ++ [
              (modulesPath + "/virtualisation/digital-ocean-config.nix")
              ./nix/module.nix
              # Extra config for the deployed server only.
              {
                # Enable Digital Ocean monitoring
                services.do-agent.enable = true;

                # Enable the registry server
                services.registry-server.enable = true;
                services.registry-server.host = host;

                # Don't change this.
                system.stateVersion = "23.05";
              }
            ];
        };
      };
    };
}
