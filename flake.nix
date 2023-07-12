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

    registryOverlay = final: prev: rec {
      nodejs = prev.nodejs-18_x;

      # We don't want to force everyone to update their configs if they aren't
      # normally on flakes.
      nixFlakes = prev.writeShellScriptBin "nixFlakes" ''
        exec ${prev.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
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
              (prevAttrs.nativeBuildInputs or [] ++ [prev.python3 prev.nodePackages.node-gyp])
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
        compilers = let
          # Only include the compiler at normal MAJOR.MINOR.PATCH versions.
          stableOnly =
            prev.lib.filterAttrs
            (name: _: (builtins.match "^purs-[0-9]_[0-9]+_[0-9]$" name != null))
            prev.purs-bin;
        in
          prev.symlinkJoin {
            name = "purs-compilers";
            paths = prev.lib.mapAttrsToList (name: drv:
              prev.writeShellScriptBin name ''
                exec ${drv}/bin/purs "$@"
              '')
            stableOnly;
          };
      in {
        apps = prev.callPackages ./app {inherit compilers package-lock spago-lock;};
        scripts = prev.callPackages ./scripts {inherit compilers package-lock spago-lock;};
        inherit compilers package-lock spago-lock;
      };
    };
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [purescript-overlay.overlays.default slimlock.overlays.default registryOverlay];
      };

      # We can't import from remote urls in dhall when running in CI, so we
      # fetch the repository and use the local path instead.
      DHALL_PRELUDE = "${builtins.fetchGit {
        url = "https://github.com/dhall-lang/dhall-lang";
        rev = "e35f69d966f205fdc0d6a5e8d0209e7b600d90b3";
      }}/Prelude/package.dhall";

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
        cp -a app foreign lib scripts $WORKDIR
        ln -s ${pkgs.registry.package-lock}/js/node_modules $WORKDIR/node_modules
        pushd $WORKDIR
        ${pkgs.spago-unstable}/bin/spago test
        popd
      '';

      mkAppOutput = drv: {
        type = "app";
        program = "${drv}/bin/${drv.name}";
      };

      # Allows you to run a local VM with the registry server, mimicking the
      # actual deployment.
      run-vm = let
        vm-machine = nixpkgs.lib.nixosSystem {
          system = builtins.replaceStrings ["darwin"] ["linux"] system;
          modules = [
            {nixpkgs.overlays = [purescript-overlay.overlays.default slimlock.overlays.default registryOverlay];}
            ./nix/vm.nix
          ];
        };
      in
        pkgs.writeShellScript "run-vm.sh" ''
          export NIX_DISK_IMAGE=$(mktemp -u -t nixos.qcow2.XXXXXXX)
          trap "rm -f $NIX_DISK_IMAGE" EXIT
          ${vm-machine.config.system.build.vm}/bin/run-registry-vm
        '';
    in rec {
      packages = pkgs.registry.apps; # // pkgs.registry.scripts;

      apps =
        pkgs.lib.mapAttrs (_: drv: mkAppOutput drv) packages
        // {
          default = {
            type = "app";
            program = "${run-vm}";
          };
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
          inherit DHALL_PRELUDE;
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

            for FILE in $(find ./lib/test/_fixtures/manifests -iname "*.json")
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
          else
            pkgs.nixosTest {
              name = "server integration test";
              nodes = {
                registry = {
                  imports = [./nix/module.nix];
                  config = {
                    virtualisation.graphics = false;
                    services.registry-server = {
                      enable = true;
                      host = "localhost";
                      port = 8080;
                      enableCerts = false;
                    };
                  };
                };
                client = {
                  config = {
                    virtualisation.graphics = false;
                  };
                };
              };
              # Test scripts are written in Python:
              # https://nixos.org/manual/nixos/stable/index.html#sec-nixos-tests
              #
              # Note that the python file will be linted, and the test will fail if
              # the script fails the lint — if you see an unexpected failure, check
              # the nix log for errors.
              testScript = ''
                # Machines are available based on their host name, or their name in
                # the "nodes" record if their host name is not set.
                start_all()
                registry.wait_for_unit("server.service")

                client.succeed("sleep 5")

                def succeed_endpoint(endpoint, expected):
                  actual = client.succeed(f"${pkgs.curl}/bin/curl http://registry/api/v1/{endpoint}")
                  if actual != expected:
                    journal = registry.succeed("journalctl -u server.service", timeout=180)
                    print(journal)
                  assert expected == actual, f"Endpoint {endpoint} should return {expected} but returned {actual}"

                print("Testing 'jobs' endpoint...")
                succeed_endpoint("jobs", "[]")
              '';
            };
      };

      devShells = {
        default = pkgs.mkShell {
          name = "registry-dev";
          inherit DHALL_PRELUDE;
          packages = with pkgs; [
            # All stable PureScript compilers
            registry.compilers

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
            overlays = [purescript-overlay.overlays.default slimlock.overlays.default registryOverlay];
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

          # Set 'true' to build on the target machine (necessary if deploying
          # from a non-linux machine).
          deployment.buildOnTarget = false;

          # We import the server module and also the digital ocean configuration
          # necessary to run in a DO droplet.
          imports =
            lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix
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
