{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    purix.url = "github:thomashoneyman/purix";
    purix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, purix, ... }:
    let
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      registryOverlay = final: prev: {
        nodejs = prev.nodejs-18_x;

        # We don't want to force everyone to update their configs if they aren't
        # normally on flakes.
        nixFlakes = prev.writeShellScriptBin "nixFlakes" ''
          exec ${prev.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
        '';
      };
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ purix.overlays.default registryOverlay ];
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
        localNpmPackages = pkgs.purix.buildPackageLock { src = ./.; };
        localSpagoPackages = pkgs.purix.buildSpagoLock { src = ./.; };
        run-tests-script = pkgs.writeShellScriptBin "run-tests-script" ''
          set -euo pipefail
          WORKDIR=$(mktemp -d)
          cp spago.yaml spago.lock $WORKDIR
          cp -a app foreign lib scripts $WORKDIR
          pushd $WORKDIR
          ln -s ${localNpmPackages}/js/node_modules .
          cp -r ${localSpagoPackages.registry-app} .
          ${pkgs.spago-unstable}/bin/spago test
          popd
        '';

        # Produces a list of all PureScript binaries supported by purix, ie. those
        # from 0.13 onwards, callable using the naming convention
        # `purs-MAJOR_MINOR_PATCH`.
        #   $ purs-0_14_0 --version
        #   0.14.0
        #
        # To add a new compiler to the list, just update purix:
        #   $ nix flake update
        compilers = let
          # Only include the compiler at normal MAJOR.MINOR.PATCH versions.
          stableOnly = pkgs.lib.filterAttrs
            (name: _: (builtins.match "^purs-[0-9]_[0-9]+_[0-9]$" name != null))
            pkgs.purs-bin;
        in pkgs.symlinkJoin {
          name = "purs-compilers";
          paths = pkgs.lib.mapAttrsToList (name: drv:
            pkgs.writeShellScriptBin name ''
              exec ${drv}/bin/purs "$@"
            '') stableOnly;
        };

        registryApps = pkgs.callPackages ./app { inherit compilers; };
        registryScripts = pkgs.callPackages ./scripts { inherit compilers; };

        mkAppOutput = drv: {
          type = "app";
          program = "${drv}/bin/${drv.name}";
        };
      in rec {
        packages = registryApps // registryScripts;

        apps = pkgs.lib.mapAttrs (_: drv: mkAppOutput drv) packages;

        checks = packages // {
          check-format = pkgs.stdenv.mkDerivation {
            name = "check-format";
            src = ./.;
            buildInputs = [ pkgs.purs-tidy ];
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
          verify-dhall = pkgs.stdenv.mkDerivation {
            name = "verify-dhall";
            src = ./.;
            buildInputs = [ pkgs.dhall pkgs.dhall-json ];
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
        };

        devShells = {
          default = pkgs.mkShell {
            name = "registry-dev";
            packages = with pkgs; [
              # All stable PureScript compilers
              compilers

              # TODO: Hacky, remove when I can run spago test in a pure env
              run-tests-script

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

              # Development tooling
              purs
              spago-unstable
              purs-tidy-unstable
              purs-backend-es-unstable
            ];
          };
        };
      });
}
