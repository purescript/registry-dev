{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    easy-dhall-nix = {
      url = "github:justinwoo/easy-dhall-nix";
      flake = false;
    };

  };

  outputs =
    { self, nixpkgs, flake-utils, easy-purescript-nix, easy-dhall-nix, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" ];

      registryOverlay = (final: prev: {
        pursPackages = prev.callPackage easy-purescript-nix { };

        dhallPackages = prev.callPackage easy-dhall-nix { };

        nodejs = prev.nodejs-16_x;

        # We don't want to force everyone to update their configs if they aren't
        # normally on flakes.
        nixFlakes = prev.writeShellScriptBin "nixFlakes" ''
          exec ${prev.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
        '';
      });

    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ registryOverlay ];
        };

        # Produces a list of all PureScript binaries supported by easy-purescript-nix,
        # callable using the naming convention `purs-MAJOR_MINOR_PATCH`.
        #   $ purs-0_14_0 --version
        #   0.14.0
        #
        # To add a new compiler to the list, just update easy-purescript-nix:
        #   $ nix flake update
        pursCompilers = let
          compilers = pkgs.lib.filterAttrs
            (name: _: (pkgs.lib.getName name == "purescript"))
            pkgs.pursPackages;

        in pkgs.lib.mapAttrsToList (name: value:
          pkgs.writeShellScriptBin "${name}" ''
            exec ${value}/bin/purs "$@"
          '') compilers;

        # Various scripts we would like to be able to run via a Nix shell
        scripts = pkgs.symlinkJoin {
          name = "scripts";
          paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
            registry-install = ''
              npm ci
              spago install
            '';

            registry-test = ''
              spago test
            '';

            registry-check-format = ''
              purs-tidy check src test
            '';

            registry-api = ''
              spago run -m Registry.API
            '';

            # This script checks that there are no duplicate entries in the two json files listing packages
            registry-verify-unique = ''
              set -euxo pipefail

              total=$(cat bower-packages.json new-packages.json | jq -s "map(length) | add")
              unique=$(cat bower-packages.json new-packages.json | jq -s "map(keys) | add | unique | length")

              if [ "$total" == "$unique" ]; then
                exit 0
              else
                echo "New packages already exist in the registry!"
                exit 1
              fi
            '';

            # This script verifies that
            # - all the dhall we have in the repo actually compiles
            # - all the example manifests actually typecheck as Manifests
            registry-verify-dhall = ''
              set -euo pipefail

              for FILE in $(find v1 -iname "*.dhall")
              do
                echo "Typechecking ''${FILE}";
                dhall <<< "./''${FILE}" > /dev/null
              done

              for FILE in $(find examples -iname "*.json")
              do
                echo "Conforming ''${FILE} to the Manifest type"
                cat "''${FILE}" | json-to-dhall --records-loose --unions-strict "./v1/Manifest.dhall" > /dev/null
              done
            '';
          };
        };

      in {
        devShells = {
          default = pkgs.mkShell {
            name = "registry";
            buildInputs = pursCompilers ++ (with pkgs; [
              # Project tooling
              nixFlakes
              nixfmt
              git
              wget
              bash
              nodejs
              jq
              licensee

              dhallPackages.dhall-simple
              dhallPackages.dhall-json-simple

              # Development tooling
              pursPackages.purs-0_14_7
              pursPackages.spago
              pursPackages.psa
              pursPackages.purs-tidy
              pursPackages.purescript-language-server

              # Helpful utilities
              scripts
            ]);
          };
        };
      });
}
