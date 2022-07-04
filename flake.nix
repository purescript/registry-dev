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
        compilers = let
          # Only include the compiler at normal MAJOR.MINOR.PATCH versions.
          pursOnly = pkgs.lib.filterAttrs
            (name: _: (builtins.match "^purs-[0-9]_[0-9]+_[0-9]$" name != null))
            pkgs.pursPackages;

        in pkgs.symlinkJoin {
          name = "purs-compilers";
          paths = pkgs.lib.mapAttrsToList (name: drv:
            pkgs.writeShellScriptBin name ''
              exec ${drv}/bin/purs "$@"
            '') pursOnly;
        };

        # Various scripts we would like to be able to run via a Nix shell. Once
        # in a shell via `nix develop`, these can be run, e.g.
        #
        #   $ registry-check-format
        #   All files formatted.
        #
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

            registry-importer = ''
              spago run -m Registry.Scripts.LegacyImport
            '';

            # This script checks that there are no duplicate entries in the two json files listing packages
            registry-verify-unique = ''
              set -euxo pipefail

              total=$(cat bower-packages.json new-packages.json | jq -s "add | length")
              unique_keys=$(cat bower-packages.json new-packages.json | jq -s "add | keys | unique | length")
              unique_values=$(cat bower-packages.json new-packages.json | jq -s "add | to_entries | map(.value) | unique | length")

              if [ "$total" == "$unique_keys" ]; then
                echo "New packages already exist in the registry!"
                exit 1
              else
                if [ "$total" != "$unique_values" ]; then
                  echo "New package URL already exists in the registry!"
                  exit 1
                else
                  exit 0
                fi
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
            packages = with pkgs; [
              # Helpful utilities
              scripts
              compilers

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
            ];
          };
        };
      });
}
