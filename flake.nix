{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # Temporary until spago@next is published as alpha
    easy-purescript-nix = {
      url = "github:f-f/easy-purescript-nix";
      flake = false;
    };

    easy-dhall-nix = {
      url = "github:justinwoo/easy-dhall-nix";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    easy-purescript-nix,
    easy-dhall-nix,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];

    registryOverlay = final: prev: {
      pursPackages = prev.callPackage easy-purescript-nix {};

      dhallPackages = prev.callPackage easy-dhall-nix {};

      nodejs = prev.nodejs-16_x;

      # We don't want to force everyone to update their configs if they aren't
      # normally on flakes.
      nixFlakes = prev.writeShellScriptBin "nixFlakes" ''
        exec ${prev.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
      '';
    };
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [registryOverlay];
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
        pursOnly =
          pkgs.lib.filterAttrs
          (name: _: (builtins.match "^purs-[0-9]_[0-9]+_[0-9]$" name != null))
          pkgs.pursPackages;
      in
        pkgs.symlinkJoin {
          name = "purs-compilers";
          paths = pkgs.lib.mapAttrsToList (name: drv:
            pkgs.writeShellScriptBin name ''
              exec ${drv}/bin/purs "$@"
            '')
          pursOnly;
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
            cd $(git rev-parse --show-toplevel)
            npm ci
            spago install
          '';

          registry-test = ''
            cd $(git rev-parse --show-toplevel)
            npm ci
            spago test -p registry-app
            spago test -p registry-lib
          '';

          registry-check-format = ''
            cd $(git rev-parse --show-toplevel)
            purs-tidy check app lib scripts
          '';

          registry-api = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-app
          '';

          registry-importer = ''
            cd $(git rev-parse --show-toplevel)
            if [ -z "$1" ]; then
              echo "No arguments supplied. Expected one of: generate, update"
              exit 1
            fi

            spago run -p registry-scripts -m Registry.Scripts.LegacyImporter -- $1
          '';

          registry-package-set-updater = ''
            cd $(git rev-parse --show-toplevel)
            if [ -z "$1" ]; then
              echo "No arguments supplied. Expected one of: generate, commit"
              exit 1
            fi

            spago run -p registry-scripts -m Registry.Scripts.PackageSetUpdater -- $1
          '';

          registry-package-transferrer = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-scripts -m Registry.Scripts.PackageTransferrer -- $1
          '';

          # This script verifies that
          # - all the dhall we have in the repo actually compiles
          # - all the example manifests actually typecheck as Manifests
          registry-verify-dhall = ''
            cd $(git rev-parse --show-toplevel)
            set -euo pipefail

            for FILE in $(find v1 -iname "*.dhall")
            do
              echo "Typechecking ''${FILE}";
              dhall <<< "./''${FILE}" > /dev/null
            done

            for FILE in $(find lib/test/_fixtures/manifests -iname "*.json")
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
            openssh
            git
            wget
            bash
            nodejs
            jq
            licensee
            coreutils
            gzip

            dhallPackages.dhall-simple
            dhallPackages.dhall-json-simple

            # Development tooling
            pursPackages.purs-0_15_4
            pursPackages.spago-next
            pursPackages.psa
            pursPackages.purs-tidy
            nodePackages.bower
          ];
        };
      };
    });
}
