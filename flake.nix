{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    spago-nix.url = "github:thomashoneyman/spago-nix";

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
    spago-nix,
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
        overlays = [registryOverlay spago-nix.overlay];
      };

      # Spago workspaces present in the repository
      workspaces = pkgs.spago-nix {src = ./.;};

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
          registry-build = ''
            cd $(git rev-parse --show-toplevel)
            npm ci
            spago build
          '';

          registry-test = ''
            cd $(git rev-parse --show-toplevel)
            npm ci
            spago test
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
            spago run -p registry-scripts -m Registry.Scripts.LegacyImporter -- $@
          '';

          registry-package-set-updater = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-scripts -m Registry.Scripts.PackageSetUpdater -- $@
          '';

          registry-package-transferrer = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-scripts -m Registry.Scripts.PackageTransferrer -- $@
          '';

          registry-package-deleter = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-scripts -m Registry.Scripts.PackageDeleter -- $@
          '';

          registry-solver = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-scripts -m Registry.Scripts.Solver -- $@
          '';

          registry-verify = ''
            cd $(git rev-parse --show-toplevel)
            spago run -p registry-scripts -m Registry.Scripts.VerifyIntegrity -- $@
          '';

          # This script verifies that
          # - all the dhall we have in the repo actually compiles
          # - all the example manifests actually typecheck as Manifests
          registry-verify-dhall = ''
            cd $(git rev-parse --show-toplevel)
            set -euo pipefail

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
        };
      };
    in {
      packages = {
        default = pkgs.stdenv.mkDerivation rec {
          name = "registry-app";
          src = ./app;
          phases = ["buildPhase" "installPhase"];
          nativeBuildInputs = [pkgs.pursPackages.purs pkgs.esbuild];
          buildPhase = ''
            set -f
            purs compile $src/src/**/*.purs ${workspaces.${name}.dependencies.globs}
            set +f
            esbuild ./output/Registry.App.Server/index.js --bundle --outfile=app.js --platform=node --minify
          '';
          installPhase = ''
            mkdir $out
            cp app.js $out
          '';
        };
      };

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
            bash
            nodejs
            jq
            licensee
            coreutils
            gzip

            dhallPackages.dhall-simple
            dhallPackages.dhall-json-simple

            # Development tooling
            pursPackages.purs
            pursPackages.spago-next
            pursPackages.purs-tidy
            nodePackages.bower
          ];
        };
      };
    });
}
