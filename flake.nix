{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";

    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    mkSpagoDerivation.inputs.nixpkgs.follows = "nixpkgs";
    mkSpagoDerivation.inputs.ps-overlay.follows = "purescript-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      purescript-overlay,
      mkSpagoDerivation,
      ...
    }:
    let
      inherit (nixpkgs.lib) fileset;

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      pureScriptFileset = fileset.intersection (fileset.gitTracked ./.) (
        fileset.unions [
          ./app
          ./app-e2e
          ./dashboard
          ./db/schema.sql
          ./foreign
          ./lib
          ./scripts
          ./test-utils
          (fileset.maybeMissing ./check)
          ./spago.lock
          ./spago.yaml
        ]
      );

      npmFileset = fileset.intersection (fileset.gitTracked ./.) (
        fileset.fileFilter (f: f.name == "package.json" || f.name == "package-lock.json") ./.
      );

      # We disable git from entering interactive mode at any time, as there is no
      # one there to answer prompts.
      GIT_TERMINAL_PROMPT = 0;

      # Build sources with filesets
      spagoSrc = fileset.toSource {
        root = ./.;
        fileset = pureScriptFileset;
      };

      npmSrc = fileset.toSource {
        root = ./.;
        fileset = npmFileset;
      };

      # Overlays
      registry-overlay = import ./nix/overlay.nix { inherit spagoSrc npmSrc; };
      overlays = [
        purescript-overlay.overlays.default
        mkSpagoDerivation.overlays.default
        registry-overlay
      ];

      # Shared Nix utilities
      nixLib = import ./nix/lib.nix { lib = nixpkgs.lib; };

      # Parse .env.example for devShell defaults
      envDefaults = nixLib.parseEnvFile (builtins.readFile ./.env.example);
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # Process-compose based test environment
        testEnv = import ./nix/test/test-env.nix {
          inherit pkgs;
          lib = pkgs.lib;
          rootPath = self;
        };
      in
      {
        packages =
          pkgs.registry.apps
          // pkgs.registry.scripts
          // {
            dashboard = pkgs.registry-dashboard;
            test-env = testEnv.testEnvScript;
          };

        apps =
          let
            mkApp = name: drv: {
              type = "app";
              program = "${drv}/bin/${drv.name}";
              meta = drv.meta or { };
            };
          in
          pkgs.lib.mapAttrs mkApp (pkgs.registry.apps // pkgs.registry.scripts)
          // {
            test-env = {
              type = "app";
              program = "${testEnv.testEnvScript}/bin/test-env";
              meta.description = "Start the registry test environment with mocked services";
            };
          };

        checks = {
          dashboard = pkgs.registry-dashboard;

          spago-test =
            pkgs.runCommand "spago-test"
              {
                nativeBuildInputs =
                  with pkgs;
                  [
                    nodejs
                    purs
                  ]
                  ++ registry-runtime-deps;
              }
              ''
                cp -r ${pkgs.registry-spago-lock} src && chmod -R +w src && cd src
                ln -s ${pkgs.registry-package-lock}/node_modules .
                node -e "import('./output/Test.Registry.Main/index.js').then(m => m.main())"
                echo "Tests passed!" > $out
              '';

          nix-format =
            pkgs.runCommand "nix-format"
              {
                src = fileset.toSource {
                  root = ./.;
                  fileset = fileset.fileFilter (f: f.hasExt "nix") ./.;
                };
                nativeBuildInputs = [ pkgs.nixfmt-rfc-style ];
              }
              ''
                nixfmt --check $(find $src -type f) && touch $out
              '';

          purescript-format =
            pkgs.runCommand "purescript-format"
              {
                src = spagoSrc;
                nativeBuildInputs = [ pkgs.purs-tidy ];
              }
              ''
                purs-tidy check $src && touch $out
              '';

          # Integration test - exercises the server API
          integration = import ./nix/test/integration.nix {
            inherit pkgs spagoSrc;
            testSupport = testEnv;
          };

          # VM smoke test - verifies deployment without full API testing
          smoke = pkgs.callPackage ./nix/test/smoke.nix {
            inherit overlays;
            rootPath = self;
          };
        };

        devShells.default = pkgs.mkShell {
          name = "registry-dev";

          shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
            if [[ $- == *i* ]]; then
              export SHELL="${pkgs.zsh}/bin/zsh"
              exec "$SHELL"
            fi
          '';

          SERVER_PORT = envDefaults.SERVER_PORT;
          DATABASE_URL = envDefaults.DATABASE_URL;

          inherit GIT_TERMINAL_PROMPT;

          # NOTE: Test-specific env vars (REGISTRY_API_URL, GITHUB_API_URL, PACCHETTIBOTTI_*)
          # are NOT set here to avoid conflicting with .env files used by production scripts.
          # Use `nix run .#test-env` to run E2E tests with mocked services.

          packages =
            with pkgs;
            registry-runtime-deps
            ++ [
              # Development-specific tools
              colmena
              nixfmt-rfc-style
              bash
              nodejs
              prefetch-npm-deps
              esbuild
              jq
              dbmate
              sqlite
              purs
              spago
              purs-tidy-unstable
              purs-backend-es-unstable
              process-compose

              # E2E test runner script - uses same fixed test environment as test-env
              (writeShellScriptBin "spago-test-e2e" ''
                set -euo pipefail
                ${testEnv.testRuntimeExports}
                exec spago run -p registry-app-e2e
              '')
            ];
        };
      }
    )
    // {
      colmena = {
        meta = {
          nixpkgs = import nixpkgs {
            system = "x86_64-linux";
            inherit overlays;
          };
        };

        registry =
          {
            lib,
            modulesPath,
            pkgs,
            ...
          }:
          let
            host = "registry.purescript.org";
          in
          {
            deployment.targetHost = host;
            deployment.buildOnTarget = true;

            imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
              (modulesPath + "/virtualisation/digital-ocean-config.nix")
              ./nix/registry-server.nix
              {
                services.do-agent.enable = true;
                services.registry-server = {
                  enable = true;
                  host = host;
                  envVars = {
                    # These env vars are known to Nix so we set them in advance.
                    # Others, like credentials, must be set in a .env file in
                    # the state directory, unless there are viable defaults.
                    inherit GIT_TERMINAL_PROMPT;
                  };
                };
                system.stateVersion = "24.05";
              }
            ];
          };
      };
    };
}
