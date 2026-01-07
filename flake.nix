{
  description = "The PureScript Registry";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.11";
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
          ./foreign
          ./lib
          ./scripts
          ./test-utils
          (fileset.maybeMissing ./check)
          ./spago.lock
          ./spago.yaml
          ./types
        ]
      );

      npmFileset = fileset.intersection (fileset.gitTracked ./.) (
        fileset.fileFilter (f: f.name == "package.json" || f.name == "package-lock.json") ./.
      );

      # The location of the Dhall type specifications, used to type-check manifests.
      DHALL_TYPES = ./types;
      DHALL_PRELUDE = "${
        builtins.fetchGit {
          url = "https://github.com/dhall-lang/dhall-lang";
          rev = "25cf020ab307cb2d66826b0d1ddac8bc89241e27";
        }
      }/Prelude/package.dhall";

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
          spago-test =
            pkgs.runCommand "spago-test"
              {
                inherit DHALL_TYPES DHALL_PRELUDE;
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

          verify-dhall =
            pkgs.runCommand "verify-dhall"
              {
                src = fileset.toSource {
                  root = ./.;
                  fileset = fileset.unions [
                    ./types
                    ./lib/fixtures/manifests
                  ];
                };
                nativeBuildInputs = with pkgs; [
                  dhall
                  dhall-json
                  parallel
                ];
                inherit DHALL_PRELUDE;
              }
              ''
                mkdir -p cache/dhall && export XDG_CACHE_HOME="$PWD/cache"
                find $src/types/v1 -name "*.dhall" | parallel --will-cite 'dhall <<< {}'
                find $src/lib/fixtures/manifests -name "*.json" | parallel --will-cite \
                  'json-to-dhall --plain --records-loose --unions-strict --file {} $src/types/v1/Manifest.dhall'
                touch $out
              '';

          # Integration test - exercises the server API
          integration = import ./nix/test/integration.nix {
            inherit pkgs spagoSrc testEnv;
          };

          # VM smoke test - verifies deployment without full API testing
          smoke = pkgs.callPackage ./nix/test/smoke.nix {
            inherit overlays;
            rootPath = self;
          };
        };

        devShells.default = pkgs.mkShell {
          name = "registry-dev";

          SERVER_PORT = envDefaults.SERVER_PORT;
          DATABASE_URL = envDefaults.DATABASE_URL;

          # Dhall environment variables needed for manifest typechecking
          inherit DHALL_TYPES DHALL_PRELUDE GIT_TERMINAL_PROMPT;

          # NOTE: Test-specific env vars (REGISTRY_API_URL, GITHUB_API_URL, PACCHETTIBOTTI_*)
          # are NOT set here to avoid conflicting with .env files used by production scripts
          # like legacy-importer. Use `nix run .#test-env` to run E2E tests with mocked services.

          packages =
            with pkgs;
            registry-runtime-deps
            ++ [
              # Development-specific tools
              colmena
              nixfmt-rfc-style
              bash
              nodejs
              jq
              dbmate
              purs
              spago
              purs-tidy-unstable
              purs-backend-es-unstable
              process-compose
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
          { lib, modulesPath, ... }:
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
                    inherit
                      DHALL_PRELUDE
                      DHALL_TYPES
                      GIT_TERMINAL_PROMPT
                      ;
                  };
                };
                system.stateVersion = "24.05";
              }
            ];
          };
      };
    };
}
