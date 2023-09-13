{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.services.registry-server;
in {
  options.services.registry-server = {
    enable = lib.mkEnableOption "registry server service";

    host = lib.mkOption {
      type = lib.types.str;
      default = "registry.purescript.org";
      description = "The hostname of the registry server";
    };

    port = lib.mkOption {
      type = lib.types.int;
      default = 8080;
      description = "The port to run the registry server on";
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/registry-server";
      description = "The directory to store the registry server state (database, etc.)";
    };

    enableCerts = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable Let's Encrypt certificates for the registry server";
    };

    envVars = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.str (lib.types.either lib.types.int lib.types.path));
      default = {};
      description = "Environment variables to set for the registry server";
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [pkgs.vim pkgs.git];
    };

    nix = {
      gc.automatic = true;
      settings = {
        auto-optimise-store = true;
        # https://garnix.io/docs/caching
        substituters = ["https://cache.garnix.io"];
        trusted-public-keys = ["cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="];
      };
    };

    networking = {
      hostName = "registry";
      firewall.allowedTCPPorts = [22 80 443];
    };

    users = {
      mutableUsers = false;

      users = let
        deployers = import ./deployers.nix;
      in
        pkgs.lib.mapAttrs (user: attrs: {
          isNormalUser = true;
          home = "/home/${user}";
          extraGroups = ["wheel"];
          packages = [pkgs.rsync pkgs.git pkgs.curl pkgs.coreutils pkgs.vim];
          openssh.authorizedKeys.keys = attrs.sshKeys;
        })
        deployers;
    };

    systemd.services = let
      # Print an attrset of env vars { ENV_VAR = "value"; } as a newline-delimited
      # string of "ENV_VAR=value" lines, then write the text to the Nix store.
      printEnv = vars:
        pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (name: value:
          if (builtins.typeOf value == "int")
          then "${name}=${toString value}"
          else "${name}=${value}")
        vars);
      defaultEnvFile =
        pkgs.writeText ".env" (printEnv cfg.envVars);
    in {
      server = {
        description = "registry server";
        wantedBy = ["multi-user.target" "nginx.service"];
        serviceConfig = {
          ExecStart = "${pkgs.writeShellScriptBin "registry-server-init" ''
            # Ensure the state directory is available and initialize the database
            mkdir -p ${cfg.stateDir}/db

            # Initialize environment variables
            set -o allexport
            source ${defaultEnvFile}

            # If a .env file exists in the stateDir then we will use it instead;
            # this overwrites the cfg.envVars settings.
            if [ -f ${cfg.stateDir}/.env ]; then
              echo "Production .env file found! Values will overwrite the defaults."
              source ${cfg.stateDir}/.env
            fi
            set +o allexport

            export DATABASE_URL="sqlite:${cfg.stateDir}/db/registry.sqlite3"
            pushd ${pkgs.registry.apps.server}/bin
            ${pkgs.dbmate}/bin/dbmate up
            popd

            echo "Starting registry server..."
            ${pkgs.registry.apps.server}/bin/registry-server
          ''}/bin/registry-server-init";
        };
      };
    };

    swapDevices = [
      {
        device = "/var/lib/swap";
        size = 4096;
      }
    ];

    security.acme = lib.mkIf cfg.enableCerts {
      acceptTerms = true;
      defaults.email = "hello@thomashoneyman.com";
    };

    services = {
      openssh.settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "yes";
      };

      nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts.${cfg.host} = {
          forceSSL = cfg.enableCerts;
          enableACME = cfg.enableCerts;

          locations."/" = {
            index = "index.html";
            root = pkgs.writeTextDir "index.html" ''
              <html>
              <head>
                <title>PureScript Registry</title>
                <style>
                  body {
                    font-family: sans-serif;
                  }
                </style>
              </head>
              <body>
                <h1>PureScript Registry</h1>
                <p>Welcome to the registry server. The API is located at <code>/api/v1.</code></p>
              </body>
              </html>
            '';
          };

          locations."/api" = {
            proxyPass = "http://127.0.0.1:${toString cfg.port}";
          };
        };
      };
    };
  };
}
