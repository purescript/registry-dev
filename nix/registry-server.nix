{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.services.registry-server;

  # Convert env vars attrset to .env file format
  envFile = pkgs.writeText ".env" (
    lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k}=${toString v}") cfg.envVars)
  );

  serverInit = pkgs.writeShellScriptBin "registry-server-init" ''
    mkdir -p ${cfg.stateDir}/db

    set -o allexport
    source ${envFile}
    [ -f ${cfg.stateDir}/.env ] && source ${cfg.stateDir}/.env
    set +o allexport

    export DATABASE_URL="sqlite:${cfg.stateDir}/db/registry.sqlite3"

    cd ${pkgs.registry-server}/bin
    ${pkgs.dbmate}/bin/dbmate up

    cd ${cfg.stateDir}
    exec ${pkgs.registry-server}/bin/registry-server
  '';
in
{
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
      description = "The directory to store the registry server state";
    };

    enableCerts = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable Let's Encrypt certificates";
    };

    envVars = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.either lib.types.str (lib.types.either lib.types.int lib.types.path)
      );
      default = { };
      description = "Environment variables for the registry server";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.vim
      pkgs.git
    ];

    nix = {
      gc.automatic = true;
      settings = {
        auto-optimise-store = true;
        substituters = [ "https://cache.garnix.io" ];
        trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
      };
    };

    networking = {
      hostName = "registry";
      firewall.allowedTCPPorts = [
        22
        80
        443
      ];
    };

    users = {
      mutableUsers = false;
      users =
        let
          deployers = import ./deployers.nix;
        in
        lib.mapAttrs (user: attrs: {
          isNormalUser = true;
          home = "/home/${user}";
          extraGroups = [ "wheel" ];
          packages = with pkgs; [
            rsync
            git
            curl
            coreutils
            vim
          ];
          openssh.authorizedKeys.keys = attrs.sshKeys;
        }) deployers;
    };

    systemd.services.server = {
      description = "registry server";
      wantedBy = [
        "multi-user.target"
        "nginx.service"
      ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = "${serverInit}/bin/registry-server-init";
        Type = "simple";
        Restart = "always";
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
                <style>body { font-family: sans-serif; }</style>
              </head>
              <body>
                <h1>PureScript Registry</h1>
                <p>Welcome to the registry server. The API is located at <code>/api/v1.</code></p>
              </body>
              </html>
            '';
          };

          locations."/api".proxyPass = "http://127.0.0.1:${toString cfg.port}";
        };
      };
    };
  };
}
