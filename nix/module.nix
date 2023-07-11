{pkgs, ...}: let
  host = "registry.purescript.org";
  port = 8080;
in {
  environment = {
    systemPackages = [
      pkgs.vim
      # FIXME: This should be picked up via the buildInputs, surely? It's not.
      pkgs.registry.compilers
    ];
  };

  # https://garnix.io/docs/caching
  nix.settings = {
    substituters = ["https://cache.garnix.io"];
    trusted-public-keys = ["cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="];
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
    stateDir = "/var/lib/registry-server";
    defaultEnv = builtins.readFile ../.env.example;
  in {
    server = {
      description = "registry server";
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        # values. Explore Nix secrets solutions (agenix, etc.) for this.
        ExecStart = "${pkgs.writeShellScriptBin "registry-server-init" ''
          # Ensure the state directory is available
          mkdir -p ${stateDir}/db

          # Initialize environment variables
          set -o allexport
          if [ -f ${stateDir}/.env ]; then
            echo "Using production environment variables"
            source ${stateDir}/.env
          else
            echo "WARNING: No environment variables found in ${stateDir}."
            echo "Continuing with dummy values, not suitable for production."
            cat ${defaultEnv} > .env
            source .env
          fi
          set +o allexport

          # Initialize or migrate the database
          export DATABASE_URL="sqlite:${stateDir}/db/registry.sqlite3"
          pushd ${pkgs.registry.apps.server}/bin
          ${pkgs.dbmate}/bin/dbmate up
          popd

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

      virtualHosts.${host} = {
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
              <p>Welcome to the registry server. The API is located at <code>/api.</code></p>
            </body>
            </html>
          '';
        };

        locations."/api" = {
          proxyPass = "http://127.0.0.1:${toString port}";
        };
      };
    };
  };

  # Match to the version used in the nixos.qcow on digital ocean
  system.stateVersion = "22.05";
}
