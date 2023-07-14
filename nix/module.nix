{pkgs, ...}: let
  host = "registry.purescript.org";
  port = 8080;
in {
  environment = {
    systemPackages = [
      pkgs.vim
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

  systemd.services = {
    server = {
      description = "registry server";
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        # FIXME: We need to pass these in correctly instead of using dummy
        # values. Explore Nix secrets solutions (agenix, etc.) for this.
        ExecStart = "${pkgs.writeShellScriptBin "registry-server-init" ''
          # Dummy env vars for the test server.
          export PACCHETTIBOTTI_TOKEN="ghp_XXX"
          export SPACES_KEY="abcxyz"
          export SPACES_SECRET="abcxyz"

          # These two are base64-encoded.
          # The first is 'ssh-ed25519 abcxyz pacchettibotti@purescript.org"
          # The second is 'abcxyz'
          export PACCHETTIBOTTI_ED25519_PUB="c3NoLWVkMjU1MTkgYWJjeHl6IHBhY2NoZXR0aWJvdHRpQHB1cmVzY3JpcHQub3Jn"
          export PACCHETTIBOTTI_ED25519="YWJjeHl6"

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
            <body>
              Welcome to the registry server. The API is located at /api. Example:

                  curl localhost:${toString port}/api/v1/jobs/0

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
