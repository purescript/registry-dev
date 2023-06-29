{pkgs, ...}: let
  port = 8080;
in {
  environment = {
    systemPackages = [
      pkgs.vim

      # FIXME: This should be picked up via the buildInputs, surely? It's not.
      pkgs.registry.compilers
      pkgs.nodejs
    ];
  };

  networking = {
    hostName = "registry";
    firewall.allowedTCPPorts = [22 80 443];
  };

  virtualisation.forwardPorts = [
    {
      from = "host";
      guest.port = 80;
      host.port = port;
    }
  ];

  systemd.services = {
    server = {
      description = "registry server";
      wantedBy = ["multi-user.target"];
      serviceConfig = {
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

  services = {
    # NOTE: Use 'shutdown now' to exit the VM.
    getty.autologinUser = "root";

    nginx = {
      enable = true;

      virtualHosts.localhost = {
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

  system.stateVersion = "23.05";
}
