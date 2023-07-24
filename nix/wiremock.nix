{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.services.wiremock;
  mappingsFormat = pkgs.formats.json {};
  rootDir = pkgs.linkFarm "wiremock-root" [
    {
      name = "mappings/mappings.json";
      path = mappingsFormat.generate "mappings.json" {
        mappings = cfg.mappings;
      };
    }
  ];
in {
  options.services.wiremock = {
    enable = mkEnableOption "WireMock";

    port = mkOption {
      type = types.int;
      default = 8080;
    };

    verbose = mkOption {
      type = types.bool;
      default = false;
    };

    mappings = mkOption {
      type = mappingsFormat.type;
      description = ''
        See the <https://wiremock.org/docs/stubbing/> for more information.
      '';
      default = [];
      example = [
        {
          request = {
            method = "GET";
            url = "/body";
          };
          response = {
            status = 200;
            headers."Content-Type" = "text/plain";
            body = "Literal text to put in the body";
          };
        }
        {
          request = {
            method = "GET";
            url = "/json";
          };
          response = {
            status = 200;
            jsonBody = {
              someField = "someValue";
            };
          };
        }
      ];
    };
  };

  config = mkIf cfg.enable {
    systemd.services.wiremock = let
      arguments =
        [
          "--port ${toString cfg.port}"
          "--root-dir ${rootDir}"
          "--disable-banner"
        ]
        ++ lib.optional cfg.verbose "--verbose";
    in {
      description = "registry server";
      wantedBy = ["multi-user.target" "nginx.service"];
      serviceConfig = {
        ExecStart = "${pkgs.writeShellScriptBin "wiremock-init" ''
          ${pkgs.wiremock}/bin/wiremock ${lib.concatStringsSep " " arguments} "$@"
        ''}/bin/wiremock-init";
      };
    };
  };
}
