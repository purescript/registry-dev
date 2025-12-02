{ service }:
{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.services."wiremock-${service}";
  mappingsFormat = pkgs.formats.json { };
  rootDir =
    let
      mappingsJson = mappingsFormat.generate "mappings.json" { mappings = cfg.mappings; };
    in
    pkgs.runCommand "wiremock-root"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
      }
      ''
        mkdir -p $out
        cd $out

        mkdir mappings
        cp ${mappingsJson} mappings/mappings.json

        mkdir __files
        ${lib.concatMapStrings (attrs: "cp ${attrs.path} __files/${attrs.name}") cfg.files}
      '';
in
{
  options.services."wiremock-${service}" = {
    enable = mkEnableOption "WireMock";

    port = mkOption {
      type = types.int;
      default = 8080;
    };

    verbose = mkOption {
      type = types.bool;
      default = false;
    };

    files = mkOption {
      description = ''
        List of files to include in the __files directory for access when stubbing.
      '';
      default = [ ];
      example = {
        name = "file-name.json";
        path = "<nix-store path>";
      };
    };

    mappings = mkOption {
      type = mappingsFormat.type;
      description = ''
        See the <https://wiremock.org/docs/stubbing/> for more information.
      '';
      default = [ ];
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
            headers."Content-Type" = "application/json";
            jsonBody = {
              someField = "someValue";
            };
          };
        }
      ];
    };
  };

  config = mkIf cfg.enable {
    systemd.services."wiremock-${service}" =
      let
        arguments = [
          "--port ${toString cfg.port}"
          "--root-dir ${rootDir}"
          "--disable-banner"
        ]
        ++ lib.optional cfg.verbose "--verbose";
      in
      {
        description = "registry server";
        wantedBy = [
          "multi-user.target"
          "nginx.service"
        ];
        serviceConfig = {
          ExecStart = "${pkgs.writeShellScriptBin "wiremock-${service}-init" ''
            ${pkgs.wiremock}/bin/wiremock ${lib.concatStringsSep " " arguments} "$@"
          ''}/bin/wiremock-${service}-init";
        };
      };
  };
}
