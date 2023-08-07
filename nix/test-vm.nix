# Machine configuration for the NixOS virtual machine suitable for testing.
{
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = ["${modulesPath}/virtualisation/qemu-vm.nix" ./module.nix];

  config = {
    # https://github.com/utmapp/UTM/issues/2353
    networking.nameservers = lib.mkIf pkgs.stdenv.isDarwin ["8.8.8.8"];

    # NOTE: Use 'shutdown now' to exit the VM.
    services.getty.autologinUser = "root";

    virtualisation = {
      graphics = false;
      host = {inherit pkgs;};
      forwardPorts = [
        {
          from = "host";
          guest.port = 80;
          host.port = 8080;
        }
      ];
    };

    system.stateVersion = "23.05";
  };
}
