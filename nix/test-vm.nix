# Machine configuration for the NixOS virtual machine suitable for testing.
{
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [
    "${modulesPath}/virtualisation/qemu-vm.nix"
    ./module.nix
  ];

  config = {
    # https://github.com/utmapp/UTM/issues/2353
    networking.nameservers = lib.mkIf pkgs.stdenv.isDarwin [ "8.8.8.8" ];

    # NOTE: Use 'shutdown now' to exit the VM.
    services.getty.autologinUser = "root";

    virtualisation = {
      forwardPorts = [
        {
          from = "host";
          guest.port = 80;
          host.port = 8080;
        }
      ];
      graphics = false;
      host = {
        inherit pkgs;
      };
      # Can be adjusted if necessary for test systems (default is 1024)
      memorySize = 2048;
    };

    system.stateVersion = "24.05";
  };
}
