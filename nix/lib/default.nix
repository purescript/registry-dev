{ lib }:
{
  parseEnv = import ./parseEnv.nix { inherit lib; };
  buildRegistryPackage = import ./buildRegistryPackage.nix;
}
