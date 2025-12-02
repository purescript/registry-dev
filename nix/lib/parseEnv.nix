# Parse a .env file into a Nix attrset, skipping comments and empty lines
{ lib }:

path:
let
  lines = lib.splitString "\n" (builtins.readFile path);
  isContent =
    line: builtins.match "^[[:space:]]*$" line == null && builtins.match "^#.*$" line == null;
  toKeyValue =
    line:
    let
      match = builtins.match "([^=]+)=(.*)" line;
    in
    {
      name = builtins.elemAt match 0;
      value = builtins.elemAt match 1;
    };
in
builtins.listToAttrs (map toKeyValue (builtins.filter isContent lines))
