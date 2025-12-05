# Shared Nix utilities for the registry.
{ lib }:

{
  # Usage:
  #   parseEnvFile (builtins.readFile ./.env.example)
  #   => { SERVER_PORT = "9000"; DATABASE_URL = "sqlite:db/registry.sqlite3"; }
  parseEnvFile =
    contents:
    let
      lines = lib.splitString "\n" contents;

      # Check if a line is a valid KEY=value assignment
      isAssignment =
        line:
        let
          trimmed = lib.trim line;
        in
        trimmed != "" && !(lib.hasPrefix "#" trimmed) && lib.hasInfix "=" trimmed;

      # Parse a single KEY=value line into { name, value }
      parseLine =
        line:
        let
          trimmed = lib.trim line;
          parts = lib.splitString "=" trimmed;
          key = builtins.head parts;
          # Rejoin in case value contains '='
          rawValue = lib.concatStringsSep "=" (builtins.tail parts);
          # Strip surrounding quotes (single or double)
          value = lib.removeSuffix "'" (
            lib.removePrefix "'" (lib.removeSuffix "\"" (lib.removePrefix "\"" rawValue))
          );
        in
        {
          name = key;
          inherit value;
        };

      assignments = builtins.filter isAssignment lines;
      parsed = map parseLine assignments;
    in
    builtins.listToAttrs parsed;
}
