{ pkgs ? import <nixpkgs> {} }:
pkgs.runCommand "dummy" {
  buildInputs = [ pkgs.jq pkgs.bash ];
} ""

