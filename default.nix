{ pkgs ? import <nixpkgs> {} }:

let
  easy-dhall = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-dhall-nix";
      rev = "eae7f64c4d6c70681e5a56c84198236930ba425e";
      sha256 = "1y2x15v8a679vlpxazjpibfwajp6zph60f8wjcm4xflbvazk0dx7";
    }
  ) {
    inherit pkgs;
  };

in
pkgs.runCommand "dummy" {
  buildInputs =
    builtins.attrValues {
      inherit (easy-dhall) dhall-simple dhall-json-simple;
    } ++
    [ pkgs.git pkgs.wget pkgs.jq pkgs.bash ];
} ""
