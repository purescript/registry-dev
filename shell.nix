let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  }) { };

  # To update to a newer version of packages fetched from GitHub, run:
  # nix-prefetch-git https://github.com/<owner>/<repo>
  #
  # Then, copy the resulting rev and sha256 here.

  dhallPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-dhall-nix";
    rev = "eae7f64c4d6c70681e5a56c84198236930ba425e";
    sha256 = "1y2x15v8a679vlpxazjpibfwajp6zph60f8wjcm4xflbvazk0dx7";
  }) { inherit pkgs; };

  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "678070816270726e2f428da873fe3f2736201f42";
    sha256 = "13l9c1sgakpmh9f23201s8d1lnv0zz0q1wsr1lc92wdpkxs9nii4";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "registry";
  buildInputs = [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.psa
    pursPkgs.purs-tidy

    dhallPkgs.dhall-simple
    dhallPkgs.dhall-json-simple

    pkgs.git
    pkgs.wget
    pkgs.bash

    pkgs.nodejs-16_x

    pkgs.jq
    pkgs.licensee
  ];
}
