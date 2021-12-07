let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
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
    rev = "d9a37c75ed361372e1545f6efbc08d819b3c28c8";
    sha256 = "1fklhnddy5pzzbxfyrlprsq1p8b6y9v0awv1a1z0vkwqsd8y68yp";
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

    pkgs.nodejs-14_x

    pkgs.jq
    pkgs.licensee
  ];
}
