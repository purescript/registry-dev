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
    rev = "86f76316f056628b3693df99ffb1120921e84902";
    sha256 = "0qpy4j8a0782vgck6lflkw6icxfiymlck6yd4mf62ick23pf4swk";
  }) { inherit pkgs; };

  # Convert a PureScript package from easy-purescript-nix (ie. pursPkgs.purs-0_14_0)
  # into a shell script of the same name that can be executed at the command line,
  # such as:
  #
  #   $ purs-0_14_0 --version
  #   0.14.0
  mkPurs = pursPkg:
    let
      stringVersion = builtins.replaceStrings ["."] ["_"] (pkgs.lib.removePrefix "v" (pkgs.lib.getVersion pursPkg));
    in
      pkgs.writeShellScriptBin "purs-${stringVersion}" ''
        exec ${pursPkg}/bin/purs "$@"
      '';


# TODO: Upstream 0.14.9 to easy-purescritp-nix
in pkgs.stdenv.mkDerivation {
  name = "registry";
  buildInputs = [
    # Development build inputs
    pursPkgs.purs-0_14_7
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

    # Multiple compilers available for building packages

    # 0.13.x series
    (mkPurs pursPkgs.purs-0_13_0)
    (mkPurs pursPkgs.purs-0_13_2)
    (mkPurs pursPkgs.purs-0_13_3)
    (mkPurs pursPkgs.purs-0_13_4)
    (mkPurs pursPkgs.purs-0_13_5)
    (mkPurs pursPkgs.purs-0_13_6)
    (mkPurs pursPkgs.purs-0_13_8)

    # 0.14.x series
    (mkPurs pursPkgs.purs-0_14_0)
    (mkPurs pursPkgs.purs-0_14_1)
    (mkPurs pursPkgs.purs-0_14_2)
    (mkPurs pursPkgs.purs-0_14_3)
    (mkPurs pursPkgs.purs-0_14_4)
    (mkPurs pursPkgs.purs-0_14_5)
    (mkPurs pursPkgs.purs-0_14_6)
    (mkPurs pursPkgs.purs-0_14_7)
  ];
}
