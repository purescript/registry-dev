{ fetchurl, writeTextFile, nodejs, stdenv }:
{ src
, package-lock ? src + "/package-lock.json"
}:
let
  # Read the package-lock.json as a Nix attrset
  packageLock = builtins.fromJSON (builtins.readFile package-lock);

  # Create an array of all (meaningful) dependencies
  deps = builtins.attrValues (removeAttrs packageLock.packages [ "" ])
    ++ builtins.attrValues (removeAttrs (packageLock.dependencies or { }) [ "" ]);

  # only cache resolved dependencies with integrity hashes
  modules = builtins.filter (p: builtins.hasAttr "resolved" p && builtins.hasAttr "integrity" p) deps;

  # Turn each dependency into a fetchurl call
  tarballs = map (p: fetchurl { url = p.resolved; hash = p.integrity; }) modules;

  # tarballs to cache
  lines = (builtins.concatStringsSep "\n" tarballs) + "\n";

  # Write a file with the list of tarballs
  tarballsFile = writeTextFile {
    name = "tarballs";
    text = lines;
  };
  version = packageLock.version or "0.0.0";
in
stdenv.mkDerivation {
  inherit (packageLock) name;
  inherit src version;
  buildInputs = [ nodejs ];
  buildPhase = ''
    export HOME=$PWD/.home
    export npm_config_cache=$PWD/.npm
    mkdir -p $out/js
    cd $out/js
    cp -r $src/. .

    cat ${tarballsFile} | xargs npm cache add

    npm ci
  '';

  installPhase = ''
    ln -s $out/js/node_modules/.bin $out/bin
  '';
}
