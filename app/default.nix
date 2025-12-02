{
  makeWrapper,
  lib,
  stdenv,
  purs-backend-es-unstable,
  esbuild,
  writeText,
  nodejs,
  compilers,
  purs-versions,
  dhall,
  dhall-json,
  git,
  git-lfs,
  licensee,
  coreutils,
  gzip,
  gnutar,
  # from the registry at the top level
  spago-lock,
  package-lock,
}:
let
  # Since both the importer and the server share the same build process, we
  # don't need to build them twice separately and can share an optimized output
  # directory.
  shared = stdenv.mkDerivation {
    name = "registry-app-shared";
    src = ./.;
    phases = [
      "buildPhase"
      "installPhase"
    ];
    nativeBuildInputs = [ purs-backend-es-unstable ];
    buildPhase = ''
      ln -s ${package-lock}/node_modules .
      ln -s ${spago-lock}/output .
      echo "Optimizing with purs-backend-es..."
      purs-backend-es build
    '';
    installPhase = ''
      mkdir $out;
      cp -r output-es $out/output;
      # This for loop exists because purs-backend-es finds the corefn.json files
      # just fine, but doesn't find the foreign.js files.
      # I suspect this is because of a quirky interaction between Nix and `copyFile`,
      # but I'm not sure how to fix it so we work around it by copying the foreign
      # modules by hand.
      for dir in output/*/; do
        subdir=$(basename "$dir")
        if [ -f "output/$subdir/foreign.js" ]; then
          cp "output/$subdir/foreign.js" "$out/output/$subdir/" || true;
        fi
      done
    '';
  };
in
{
  server = stdenv.mkDerivation rec {
    name = "registry-server";
    src = ./.;
    database = ../db;
    nativeBuildInputs = [
      esbuild
      makeWrapper
    ];
    buildInputs = [ nodejs ];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.Server";
      main();
    '';
    buildPhase = ''
      ln -s ${package-lock}/node_modules .
      cp -r ${shared}/output .
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node --packages=external
    '';
    installPhase = ''
      mkdir -p $out/bin

      echo "Copying files..."
      cp ${name}.js $out/${name}.js
      ln -s ${package-lock}/node_modules $out/node_modules

      echo "Copying database..."
      cp -r ${database} $out/bin/db

      echo "Creating node script..."
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${
          lib.makeBinPath [
            compilers
            purs-versions
            dhall
            dhall-json
            licensee
            git
            git-lfs
            coreutils
            gzip
            gnutar
          ]
        } \
    '';
  };

  github-importer = stdenv.mkDerivation rec {
    name = "registry-github-importer";
    src = ./.;
    nativeBuildInputs = [
      esbuild
      makeWrapper
    ];
    buildInputs = [ nodejs ];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.GitHubIssue";
      main();
    '';
    buildPhase = ''
      ln -s ${package-lock}/node_modules .
      cp -r ${shared}/output .
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node --packages=external
    '';
    installPhase = ''
      mkdir -p $out/bin $out

      echo "Copying files..."
      cp ${name}.js $out/${name}.js
      ln -s ${package-lock}/node_modules $out/node_modules

      echo "Creating node script..."
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${
          lib.makeBinPath [
            compilers
            purs-versions
            dhall
            dhall-json
            licensee
            git
            git-lfs
            coreutils
            gzip
            gnutar
          ]
        } \
    '';
  };
}
