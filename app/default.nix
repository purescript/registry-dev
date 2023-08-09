{
  makeWrapper,
  lib,
  stdenv,
  purix,
  purs-backend-es,
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
}: let
  # Since both the importer and the server share the same build process, we
  # don't need to build them twice separately and can share an optimized output
  # directory.
  shared = stdenv.mkDerivation {
    name = "registry-app-shared";
    src = ./.;
    phases = ["buildPhase" "installPhase"];
    nativeBuildInputs = [purs-backend-es];
    buildPhase = ''
      ln -s ${package-lock}/js/node_modules .
      cp -r ${spago-lock.registry-app}/output .
      echo "Optimizing with purs-backend-es..."
      purs-backend-es build
    '';
    installPhase = ''
      mkdir $out
      mv output-es $out/output
    '';
  };
in {
  server = stdenv.mkDerivation rec {
    name = "registry-server";
    src = ./.;
    database = ../db;
    nativeBuildInputs = [esbuild makeWrapper];
    buildInputs = [nodejs];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.Server";
      main();
    '';
    buildPhase = ''
      ln -s ${package-lock}/js/node_modules .
      cp -r ${shared}/output .
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node --packages=external
    '';
    installPhase = ''
      mkdir -p $out/bin

      echo "Copying files..."
      cp ${name}.js $out/${name}.js
      ln -s ${package-lock}/js/node_modules $out

      echo "Copying database..."
      cp -r ${database} $out/bin/db

      echo "Creating node script..."
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${lib.makeBinPath [compilers purs-versions dhall dhall-json licensee git git-lfs coreutils gzip gnutar]} \
    '';
  };

  github-importer = stdenv.mkDerivation rec {
    name = "registry-github-importer";
    src = ./.;
    nativeBuildInputs = [esbuild makeWrapper];
    buildInputs = [nodejs];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.GitHubIssue";
      main();
    '';
    buildPhase = ''
      ln -s ${package-lock}/js/node_modules .
      cp -r ${shared}/output .
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node --packages=external
    '';
    installPhase = ''
      mkdir -p $out/bin $out

      echo "Copying files..."
      cp ${name}.js $out/${name}.js
      ln -s ${package-lock}/js/node_modules $out

      echo "Creating node script..."
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${lib.makeBinPath [compilers purs-versions dhall dhall-json licensee git git-lfs coreutils gzip gnutar]} \
    '';
  };
}
