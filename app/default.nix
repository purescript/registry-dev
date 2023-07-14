{
  makeWrapper,
  lib,
  stdenv,
  purix,
  slimlock,
  purs-backend-es,
  esbuild,
  writeText,
  nodejs,
  compilers,
  dhall,
  dhall-json,
  git,
  licensee
}: let
  package-lock = slimlock.buildPackageLock {src = ../.; omit = ["dev" "peer"];};
  spago-lock = purix.buildSpagoLock {
    src = ../.;
    corefn = true;
  };

  # Since both the importer and the server share the same build process, we
  # don't need to build them twice separately and can share an optimized output
  # directory.
  shared = stdenv.mkDerivation {
    name = "registry-app-shared";
    src = ./src;
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
    src = ./src;
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
      esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp ${name}.js $out/${name}.js
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
      cp ${name}.js $out
    '';
    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${lib.makeBinPath [ compilers dhall dhall-json licensee git ]}
    '';
  };

  github-importer = stdenv.mkDerivation rec {
    name = "registry-github-importer";
    src = ./src;
    nativeBuildInputs = [esbuild makeWrapper];
    buildInputs = [nodejs];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.Main";
      main();
    '';
    buildPhase = ''
      ln -s ${package-lock}/js/node_modules .
      cp -r ${shared}/output .
      cp ${entrypoint} entrypoint.js
      esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp ${name}.js $out/${name}.js
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
      cp ${name}.js $out
    '';
    postFixup = ''
      wrapProgram $out/bin/${name} \
        --set PATH ${lib.makeBinPath [ compilers dhall dhall-json licensee git ]}
    '';
  };
}
