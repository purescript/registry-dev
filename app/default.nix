{
  stdenv,
  purix,
  purs-backend-es,
  esbuild,
  writeText,
  compilers,
  nodejs,
}: let
  package-lock = purix.buildPackageLock {src = ../.;};
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
    nativeBuildInputs = [esbuild];
    buildInputs = [compilers nodejs];
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
  };

  github-importer = stdenv.mkDerivation rec {
    name = "registry-github-importer";
    src = ./src;
    nativeBuildInputs = [esbuild];
    buildInputs = [compilers nodejs];
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
    checkPhase = ''

    '';
    installPhase = ''
      mkdir -p $out/bin
      cp ${name}.js $out/${name}.js
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec node '"$out/${name}.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
      cp ${name}.js $out
    '';
  };
}
