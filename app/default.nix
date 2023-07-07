{
  stdenv,
  purix,
  slimlock,
  purs-backend-es,
  writeText,
  compilers,
  python3,
  nodejs,
  nodePackages,
}: let
  package-lock =
    (slimlock.buildPackageLock {
      src = ../.;
      omit = ["dev" "peer"];
    })
    # better-sqlite3 relies on node-gyp and python3 in the build environment, so
    # we add those to the native build inputs.
    .overrideAttrs (final: prev: {
      nativeBuildInputs = prev.nativeBuildInputs or [] ++ [python3 nodePackages.node-gyp];
    });

  spago-lock = purix.buildSpagoLock {
    src = ../.;
    corefn = true;
  };

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
    buildInputs = [compilers nodejs];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.Server/index.js";
      main();
    '';
    installPhase = ''
      mkdir -p $out/bin

      echo "Copying files..."
      cp package.json $out/package.json
      ln -s ${package-lock}/js/node_modules $out/node_modules
      cp -r ${shared}/output $out/output
      cp ${entrypoint} $out/entrypoint.js

      echo "Creating node script..."
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/entrypoint.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
  };

  github-importer = stdenv.mkDerivation rec {
    name = "registry-github-importer";
    src = ./src;
    buildInputs = [compilers nodejs];
    entrypoint = writeText "entrypoint.js" ''
      import { main } from "./output/Registry.App.Main";
      main();
    '';
    installPhase = ''
      mkdir -p $out/bin

      echo "Copying files..."
      cp package.json $out/package.json
      ln -s ${package-lock}/js/node_modules $out/node_modules
      cp -r ${shared}/output $out/output
      cp ${entrypoint} $out/entrypoint.js

      echo "Creating node script..."
      echo '#!/usr/bin/env sh' > $out/bin/${name}
      echo 'exec ${nodejs}/bin/node '"$out/entrypoint.js"' "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
  };
}
