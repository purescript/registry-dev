{ makeWrapper, lib, stdenv, purix, slimlock, esbuild, nodejs, writeText, compilers, dhall, dhall-json, licensee, git, coreutils, gzip }:
let
  package-lock = slimlock.buildPackageLock { src = ../.; omit = ["dev" "peer"];};

  spago-lock = purix.buildSpagoLock {
    src = ../.;
    # Not technically necessary since we aren't using backend-es, but this
    # prevents us from having to build dependencies with and without corefn.
    corefn = true;
  };

  build-script = name: module:
    stdenv.mkDerivation rec {
      inherit name;
      src = ./src;
      nativeBuildInputs = [ esbuild makeWrapper ];
      buildInputs = [ nodejs ];
      entrypoint = writeText "entrypoint.js" ''
        import { main } from "./output/Registry.Scripts.${module}";
        main();
      '';
      buildPhase = ''
        ln -s ${package-lock}/js/node_modules .
        cp -r ${spago-lock.registry-scripts}/output .
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
          --set PATH ${lib.makeBinPath [ compilers dhall dhall-json licensee git coreutils gzip ]}
      '';
      };
in {
  legacy-importer = build-script "registry-legacy-importer" "LegacyImporter";
  package-deleter = build-script "registry-package-deleter" "PackageDeleter";
  package-set-updater = build-script "registry-package-set-updater" "PackageSetUpdater";
  package-transferrer = build-script "registry-package-transferrer" "PackageTransferrer";
  solver = build-script "registry-solver" "Solver";
  verify-integrity = build-script "registry-verify-integrity" "VerifyIntegrity";
}
