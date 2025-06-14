{
  makeWrapper,
  lib,
  stdenv,
  esbuild,
  nodejs,
  writeText,
  compilers,
  purs-versions,
  dhall,
  dhall-json,
  licensee,
  git,
  git-lfs,
  coreutils,
  gzip,
  gnutar,
  # from the registry at the top level
  spago-lock,
  package-lock,
}:
let
  build-script =
    name: module:
    stdenv.mkDerivation rec {
      inherit name;
      src = ./src;
      nativeBuildInputs = [
        esbuild
        makeWrapper
      ];
      buildInputs = [ nodejs ];
      entrypoint = writeText "entrypoint.js" ''
        import { main } from "./output/Registry.Scripts.${module}";
        main();
      '';
      buildPhase = ''
        ln -s ${package-lock}/js/node_modules .
        ln -s ${spago-lock}/output .
        cp ${entrypoint} entrypoint.js
        esbuild entrypoint.js --bundle --outfile=${name}.js --platform=node --packages=external
      '';
      installPhase = ''
        mkdir -p $out/bin

        echo "Copying files..."
        cp ${name}.js $out/${name}.js
        ln -s ${package-lock}/js/node_modules $out

        echo "Creating wrapper script..."
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
          }
      '';
    };
in
{
  legacy-importer = build-script "registry-legacy-importer" "LegacyImporter";
  package-deleter = build-script "registry-package-deleter" "PackageDeleter";
  package-set-updater = build-script "registry-package-set-updater" "PackageSetUpdater";
  package-transferrer = build-script "registry-package-transferrer" "PackageTransferrer";
  solver = build-script "registry-solver" "Solver";
  verify-integrity = build-script "registry-verify-integrity" "VerifyIntegrity";
  compiler-versions = build-script "registry-compiler-versions" "CompilerVersions";
}
