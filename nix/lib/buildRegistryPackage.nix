# Helper function for building registry PureScript executables. Compiles a
# PureScript module to an esbuild-bundled Node.js executable.
#
# Returns a function suitable for callPackage that will be auto-injected with:
#   - registry-runtime-deps, registry-package-lock (from overlay)
#   - Standard build tools (esbuild, nodejs, etc.)
{
  name,
  module,
  src,
  spagoLock,
  description,
  extraInstall ? "",
}:
{
  lib,
  stdenv,
  makeWrapper,
  esbuild,
  writeText,
  nodejs,
  registry-runtime-deps,
  registry-package-lock,
}:
let
  # ESM entrypoint that imports and runs the PureScript main function
  entrypoint = writeText "entrypoint.js" ''
    import { main } from "./output/${module}";
    main();
  '';
in
stdenv.mkDerivation {
  inherit name src;

  nativeBuildInputs = [
    esbuild
    makeWrapper
  ];

  buildInputs = [ nodejs ];

  meta = {
    inherit description;
    mainProgram = name;
  };

  buildPhase = ''
    runHook preBuild

    # Link dependencies and compiled output
    ln -s ${registry-package-lock}/node_modules .
    cp -r ${spagoLock}/output .

    # Bundle with esbuild
    cp ${entrypoint} entrypoint.js
    esbuild entrypoint.js \
      --bundle \
      --outfile=${name}.js \
      --platform=node \
      --packages=external

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin

    # Install the bundled JavaScript
    cp ${name}.js $out/${name}.js

    # Create wrapper script with runtime dependencies in PATH
    makeWrapper ${nodejs}/bin/node $out/bin/${name} \
      --add-flags "$out/${name}.js" \
      --set NODE_PATH "${registry-package-lock}/node_modules" \
      --prefix PATH : "${lib.makeBinPath registry-runtime-deps}"

    ${extraInstall}

    runHook postInstall
  '';
}
