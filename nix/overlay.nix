# Registry packages overlay
#
# This overlay provides all the registry server components, tools, and scripts.
#
# Architecture:
#   - Apps (in ./app): Server and GitHub importer that share compiled dependencies
#   - Scripts (in ./scripts): CLI utilities that depend on the app code
#   - Build optimization: Apps share a pre-compiled output (app) since they
#     use the same dependencies and source. Scripts also use this to avoid recompiling.
{
  spagoSrc,
  npmSrc,
}:
final: prev:
let
  # Shared optimized output for apps. Both registry-server and registry-github-importer
  # are built from ./app with the same dependencies, so we compile once and reuse.
  app = prev.stdenv.mkDerivation {
    name = "registry-app-optimized";
    src = ../app;
    nativeBuildInputs = [ prev.purs-backend-es-unstable ];

    phases = [
      "buildPhase"
      "installPhase"
    ];

    buildPhase = ''
      # Link dependencies
      ln -s ${final.registry-package-lock}/node_modules .
      ln -s ${final.registry-spago-lock}/output .

      # Compile PureScript to JavaScript using purs-backend-es
      purs-backend-es build
    '';

    installPhase = ''
      mkdir $out
      cp -r output-es $out/output

      # purs-backend-es doesn't copy foreign files, so we need to manually include them
      for dir in output/*/; do
        subdir=$(basename "$dir")
        if [ -f "output/$subdir/foreign.js" ]; then
          cp "output/$subdir/foreign.js" "$out/output/$subdir/"
        fi
      done
    '';
  };

  # Map of script name -> { module, description }
  scripts = {
    archive-seeder = {
      module = "Registry.Scripts.ArchiveSeeder";
      description = "Seed the registry archive with tarballs for deleted GitHub repos";
    };
    legacy-importer = {
      module = "Registry.Scripts.LegacyImporter";
      description = "Import packages from legacy registries (bower, psc-package, etc.)";
    };
    package-deleter = {
      module = "Registry.Scripts.PackageDeleter";
      description = "Delete packages from the registry";
    };
    package-set-updater = {
      module = "Registry.Scripts.PackageSetUpdater";
      description = "Update package sets";
    };
    package-transferrer = {
      module = "Registry.Scripts.PackageTransferrer";
      description = "Transfer packages between storage backends";
    };
    solver = {
      module = "Registry.Scripts.Solver";
      description = "Run dependency solver against registry manifests";
    };
    verify-integrity = {
      module = "Registry.Scripts.VerifyIntegrity";
      description = "Verify registry and registry-index consistency";
    };
    compiler-versions = {
      module = "Registry.Scripts.CompilerVersions";
      description = "List supported compiler versions";
    };
  };

  # Helper function for building registry PureScript executables.
  # Compiles a PureScript module to an esbuild-bundled Node.js executable.
  buildRegistryPackage =
    {
      name,
      module,
      description,
      src,
      spagoLock,
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
        ln -s ${registry-package-lock}/node_modules .
        cp -r ${spagoLock}/output .
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
        cp ${name}.js $out/${name}.js
        makeWrapper ${nodejs}/bin/node $out/bin/${name} \
          --add-flags "$out/${name}.js" \
          --set NODE_PATH "${registry-package-lock}/node_modules" \
          --prefix PATH : "${lib.makeBinPath registry-runtime-deps}"
        ${extraInstall}
        runHook postInstall
      '';
    };
in
{
  # Use the latest spago from purescript-overlay
  spago = prev.spago-unstable;

  # Spago lock: compiled PureScript dependencies for the entire workspace
  registry-spago-lock = prev.mkSpagoDerivation {
    name = "registry";
    src = spagoSrc;
    nativeBuildInputs = [
      final.spago
      prev.purescript
    ];
    buildPhase = "spago build";
    installPhase = "mkdir $out; cp -r * $out";
  };

  # NPM lock: JavaScript dependencies (esbuild, node-gyp, etc.)
  registry-package-lock = prev.buildNpmPackage {
    pname = "purescript-registry";
    version = "0.0.1";
    src = npmSrc;
    dontNpmBuild = true;
    npmFlags = [ "--omit=optional" ];

    nativeBuildInputs =
      with prev;
      [
        # needed for better-sqlite
        python3
        nodePackages.node-gyp
      ]
      ++ prev.lib.optionals prev.stdenv.isDarwin [ prev.darwin.cctools ];

    # To update: run `nix build .#server` and copy the hash from the error
    npmDepsHash = "sha256-iWHvXmTcWr4A/VerriuewnH0qNIYBtYkQnqv1VO8Jhs=";

    installPhase = ''
      mkdir -p $out
      rm -f node_modules/{registry-app,registry-lib,registry-foreign}
      mv node_modules $out/
    '';
  };

  # Compiler infrastructure

  # All PureScript compilers we support (filtered from purs-bin overlay)
  registry-supported-compilers = prev.lib.filterAttrs (
    name: _: builtins.match "^purs-[0-9]+_[0-9]+_[0-9]+$" name != null
  ) prev.purs-bin;

  # Executable directory containing all supported compiler versions
  registry-compilers = prev.symlinkJoin {
    name = "purs-compilers";
    paths = prev.lib.mapAttrsToList (
      name: drv: prev.writeShellScriptBin name ''exec ${drv}/bin/purs "$@"''
    ) final.registry-supported-compilers;
  };

  # Script that prints all supported compiler versions (space-separated)
  registry-purs-versions = prev.writeShellScriptBin "purs-versions" ''
    echo ${
      prev.lib.concatMapStringsSep " " (
        x: prev.lib.removePrefix "purs-" (builtins.replaceStrings [ "_" ] [ "." ] x)
      ) (prev.lib.attrNames final.registry-supported-compilers)
    }
  '';

  # Runtime dependencies needed by all registry executables
  # Used in: buildRegistryPackage, spago-test check, and devShell
  registry-runtime-deps = with prev; [
    final.registry-compilers
    final.registry-purs-versions

    dhall
    dhall-json
    licensee
    git
    git-lfs
    coreutils
    gzip
    gnutar
  ];

  # Applications

  registry-server = prev.callPackage (buildRegistryPackage {
    name = "registry-server";
    module = "Registry.App.Server";
    description = "PureScript Registry API server";
    src = ../app;
    spagoLock = app;
    extraInstall = "cp -r ${../db} $out/bin/db";
  }) { };

  registry-github-importer = prev.callPackage (buildRegistryPackage {
    name = "registry-github-importer";
    module = "Registry.App.GitHubIssue";
    description = "Import packages from GitHub issues";
    src = ../app;
    spagoLock = app;
  }) { };

  # Scripts - generated from the scripts attrset with module and description
}
// prev.lib.mapAttrs' (
  name: info:
  prev.lib.nameValuePair "registry-${name}" (
    prev.callPackage (buildRegistryPackage {
      name = "registry-${name}";
      module = info.module;
      description = info.description;
      src = ../scripts/src;
      spagoLock = final.registry-spago-lock;
    }) { }
  )
) scripts
// {
  # Convenience namespace for bulk access to apps and scripts
  registry = {
    apps = {
      server = final.registry-server;
      github-importer = final.registry-github-importer;
    };
    scripts = prev.lib.mapAttrs (name: _: final."registry-${name}") scripts;
  };
}
