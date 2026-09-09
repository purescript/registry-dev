# Registry documentation

The `registry-docgen` package contains the shared documentation infrastructure
for the PureScript Registry and Pursuit. It owns the compiler-independent
documentation model used by the registry, conversion from historical compiler
and Pursuit JSON, re-export resolution, and rendering package documentation to
HTML.

The package began as
[`purescript-registry-docgen`](https://github.com/natefaubion/purescript-registry-docgen)
by Nathan Faubion. It is maintained here so documentation generation, storage,
search, and the Pursuit application can evolve together with the registry.

## Architecture

The intended data flow is:

```text
compiler docs JSON + package sources + registry metadata
                         |
                         v
            canonical package documentation JSON
                         |
          +--------------+---------------+
          |              |               |
          v              v               v
       Pursuit       search index     HTML renderer
```

The important ownership boundary is that the registry owns the canonical
artifact. Pursuit, search, and static rendering consume it; they do not define
it. This is why `docgen` is a top-level workspace package rather than part of a
Pursuit application or the public `registry-lib` package.

Package metadata always comes from the canonical `purs.json` stored in the
registry tarball. Historical Pursuit JSON is used during migration only for
compiler-produced documentation and resolution data; it is not an alternative
source for package identity, metadata, dependencies, or source files.
Migration accepts its exact resolutions and module ownership only when they are
consistent with the canonical manifest; inconsistent artifacts must be
regenerated through the registry documentation pipeline.

The main modules are:

- `Registry.Docgen.Docs`: the canonical in-memory documentation model.
- `Registry.Docgen.Codec`: codecs for the canonical JSON artifact.
- `Registry.Docgen.Legacy.*`: codecs for historical compiler and Pursuit JSON.
- `Registry.Docgen.Convert`: conversion from historical JSON to the canonical
  model.
- `Registry.Docgen.Generate`: deterministic validation and assembly of a
  canonical package artifact from compiler docs, parsed source headers,
  package-relative paths, and registry metadata.
- `Registry.Docgen.Reexports`: resolution of module re-exports using package
  source headers.
- `Registry.Docgen.Package.Render`: package and module HTML rendering.

The Pursuit stylesheets used by the renderer are preserved in `assets/css`.
Rendered documents receive their asset URLs explicitly, so a Pursuit server or
another consumer can serve the vendored files from its own static asset path.
The `render-docs` script copies them to `/static/css` in its output directory.

Filesystem access, compiler invocation, source archive handling, README
acquisition, object storage, and executable migration tools belong in `app` or
`scripts`. The `docgen` package should remain deterministic for the same
explicit inputs and should not depend on a Git checkout or a forge API.

## Development

From the repository root, in the Nix development environment:

```sh
spago build -p registry-docgen
spago test -p registry-docgen
purs-tidy check docgen
```

Outside an interactive shell which has loaded `direnv`, prefix these commands
with `direnv exec .`.

## Render an artifact

Use the `render-docs` script to turn a canonical documentation JSON artifact
into static package and module pages:

```sh
nix run .#render-docs -- docgen/fixtures/undefined-1.0.2.json scratch/docs-preview
```

The script prints the generated package directory. Serve the output root with
any static file server and open the corresponding `/packages/<name>/<version>/`
path. Links between the package page and its module pages use the same paths as
Pursuit.
