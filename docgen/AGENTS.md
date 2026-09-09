# Docgen guidance

- Treat the canonical package codec as a persisted compatibility boundary.
  Stored-format changes require an explicit schema-version decision and codec
  tests for both accepted and rejected versions.
- Keep `registry-docgen` deterministic from explicit inputs. Filesystem and
  network acquisition, compiler invocation, object storage, and runnable tools
  belong in `app` or `scripts`, not this package.
- Source spans must be relative to the package tarball root. Do not assume a
  package uses a `src` directory or that its source is in a Git checkout.
- Keep Pursuit HTTP behavior and search indexing out of this package. They are
  consumers of the canonical artifact.
- Test canonical codec round trips, representative historical JSON conversion,
  and direct, selective, aliased, transitive, missing, and cyclic re-exports.
  Re-export failures must be reported; documentation modules must never be
  silently omitted.
- Schema version 1 records package and compiler versions, source archive
  identity, declared and exact dependencies, module ownership, resolved
  re-exports, README content and format, source location provenance, and
  package-relative source spans. Dependency ranges are retained only for
  historical display; use registry manifests when solving ranges.
- Canonical documentation is stored separately from immutable package tarballs
  through `Registry.App.Effect.DocsStorage`, at `<package>/<version>.json` in
  `DOCS_BUCKET`. Initial uploads are create-only; replacement must remain an
  explicit operation because documentation is derived data.
