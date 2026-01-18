# AGENTS.md

The PureScript Registry implements a package registry for PureScript. See @SPEC.md for the registry specification and @CONTRIBUTING.md for detailed contributor documentation.

## Development Environment

This project uses Nix with direnv. You should already be in the Nix shell automatically when entering the directory. If not, run:

```sh
nix develop
```

Watch out for these Nix quirks:
- If Nix tries to fetch from git during a build, it is likely that spago.yaml files were changed but the lock file was not updated; if so, update the lockfile with `spago build`
- If a Nix build appears to be stale, then it is likely files were modified but are untracked by Git; if so, add modified files with `git add` and retry.

### Build

The registry is implemented in PureScript. Use spago to build it.

```sh
spago build  # Build all PureScript code
```

The registry infrastructure is defined in Nix. Build it with Nix:

```sh
nix build .#server
```

### Test

The registry contains a mixture of unit tests, e2e tests, and nix flake checks. When you complete a change you should generally run the unit tests. When working on the server, you should generally also run the e2e tests. If you are on a Linux system, you can run `nix flake check -L` to run the flake checks prior to committing code to ensure it works.

#### Unit Tests

Unit tests can be run with `spago`. They are fast and cheap.

```sh
spago test   # Run all unit tests
spago test -p <package-name>  # Run tests for a specific package
```

#### End-to-End Tests

The end-to-end (integration) tests are in `app-e2e`. They can be run via Nix on Linux:

```sh
nix build .#checks.x86_64-linux.integration
```

Alternately, they can be run on macOS or for more iterative development of tests using two terminals: one to start the test env, and one to execute the tests.

```sh
# Terminal 1: Start test environment (wiremock mocks + registry server on port 9000)
nix run .#test-env

# Terminal 2: Run E2E tests once server is ready
spago-test-e2e
```

Options: `nix run .#test-env -- --tui` for interactive TUI, `-- --detached` for background mode to use a single terminal.

State is stored in `/tmp/registry-test-env` and cleaned up on each `nix run .#test-env`. To examine state after a test run (for debugging), stop the test-env but don't restart it. This is useful, for example, to read the logs of the most recent run. For example:

```sh
# after a test run, see the logs (log name is today's date)
cat /tmp/registry-test-env/scratch/logs/*.log
```

#### Smoke Test (Linux only)

The smoke test verifies that the server comes up properly and tests deployment. Only run this test if you are making changes which could break the deployment of the server.

```sh
nix build .#checks.x86_64-linux.smoke -L
```

#### Continuous Integration via Nix Checks 

There is a full suite of checks implemented with Nix which verify that packages build, formatting is correct, registry types are Dhall-conformant, and more. This is the primary check run in CI.

```sh
nix flake check -L
```

## Formatting

```sh
# Format PureScript
purs-tidy format-in-place app app-e2e foreign lib scripts 
purs-tidy check app app-e2e foreign lib scripts

# Format Nix files
nixfmt *.nix nix/**/*.nix
```

## Project Structure

- `app/` — Registry server implementation.
- `app-e2e/` — E2E tests for the server API.
- `lib/` — **Public library** for consumers (Spago, Pursuit, etc.). Only types and functions useful to external tools belong here. Avoid implementation-specific code.
- `foreign/` — FFI bindings to JavaScript libraries.
- `scripts/` — Runnable modules for registry tasks (LegacyImporter, PackageTransferrer, PackageSetUpdater, etc.). Run via `nix run .#legacy-importer`, etc.
- `test-utils/` — Shared test utilities.
- `db/` — SQLite schemas and migrations (use `dbmate up` to initialize).
- `types/` — Dhall type specifications.
- `nix/` — Nix build and deployment configuration.

## Scripts & Daily Workflows

The `scripts/` directory contains modules run as daily jobs by the purescript/registry repository:

- `LegacyImporter` — imports package versions from legacy Bower registry
- `PackageTransferrer` — handles package transfers
- `PackageSetUpdater` — automatic daily package set updates

Run scripts via Nix: `nix run .#<kebab-case-name>` (e.g., `nix run .#legacy-importer`). All scripts support `--help` for usage information.

## Scratch Directory & Caching

The `scratch/` directory (gitignored) is used by scripts for:
- `.cache/` — Cached API responses, downloaded packages, etc.
- `logs/` — Log files
- `registry/`, `registry-index/` — Local clones for testing, also modified and optionally committed to by scripts

Caching is critical for the legacy importer due to the expense of downloading packages. The `Registry.App.Effect.Cache` module handles caching.

## PureScript Conventions

### Custom Prelude

Always use `Registry.App.Prelude` in `app/` and `app-e2e/` directories:

```purescript
import Registry.App.Prelude
```

### Effects via Run

Use the `run` library for extensible effects. Do NOT perform HTTP calls, console logs, or other effects directly in `Aff`. Check for existing effects in `app/src/App/Effect/` or consider adding one.

### Import Style

Import types unqualified, values qualified. Use shortened module names:

```purescript
import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Node.FS.Aff as FS.Aff
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Registry.Operation (AuthenticatedData)
import Registry.SSH as SSH
```

### Syntax

- Never use `let/in` syntax unless in an `ado` block. Prefer `do/let`.

## Deployment

Continuous deployment via GitHub Actions on master. Manual deploy:

```sh
colmena apply
```
