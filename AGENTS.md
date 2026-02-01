# AGENTS.md

The PureScript Registry implements a package registry for PureScript.

- @SPEC.md contains the registry specification. Use this to understand the core data types and operations of the registry.
- @CONTRIBUTING.md describes the structure of the registry codebase, what the various packages in the monorepo represent, and how to build and test the registry code.

## Development Environment

We use Nix with direnv. Expect to be in a Nix shell automatically, but if you are not, you can enter one:

```sh
nix develop
```

### Nix Quirks

- If Nix tries to fetch from git during a build and fails, then most likely `spago.yaml` files have been changed but the lockfiles were not updated. Update them with `spago build`.
- If a Nix build appears to be stale, then most likely files were modified but not tracked by Git. Nix flakes only consider Git-tracked files. Add modified files with `git add` and retry.

### Build & Test Commands

The registry is implemented in PureScript. Use spago to build it.

```sh
spago build  # Build all PureScript code
```

The registry contains unit tests, end-to-end tests, and nix flake checks.

- Run unit tests when you complete a change with `spago test` or `spago test -p <package-name>`.
- Run end-to-end tests when working on the registry server in `app`. 
- On Linux systems you can run all flake checks (the tests run in CI) using `nix flake check -L`.

### End-to-End Tests

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
- `scripts/` — Runnable modules for registry tasks (PackageTransferrer, PackageSetUpdater, DailyImporter, etc.). Run via `nix run .#package-transferrer`, etc.
- `test-utils/` — Shared test utilities.
- `db/` — SQLite schemas and migrations (use `dbmate up` to initialize).
- `types/` — Dhall type specifications.
- `nix/` — Nix build and deployment configuration.

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

Never use `let/in` syntax unless in an `ado` block. Always `do/let`.

```purs
func =
  -- NEVER use let/in syntax
  let x = 1
   in x + x

func = do
  -- ALWAYS use do/let syntax
  let x = 1
  x + x
```
