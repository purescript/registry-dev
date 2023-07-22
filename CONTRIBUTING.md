# Contributing to the PureScript Registry

Welcome to the PureScript Registry development repository! This file helps you get up to speed contributing to the registry.

## Getting Started

You can get all the tools you need to work on this repository via Nix by entering the Nix shell:

```sh
nix develop
```

Then, you can treat this repository as an ordinary PureScript project:

```sh
# Build the source
spago build

# Run the tests
spago test
```

> NOTE: You don't strictly need Nix to work on the registry; if you are contributing PureScript changes only then you can get away with just `purs` and `spago`, but be aware that some tests will fail on your machine, you may see database errors, and you will not be able to run integration tests.

## Repository Structure

The registry is a significant PureScript application split into several runnable modules.

- `app` is the main application and contains the registry server and the GitHub-based API. App code goes here.
- `foreign` contains library code for FFI bindings to JavaScript libraries. Any FFI you write should go here.
- `lib` contains library code meant for other PureScript packages (such as Spago) to reuse. Core registry types and functions go here, and we are careful not to introduce breaking changes unless absolutely necessary.
- `scripts` contains runnable modules written on top of the app for performing registry tasks like uploading and transferring packages.

There are three more directories containing code for the registry.

- `db` contains schemas and migrations for the sqlite3 database used by the server.
- `nix` contains Nix code for building and deploying the registry server to Digital Ocean.
- `types` contains Dhall specifications for the core registry types

Finally, the `flake.nix` file orchestrates builds for the whole repository.

## Available Nix Commands

The Registry server can be run locally:

```sh
nix run .#server
```

You can also run any of the modules listed in the [scripts](./scripts/) directory by converting the camel-case file name to kebab-case, such as:

```sh
# To run `LegacyImporter.purs`
nix run .#legacy-importer

# To run `PackageTransferrer.purs`
nix run .#package-transferrer
```

### Required Environment Variables

The [.env.example](./.env.example) file lists out a number of environment variables that you can set. Scripts that require environment variables will fail at startup if the required env var is not found, so you can add only the ones you need to your .env file.

## Testing

The usual PureScript testing workflow applies in the registry — from within a Nix shell, you can execute all tests:

```sh
spago test
```

There are also a number of checks run by the Nix flake for non-PureScript code, such as verifying Dhall types. Run them:

```sh
nix flake check
```

You can "deploy" the registry server to a local VM and manually hit the API as if it were the production server:

```sh
# The server will be available at localhost:8080
nix run
```

These checks are also run in the repository continuous integration.

## Integration Test

There is an integration test that will deploy the registry server and make requests to the API, which you can run if you are on a Linux machine. It is included in the `nix flake check` command by default, but it can be convenient to run standalone as well:

```sh
nix build checks.x86_64-linux.integration
```

### Structure

The integration test uses the same server machine that we deploy. It makes requests to the GitHub API and our S3 storage, executes `git` commands against the upstream registry, registry-index, and package-sets repositories, accesses a SQLite database, and so on. In other words, it uses the real-world implementations of the `Registry`, `GitHub`, `Storage`, and other effects.

Of course, we don't _actually_ want to touch any real-world data and in a Nix test environment we cannot access the network arbitrarily. Instead we hijack the effect implementations from the outside in two ways.

#### Git

Git can use a local file path instead of a remote URL, such that `git clone my-path new-repo` clones to `new-repo` and sets as its origin `my-path`. You can even make changes and push to the upstream if the upstream has been configured with `receive.denyCurrentBranch` set to `ignore`, though this wrecks the upstream's working index. For the sake of tests this doesn't matter.

To support the integration test we set the `GIT_SERVER_URL` environment variable to the local file system instead of to github.com. Specifically, we point to a directory that is set up with fake `purescript/registry`, `purescript/registry-index`, and `purescript/package-sets` repositories built from the fixtures at runtime.

```sh
GIT_SERVER_URL=file:///tmp/repo-fixtures/
```

#### HTTP

Likewise, we can replace HTTP requests with [wiremock](https://wiremock.org). This tool allows us to intercept requests and return a fixture result instead. The virtual machine that runs the integration test is set up with `wiremock` running on the machine; its configuration is in the [`nix`](./nix) directory.

Instead of sending requests to the actual APIs we want to hit, we'll instead send them to the Wiremock server. To do that we need to configure the base URLs for each external API we hit (namely, S3 and GitHub).

```sh
GITHUB_API_URL=http://localhost:<wiremock-port>/github
S3_API_URL=https://localhost:<wiremock-port>/s3
```

## Deployment

You can deploy a new version of the registry server in one step:

```sh
# Will deploy the server to registry.purescript.org
colmena apply
```

If the deployment fails it will automatically be rolled back. If you have provisioned a new machine altogether, then you will first need to copy a valid `.env` file to `/var/lib/registry-server/.env` before the server will run. You can test that the server has come up appropriately by SSH-ing into the server and running `journalctl -u server.service`.
