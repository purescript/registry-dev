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

There is an integration test that will deploy the registry server and make requests to the API, which you can run if you are on a Linux machine; it is included in the `nix flake check` command by default, but it can be convenient to run standalone as well:

```sh
nix build checks.x86_64-linux.integration
```

You can "deploy" the registry server to a local VM and manually hit the API as if it were the production server:

```sh
# The server will be available at localhost:8080
nix run
```

These checks are also run in the repository continuous integration.

## Deployment

You can deploy a new version of the registry server in one step:

```sh
# Will deploy the server to registry.purescript.org
colmena apply
```

If the deployment fails it will automatically be rolled back. If you have provisioned a new machine altogether, then you will first need to copy a valid `.env` file to `/var/lib/registry-server/.env` before the server will run. You can test that the server has come up appropriately by SSH-ing into the server and running `journalctl -u server.service`.
