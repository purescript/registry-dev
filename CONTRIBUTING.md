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
nix flake check -L
```

There is an integration test that will deploy the registry server and make requests to the API, which you can run if you are on a Linux machine. It is included in the `nix flake check` command by default, but it can be convenient to run standalone as well:

```sh
nix build .#checks.x86_64-linux.integration
```

You can "deploy" the registry server to a local VM and manually hit the API as if it were the production server:

```sh
# The server will be available at localhost:8080
nix run
```

### Testing Guidelines

The PureScript code in the registry is well-tested, ranging from tests for individual functions to full end-to-end tests for the registry server running in a NixOS machine configured the same way as the deployed machine. The smaller and more pure the test, the easier it is to write and maintain; most code is tested via unit tests written with `spec`, and only the core pipelines are run in the integration test.

Each PureScript workspace has a `test` directory containing tests written with `spec`. For example, see the [`lib`](./lib/test/), [`foreign`](./foreign/test/), or [`app`](./app/test/) test directories. If you write a new function in e.g. the `foreign` workspace, then write tests in the `foreign/test` directory.

In general we prefer that tests are self-contained in PureScript. For example, if you need to decode some JSON data, prefer to write that data in a PureScript string and decode the string rather than read a JSON file from disk. If a function must read from disk, prefer to generate test data and write it to a `tmp` directory rather than commit test files to the repository.

However, in the rare case where you do need to store test data in the repository, you can do so in the `fixtures` directory. For example, see the [`lib` fixtures](./lib/fixtures/) or the [`app` fixtures](./app/fixtures/). (These are only for tests, but they're kept outside the `test` directory so that Spago doesn't try to compile PureScript code found in the fixtures when running `spago test`.)

### Mock Tests

The registry source code is defined such that most effectful code is abstracted by the [`App.Effect`](./app/src/App/Effect) modules. This allows us to mock those effects for testing purposes. For example, the `publish` function will download packages from S3 using the `STORAGE` effect, publish documentation to Pursuit with the `PURSUIT` effect, write to the registry repository with the `REGISTRY` effect, fetch data from remote repositories with the `GITHUB` effect, and more.

We obviously don't want to perform these effects in our tests, but we still want to test that the `publish` function behaves as expected. If you are writing a test for a function written in `Run (SOME_EFFECT + r) a`, like `publish`, then you will need to mock effects when writing your tests. You can see the mock implementations in the [`Registry.Test.Assert.Run`](./app/test/Test/Assert/Run.purs) module.

The mock tests use fixtures to represent remote resources. For example, instead of a remote S3 bucket we have the [`registry-storage`](./app/fixtures/registry-storage/) fixtures; this directory is our 'storage backend' and we can 'download' tarballs from it and 'upload' tarballs to it. Instead of accessing arbitrary GitHub repositories we have the [`github-packages`](./app/fixtures/github-packages/) fixtures. Instead of the upstream registry, registry-index, and package-sets repositories, we have e.g. the [`registry-index`](./app/fixtures/registry-index) fixtures.

The function under test will only have access to data in these fixtures.

### Integration Tests

There is an integration test that will deploy the production registry server (no mock effects) and then execute a number of requests against its API. On x86_64-linux machines you can run it:

```sh
nix build checks.x86_64-linux.integration
```

The integration test uses the same server machine that we deploy. It makes requests to the GitHub API and our S3 storage, executes `git` commands against the upstream registry, registry-index, and package-sets repositories, accesses a SQLite database, and so on. In other words, it uses the real-world implementations of the `Registry`, `GitHub`, `Storage`, and other effects. It is the most complicated to set up, so the integration tests **should be kept minimal**. If it is possible to use unit tests or mock effects, use those instead. The integration test ensures that each API endpoint is usable, but scenarios more complicated than standard usage should be done in mock effect tests instead.

Of course, we don't _actually_ want to touch any real-world data and in a Nix test environment we cannot access the network arbitrarily. Instead we hijack the effect implementations from the outside and supply the same fixtures which are available in the mock effect tests. There are two external methods of access we need to replace.

#### Intercepting Git

Git can use a local file path instead of a remote URL, such that `git clone my-path new-repo` clones to `new-repo` and sets as its origin `my-path`. You can even make changes and push to the upstream if the upstream has been configured with `receive.denyCurrentBranch` set to `ignore`, though this wrecks the upstream's working index. For the sake of tests this doesn't matter.

To support the integration test we supply a wrapped version of `git` that replaces URLs of the form `https://...<domain>/...` with `file://...<path>/...`, where `<path>` is a temporary directory set up with fake repositories built from the fixtures at runtime. For example, this path for the registry-index repository might be `file:///tmp/repo-fixtures/purescript/registry-index`. In this way we can replace various possible Git servers the registry may contact with local fixtures instead.

The wrapped git needs to know where the fixture data lives on the integration test virtual machine, and so we thread a `REPO_FIXTURES_DIR` environment variable through the systemd service for the server to the wrapper script. Packages will be cloned from that directory instead of from GitHub.

#### Intercepting HTTPS

Likewise, we can replace HTTP requests with [wiremock](https://wiremock.org). This tool allows us to return fixture results to HTTP requests. Each API we access has its own wiremock service set up with fixture data; the basic service definition is in the [`nix/wiremock.nix`](./nix/wiremock.nix) file, and individual services with their fixture data are found in the [`flake.nix`](./flake.nix) file. Instead of sending requests to e.g. the GitHub API at https://api.github.com we send them to the local Wiremock server. To do that, we configure our integration test VM with the base URLs for each API we hit. For example:

```sh
# Requests to the GitHub API via Octokit
GITHUB_API_URL=http://localhost:9001

# Requests to packages.registry.purescript.org, e.g. downloads
S3_API_URL=https://localhost:9002

# Requests to the underlying S3 bucket, e.g. 'listObjects'
S3_BUCKET_URL=https://localhost:9003

# Requests to pursuit.purescript.org
PURSUIT_API_URL=https://localhost:9004
```

For each service definition we include request/response pairs we intend to be available on our local API, written in Nix. Here's a short example of creating a mock GitHub API with a request/response pair; in the deployed virtual machine, requests to the GitHub API can be made to http://localhost:9001.

```nix
services.wiremock-github-api = {
  enable = true;
  port = 9001;
  mappings = [
    {
      request = {
        method = "GET";
        url = "/repos/purescript/package-sets/tags";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          name = "psc-0.15.10-20230105";
          commit = {
            sha = "090897c992b2b310b1456506308db789672adac1";
            url = "https://api.github.com/repos/purescript/package-sets/commits/090897c992b2b310b1456506308db789672adac1";
          };
        };
      };
    }
  ];
};
```

It is also possible to include specific files that should be returned to requests via the `files` key. Here's another short example of setting up an S3 mock, in which we copy files from the fixtures into the wiremock service's working directory given a particular file name, and then write request/response mappings that respond to requests by reading the file at path given by `bodyFileName`.

```nix
services.wiremock-s3-api = {
  enable = true;
  port = 9002;
  files = [
    {
      name = "prelude-6.0.1.tar.gz";
      path = ./app/fixtures/registry-storage/prelude-6.0.1.tar.gz;
    }
  ];
  mappings = [
    {
      request = {
        method = "GET";
        url = "/prelude/6.0.1.tar.gz";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/octet-stream";
        bodyFileName = "prelude-6.0.1.tar.gz";
      };
    }
  ];
};
```

## Deployment

The registry is continuously deployed. The [deploy.yml](./.github/workflows/deploy.yml) file defines a GitHub Actions workflow to auto-deploy the server when a new commit is pushed to `master` and test workflows have passed.

However, you can manually deploy a new version of the registry server in one step:

```sh
# Will deploy the server to registry.purescript.org
colmena apply
```

If the deployment fails it will automatically be rolled back. If you have provisioned a new machine or need to update a secret, then you will first need to copy a valid `.env` file to `/var/lib/registry-server/.env` before the server will run. You can test that the server has come up appropriately by SSHing into the server and running `journalctl -u server.service`.
