# A proposal for a PureScript Registry

## Problem at hand

PureScript needs a way to distribute packages, and [it's not possible to rely anymore on the Bower registry for that](https://discourse.purescript.org/t/the-bower-registry-is-no-longer-accepting-package-submissions/1103).

## Goals for a PureScript package registry

We would like to have a registry for PureScript packages, that has the following properties:
- independent: we were leaning on Bower, but that has ultimately faulted us, so we should have something that we are able to control
- package uploads: we don't want a registry that just links to the location of things - like Bower - but we want to store the actual content, to prevent it from disappearing and/or being altered
- content hashes: so that clients can verify package integrity when downloading them
- with version bounds for packages: so that packages can express compatibilities between themselves
- with a way for trusted editors to edit version bounds without republishing: so that faulty bounds can be corrected immediately without having to wait for package authors
- easy to implement: the less code we have to maintain, the better
- easy to secure: since this is a sensitive piece of the infrastructure, the smaller the attack surface is, the better
- easy to host: we'd eventually like to have multiple copies of this registry available, so it should be easy to replicate the hosting setup

## Non-goals of this design

Things we do not want to achieve with this:
- all-purpose use: we are going to care about hosting PureScript packages only.
- user-facing frontend: this design aims to provide a mechanism for publishing, collecting, hosting and distributing PureScript packages.
  The data about packages will be exposed publicly, but we do not concern ourselves with presenting it in a navigable/queriable way - this should
  be responsibilities of other tools/services built on top of this data.

## Proposed design: "Just a GitHub Repo"

The gist of this design is that we'd have a GitHub repo (this one) to act as "the registry".

This repo will contain a file for *every version* of *every package* that is published.
Each one of these files will contain all the info about a package at a specific version, e.g. **the package name, the version, source code location, dependencies, bounds, etc**
This info should conform to a schema that we define as [`PackageDefinition`](#PackageDefinition-schema).

### Adding a package to the registry

Can be done by *opening a pull request* to the repo adding a `PackageDefinition` file for that package.

Once the PR is merged to `master`, the `Registry Curator` detects the new package, and:
- fetches the package from its address
- packages it in a tarball together with any overrides we might have for that `PackageDefinition`
- computes the SHA256 of the tarball
- uploads the tarball and the hash **to a GitHub release** for hosting. 
  Note: every package has a dedicated github release that holds all the versions of that package
- generates docs for that version and uploads them to Pursuit

Note: upgrading an existing package consists of the same process, since it involves creating a new `PackageDefinition` file.

The above process covers many of the goals we defined above:
- we store a copy of the packages' sources at the specified version, and this guarantees *immutability* for published packages (of course trusting that the registry itself would not change these)
- we protect the *integrity* of published packages by storing their hash
- we host the packages at no cost since they are normal GitHub releases
- we avoid implementing authentication/authorization ourselves by piggybacking on GitHub's accounts, avoiding potential security vulnerabilities from this
- we avoid implementing a backend for uploading packages, since every upload is done by the `Registry Curator`, that is not exposed to the public internet. This also avoids some security concerns
- there's a way for `Registry Trustees` to override details about some package version - e.g. outdated version bounds - without the need for the original author to republish a package

### Downloading a package

A package manager should download packages in the following way:
- fetch the tarball and the hash with the right version from the package's GitHub release in this repo
- compute the SHA256 of the tarball and check that it matches the published one
- unpack the tarball and use the sources as desired in the compilation process

### Migrating from the current situation

Right now we hold the Bower registry as the "source of truth" for "which package does a name refer to". This is to avoid naming inconsistencies, such as two different repos being regarded as the `prelude` package.

In order to provide a smooth migration path, we shall pre-populate this new registry with all the PureScript packages existing on the Bower registry.
A list of them (together with their location) can be found in the [`bower-packages.json`](./bower-packages.json) file - it has been obtained by crawling the Bower registry (through `libraries.io`) for all the packages starting with the `purescript-` prefix.

A possible migration path could involve going through this list of already published packages, getting all their versions, and adding a `PackageDefinition` override for all these packages, so that they can be repackaged and uploaded to this registry.

### The `PackageDefinition` schema

TODO

### `PackageDefinition` overrides

TODO

### The `Registry Curator`

TODO

### Mirroring the Registry

We can think of "The Registry" as:
- this git repo
- all the tarballs+hashes

Mirroring it to an alternative location would consist of:
- mirroring the git repo - it's just another git remote
- copying all the release artifacts to another hosting location
- optional, to keep the alternative location in sync: add another "tarball upload destination" to the `Registry Curator`
