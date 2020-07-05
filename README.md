# A proposal for a PureScript Registry

## Problem at hand

PureScript needs a way to distribute packages, and [it's not possible to rely anymore on the Bower registry for that](https://discourse.purescript.org/t/the-bower-registry-is-no-longer-accepting-package-submissions/1103).

## Goals for a PureScript package registry

We would like to have a registry for PureScript packages, that has the following properties:
- independent: we were leaning on Bower, but that has ultimately faulted us, so we should have something that we are able to control
- package uploads: we don't want a registry that just links to the location of things - like Bower - but we want to store the actual content. GitHub repos are moved/removed all the time, and we want to prevent dependencies for existing builds from disappearing and/or being altered
- content hashes: so that clients can verify package integrity when downloading them
- with version bounds for packages: so that packages can express compatibilities between themselves
- with a way for trusted editors to edit version bounds without republishing: so that faulty bounds/dependencies can be timely corrected if package authors are not around
- easy to implement: the less code we have to maintain, the better
- easy to secure: since this is a sensitive piece of the infrastructure, the smaller the attack surface is, the better
- easy to host: we'd eventually like to have multiple copies of this registry available, so it should be easy to replicate the hosting setup

## Non-goals of this design

Things we do not want to achieve with this:
- all-purpose use: we are going to care about hosting PureScript packages only.
- user-facing frontend: this design aims to provide a mechanism for publishing, collecting, hosting and distributing PureScript packages.
  The data about packages will be exposed publicly, but we do not concern ourselves with presenting it in a navigable/queriable way - this should
  be responsibilities of other tools/services built on top of this data.

# Proposed design: "Just a GitHub Repo"

The gist of this design is that we'd have a GitHub repo (this one) to act as "the registry".

That is, **the registry is a git repo, consisting in a collection of package manifests**.

This repo will contain a file for *every version* of *every package* that is published.
Each one of these files will contain all the info about a package at a specific version, e.g. **the package name, the version, source code location, dependencies, bounds, etc**
This info should conform to a schema that we define as [the `Manifest`](#the-Manifest-schema).

## Features

This section is largely similar to the "goals" above, but goes a bit more in the details of our implementation. A big inspiration for this
feature set has been [this discussion](https://github.com/ziglang/zig/issues/943).

Features of the current design:
- **immutability**: packages are largely immutable - once a version has been published
  then its source code is forever packaged in the tarball uploaded to our storage
  - ..but **unpublishing** will be possible for some time after publishing
  - ..and **trustees would be able to publish new versions** under specified conditions,
    so that e.g. version bounds can be updated if package authors are not available.
- **ease of publishing a release** is optimized for and entirely automated.
- package manifests are **declarative**: authors need not to concern themselves with
  the "how", but just declare properties about their packages.
- **no webserver**: all the software running the Registry is designed in such a way that
  we don't need to authenticate people ourselves, nor need them to upload anything, nor
  need to expose any webserver in general. This greatly reduces the amount of attack vectors
  from the security standpoint.
- **first class support for Package Sets**: see the [relevant section for more info](#Package-Sets).

## The `Manifest` schema

A `Manifest` stores all the metadata (e.g. package name, version, dependencies, etc)
for **a specific  version** of **a specific package**.

You can find all the manifests in the [packages folder](./packages): there is a folder
for each package, which contains a `Manifest` file for every published version.

The storage format for `Manifest`s is **JSON**.

Packages *are not expected* to version the `Manifest` in their source, as the `Manifest`
published on this repo will be packaged in the tarball anyways.
See [here](#Registry-Trustees) for reasons why.

All `Manifest`s are conformed to the [`Manifest` schema](./v1/Manifest.dhall) to ensure forwards-
and backwards-compatibility with past and future schemas.

Here we embed a copy for ease of consultation:

```dhall
{-

The type of a Package version manifest in the Registry

-}

let Map = (./Prelude.dhall).Map.Type

let Target = ./Target.dhall

let Package =
      -- The name of the package
      { name : Text
      -- The SPDX code for the license under which the code is released
      , license : Text
      -- The git repo the package is published at
      , repository : ./Repo.dhall
      -- Compilation targets for the Package
      , targets : Map Text Target
      }

in Package
```

It's useful to embed the definition for `Target` too, since it's the main component of a `Manifest`:

```dhall
{-

A "compilation target".

Every target can have its own dependencies, source globs, etc.
By convention a package needs to have at least one target called `lib`.

Other common ones are `app`, `test`, `dev`, `bench`, etc.

-}

let Map = (./Prelude.dhall).Map.Type

let Target =
      -- A mapping between package names (as published on the Registry or
      -- included in the Package Set) and SemVer ranges for them.
      { dependencies : Map Text Text
      -- A mapping between package names and SemVer ranges for them.
      -- Said dependencies are not PureScript code, and they are used
      -- in FFI to interact with the underlying backend.
      , nativeDependencies : Map Text Text
      -- Local source globs to include in the compilation for the target
      , sources : List Text
      }

in  Target
```


## Adding a package to the registry

Can be done by *opening a pull request* to the repo containing the `Manifest` for
the new version - this operation can (and should) be entirely automated by the package
manager the author is using.

E.g. if we're publishing version `v5.0.0` of the `prelude` package, then we need
to add a file called `packages/prelude/v5.0.0.json`, containing the following:

```dhall
{
  "targets": {
    "lib": {
      "sources": [
        "src/**/*.purs"
      ],
      "nativeDependencies": {},
      "dependencies": {}
    }
  },
  "repository": {
    "version": "v5.0.0",
    "owner": "purescript",
    "repo": "purescript-prelude"
  },
  "name": "prelude",
  "license": "BSD-3-Clause"
}
```

----

Once the PR is open, the following will happen:
- [CI in this repo](#The-Registry-CI) will run some checks and vet if it's good to merge.
  Note: package managers are expected to run the same checks locally as well, to tighten the feedback time for authors.
- Once CI passes then the PR will be merged to the main branch - this will trigger more CI jobs, that will:
  - note that a new package `Manifest` has landed in the Registry
  - then go to the relevant git tag and fetch the sources from the package address
  - package them in a tarball together with the `Manifest` from this repo
    (see [here](#Registry-Trustees) for reasons why)
  - compute the SHA256 of the tarball
  - them upload the tarball and the hash **to a GitHub release** for hosting.
    Note: every package has a dedicated GitHub release that holds all the versions of that package.
    E.g. for the `prelude` package there will be a release called `prelude`, that holds a tarball
    (and its hash) for each of the versions: `v4.1.1`, `v4.1.0`, etc.
    Note: while we'll start with only this storage solution, the goal is to upload to different storage backends.
    See [here](#Mirroring-the-Registry) for more details.
  - generate docs for the published version and upload them to Pursuit
  - optionally add the package to the [unpublished Package Set](#Package-Sets)

Note: upgrading an existing package consists of the same process,
since it involves creating a new `Manifest` file.

The above process covers many of the goals we defined above:
- we store a copy of the packages' sources at the specified version, and this guarantees *immutability* for published packages (of course trusting that the registry itself would not change these)
- we protect the *integrity* of published packages by storing their hash
- we host the packages at no cost since they are normal GitHub releases
- we avoid implementing authentication/authorization ourselves by piggybacking on GitHub's accounts, avoiding potential security vulnerabilities from this
- we avoid implementing a backend for uploading packages, since every upload is done by [our CI](#The-Registry-CI), that is not exposed to the public internet. This also avoids some security concerns
- there's a way for ["Registry Trustees"](#Registry-Trustees) to override details about some package version - e.g. outdated version bounds - without the need for the original author to republish a package.

## Downloading a package

A package manager should download packages in the following way:
- fetch the tarball and the hash with the right version from the package's GitHub release in this repo (or alternative storage backends)
- compute the SHA256 of the tarball and check that it matches the published one
- unpack the tarball and use the sources as desired in the compilation process


## Unpublishing a package/release

Unpublishing a version for a package can be done by removing the version file
associated with it, and the uploaded tarball from the storage locations.
This is allowed for security reasons (e.g. if some package was taken over maliciously),
but for a different set of reasons (i.e. the `leftpad` problem) it's allowed only
for a set period of time: one week after publishing.

The only allowed actors that can validate/approve such unpublishing are:
- the [Registry Trustees](#Registry-Trustees)
- the Package Author

Exceptions to this rule are legal concerns (e.g. DMCA requests) for which Trustees
might have to remove packages at any time.


## Package Sets

As noted in the beginning, Package Sets are a first class citizen of this design.

We'll publish Package Sets inside this repo - you can find an example [here](./v1/sets/20200418.dhall).


## Registry Trustees

The "Registry Trustees" mentioned all across this document are a group of trusted
janitors that have write access to this repo.

Their main task will be that of eventually publish new versions/revisions of packages
that will need adjusting, under very specific conditions.

The reason why this is necessary (vs. only letting the authors publish new versions)
is that for version-solving to work in package managers the Registry will need maintenance.
This maintenance will ideally be done by package authors, but for a set of reasons authors
sometimes become unresponsive.

The reason why this needs to happen is because otherwise older versions of packages with
bad bounds will still break things, even if newer versions have good bounds.
Registries which don’t support revisions will instead support another kind of "mutation" called "yanking",
which allows a maintainer to tell a solver not to consider a particular version any more when constructing build plans.
You can find [a great comparison between the two here](https://www.reddit.com/r/haskell/comments/gf7uw8/on_pvp_and_restrictive_bounds/fpv3dtg/).

Trustees will have to work under a set of constraints so that their activity will
not cause disruption, unreproducible builds, etc.
They will also strive to involve maintainers in this process as much as possible while being friendly, helpful, and respectful.
In general, trustees aim to empower and educate maintainers about the tools at their disposal to better manage their own packages.
We realize that not everyone shares our priorities and we do not want to take up anyone's time unnecessarily.
For these cases we would like to find an arrangement that suits the maintainers wishes, while also not unduly burdening resources of trustees.
Being a part of this curation process is entirely optional and can be opted-out from.

Trustees will try to contact the maintainer of a package for __4 weeks__ before publishing a new version, except if the author
has opted out from this process, in which case they won't do anything.

Trustees are __not__ able to change the source of a package, but only its metadata in the `Manifest` file.

Trustees are allowed to publish __new revisions__ (i.e. using the `pre-release` segment from SemVer), to:
- relax version bounds
- tighten version bounds
- add/remove dependencies/native-dependencies to make the package build correctly


## Name squatting and reassigning names

If you'd like to reuse a package name that has already been taken, you can open an issue in this repo.

If there's no resolution after __4 weeks__, then Registry Trustees will address it.

For more details see [the policy](https://www.npmjs.com/policies/disputes) that NPM has for this.


## Migration path

Right now we hold the Bower registry as the "source of truth" for "which package does a name refer to".
This is to avoid naming inconsistencies, such as two different repos being regarded as the `prelude` package.

In order to provide a smooth migration path to have this Registry be the source of truth instead,
we will pre-populate this new registry with all the PureScript packages existing on the Bower registry.
A list of them (together with their location) can be found in the [`bower-packages.json`](./bower-packages.json) file - it has been obtained by crawling the Bower registry (through `libraries.io`) for all the packages starting with the `purescript-` prefix.

For all these published packages `Manifest`s have been generated, and are stored in this repo.
This the only thing we need in order to repackage these releases in order to upload them to the registry.

## The Registry CI

All the Registry CI runs on GitHub Actions.
See the [ci](./ci) folder in this repo for more details.


## Mirroring the Registry

We can think of "The Registry" as:
- this git repo
- all the tarballs+hashes

Mirroring it to an alternative location would consist of:
- mirroring the git repo - it's just another git remote
- copying all the release artifacts to another hosting location
- optional, to keep the alternative location in sync: add another "tarball upload destination" to the `Registry Curator`

## Authors of this proposal

This design is authored by [**@f-f**](https://github.com/f-f), with suggestions and
ideas from [**@reactormonk**](https://github.com/reactormonk) and [**@justinwoo**](https://github.com/justinwoo)