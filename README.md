# RFC: PureScript Registry

## Problem at hand

PureScript needs a way to distribute packages. We used to rely on the Bower registry for that, but [this is not possible anymore](https://discourse.purescript.org/t/the-bower-registry-is-no-longer-accepting-package-submissions/1103).

## Goals for a PureScript package registry

Here's a non-comprehensive list of desiderable properties/goals that we'd generally like to
achieve when talking of a PureScript registry, and that we think this design covers:
- **independent**: we're coming from a situation when relying on a third party registry has
  faulted us, so we'd need something that we can control and possibly host ourselves.
- **immutable**: published packages are immutable - once a version has been published
  then its source code is forever packaged in the tarball uploaded to our storage(s).
  The only exception is that **unpublishing** will be possible for some time after publishing.
  This goal also directly stems from our experience with Bower, where we were not able to
  prevent packages from disappearing and/or being altered, since we were not storing the source
  anywhere, but just pointing to the original location of the source, that we had no control over.  
  This means that with this Registry if your package is building today then you're guaranteed that
  the packages you're depending on will not disappear.
- **with content hashes**, so that we're able to support independent storage backends
  and be sure that the content they serve is the same, and - perhaps most importantly - that
  its integrity is preserved over time.
- **with version bounds for packages**, so that package authors can control the amount
  of support that they want to provide for their packages - i.e. allowing a wider range of
  versions for upstream dependencies means more support.
- **with a way for trusted editors to publish new versions**, so that under specified conditions
  faulty bounds/dependencies can be timely corrected if package authors are not around.
- **ease of publishing a release** is optimized for and entirely automated.
- package manifests are **declarative**: authors need not to concern themselves with
  the "how", but just declare properties about their packages. E.g. there's no such
  thing as NPM's `postinstall` and other similar hooks.
- **no webserver**: all the software running the Registry is designed in such a way that
  we don't need to authenticate people ourselves, nor need them to upload anything, nor
  need to expose any webserver in general. This greatly reduces the amount of attack surface
  from the security standpoint.
- **with first class support for Package Sets**: see the [relevant section for more info](#Package-Sets).

This section has been informed by happenings, discussions, and various other readings
on the internet, such as [this one](https://github.com/ziglang/zig/issues/943) and
[this one](https://forums.swift.org/t/swift-package-registry-service/37219).


## Non-goals of this design

Things we _do not_ aim to achieve with this design:
- **all-purpose use**: we are going to care about hosting PureScript packages only.
- **user-facing frontend**: this design aims to provide procedures for publishing, collecting, hosting and distributing PureScript packages.
  The metadata about packages will be exposed publicly, but we do not concern ourselves with presenting it in a navigable/queriable way -
  other tools/services should be built on top of this data to achieve that.

# Proposed design: "Just a GitHub Repo"

Two main ideas here:
- the Registry is nothing more than __some data__ - in our case tarball of package sources stored somewhere public - and __some metadata__ linked to the data so that we can make sense of it - in our case this GitHub repo is our "metadata storage"
- to minimize the attack surface we will use a __pull model__ where all sources are fetched by our CI rather than having authors upload them. In practice this means that all the registry operations will run on the CI infrastructure of this repo.

This repo will contain:
- CI workflows that run the Registry operations
- the source for all this CI
- all the issues/pull-requests that trigger CI operations that affect the Registry
- metadata about packages, such as the list of maintainers, published versions, hashes of the archives, etc.
- the package-sets

All of the above is about metadata, while the real data (i.e. the package tarballs)
will live on various [storage backends](#Storage-Backends).


## The Package `Manifest`

A `Manifest` stores all the metadata (e.g. package name, version, dependencies, etc)
for **a specific  version** of **a specific package**.

Packages are expected to version in their sources a `purs.json` file, that conforms
to the [`Manifest` schema](./v1/Manifest.dhall) to ensure __forwards-compatibility__
with future schemas.
This means that new clients will be able to read old schemas, but not vice-versa.
And the reason why forward (rather than backwards) compatibility is needed is because
package manifests are baked in the (immutable) package tarballs forever, which means that
any client (especially old ones) should always be able to read that manifest.

This means that the only changes allowed to the schema are:
- adding new fields
- removing optional fields
- relaxing constraints not covered by the type system

For more info about the different kinds of schema compatibility, see [here](https://web.archive.org/web/20200913161023/https://docs.confluent.io/current/schema-registry/avro.html#compatibility-types)

All the pre-registry packages will be grandfathered in, see [here for details](#Implementation-plan).
You can find some examples of the `Manifest` that has been generated for them in
the [examples](./examples) folder.

Here we embed a copy of the `Manifest` schema for ease of consultation:

```dhall
{-

The schema for `purs.json` files.

This object holds all the info that the Registry needs to know about it.

-}

let Map = (./Prelude.dhall).Map.Type

let Manifest =
      -- The name of the package
      { name : Text
      -- A short description of the package
      , description : Optional Text
      -- The SPDX code for the license under which the code is released
      , license : Text
      -- The version of this package
      , version : Text
      -- The location where package sources can be found
      , location : ./Location.dhall
      -- The packages this package depends on
      , dependencies : Map Text Text
      }

in Manifest
```

Note: the [`Location` schema](./v1/Location.dhall) includes support for packages that are
not published from the root of the repository, by supplying the (optional) `subdir` field.
This means that a repository could potentially host several packages (commonly called a "monorepo").

### Registry Versions & Version Ranges

The PureScript registry allows packages to specify a version for themselves and version ranges for their dependencies.

We use a restricted version of the SemVer spec which only allows versions with major, minor, and patch places (no build metadata or prerelease identifiers) and version ranges with the `>=` and `<` operators.

This decision keeps versions and version ranges easy to read, understand, and maintain over time.

#### Package Versions
Package versions always take the form `X.Y.Z`, representing major, minor, and patch places. All three places must be natural numbers. For example, in a manifest file:

```json
{
  "name": "my-package",
  "version": "1.0.1"
}
```

If a package uses all three places (ie. it begins with a non-zero number, such as `1.0.0`), then:

* `MAJOR` means values have been changed or removed, and represents a breaking change to the package.
* `MINOR` means values have been added, but existing values are unchanged.
* `PATCH` means the API is unchanged and there is no risk of breaking code.

If a package only uses two places (ie. it begins with a zero, such as `0.1.0`), then:

* `MAJOR` is unused because it is zero
* `MINOR` means values have been changed or removed and represents a breaking change to the package
* `PATCH` means values may have been added, but existing values are unchanged

If a package uses only one place (ie. it begins with two zeros, such as `0.0.1`), then all changes are potentially breaking changes.

#### Version Ranges
Version ranges are always of the form `>=X.Y.Z <X.Y.Z`, where both versions must be valid and the first version must be less than the second version.

When comparing versions, the major place takes precedence, then the minor place, and then the patch place. For example:

* `1.0.0` is greater than `0.12.0`
* `0.1.0` is greater than `0.0.12`
* `0.0.1` is greater than `0.0.0`

All dependencies must take this form. For example, in a manifest file:

```json
{
  "name": "my-package",
  "license": "MIT",
  "version": "1.0.1",
  "dependencies": {
    "aff": ">=1.0.0 <2.0.0",
    "prelude": ">=2.1.5 <2.1.6",
    "zmq": ">=0.1.0 <12.19.124"
  }
}
```

## The Registry API

The Registry should support various automated (i.e. no/little human intervention required) operations:
- adding new packages
- adding new versions of a package
- unpublishing a package

### Adding a new package

As package authors the only thing that we need to do in order to have the Registry upload our
package is to tell it where to get it.

We can do that by *opening an issue* containing JSON that conforms to the
schema of an [`Addition`](./v1/Operation.dhall).

Note: this operation __should__ be entirely automated by the package manager, and
transparent to the user. I.e. package authors shouldn't need to be aware of the inner
workings of the Registry in order to publish a package, and they should be able to
tell to the package manager "publish this" and be given back either a confirmation
of success or failure, or a place to follow updates about the fate of the publishing process.

Implementation detail: how do we "automatically open a GitHub issue" while at the same
time not requiring a GitHub authentication token from the users? The idea is that if
a package manager wants to avoid doing that then it's possible to generate a URL that
the user can navigate to, so that they can preview the issue content before opening it.
[This](https://github.com/purescript/registry/issues/new?title=Add%20package%3A%20example&body=%7B%22packageName%22%3A%22example%22%2C%22location%22%3A%22etc%22%7D)
is an example of such link.

Once the issue is open, the [CI in this repo](#The-Registry-CI) will:
- detect if this is an `Addition`, and continue running if so
- fetch the git repo the `Repo` refers to, checking out the `ref` specified in the `Addition`,
  and considering the package directory to be `subdir` if specified, or the root of the repo if not
- run the [checks for package admission](#Checks-on-new-packages) on the package source we just checked out
  Note: package managers are generally expected to run the same checks locally as well, to tighten the feedback time for authors.
- if all is well, upload the tarball to the [storages](#Storage-backends).
  Note: if any of the Storage Backends is down we fail here, so that the problem can be addressed.
- generate the [package's `Metadata` file](#Package-metadata):
  - add the SHA256 of the tarball
  - add the author of the release as a maintainer of the package.
    If that is unavailable (e.g. if a release is published by a bot),
    then it's acceptable to skip this and proceed anyways, as the list of maintainers of
    a package should be curated by Trustees in any case, as it's going to be useful only for
    actions that require manual intervention.
- optionally add the package to the [next Package Set](#Package-Sets)
- upload the package documentation to [Pursuit](https://pursuit.purescript.org)

The CI will post updates in the issue as it runs the various operations, and close the issue
once all the above operations have completed correctly.

Once the issue has been closed the package can be considered __published__.

### Publishing a new version of an existing package

It is largely the same process as above, with the main difference being that the body of the created issue will
conform to the schema of an [`Update`](./v1/Operation.dhall).

### Unpublishing a package/release

Unpublishing a version for a package can be done by creating an issue containing JSON
conforming to the schema of an [`Unpublish`](./v1/Operation.dhall).

CI will verify that _all_ the following conditions hold:
- the author of the issue is either one of the maintainers or one of the [Registry Trustees](#Registry-Trustees)
- the version is less than __1 week old__

If these conditions hold, then CI will:
- move that package version from `published` to `unpublished` in the package `Metadata`
- delete that package version from the storages

Unpublishing is allowed for security reasons (e.g. if some package was taken over maliciously),
but it's allowed only for a set period of time because of the `leftpad` problem (i.e. breaking everyone's builds).

Exceptions to this rule are legal concerns (e.g. DMCA takedown requests) for which Trustees might have to remove packages at any time.

## Package metadata

Every package will have its own file in the `packages` folder of this repo.

You can see the schema of this file [here](./v1/Metadata.dhall), and the main reasons for this file to exist are to track:
- the upstream location for the sources of the package
- published versions and the SHA256 for their tarball as computed by [our CI](#Adding-a-new-package).
  Note: these are going to be sorted in ascending order according to [SemVer](https://semver.org)
- unpublished versions together with the reason for unpublishing
- GitHub usernames of package maintainers, so that we'll be able to contact them if any action is needed for any of their packages

## Package Sets

As noted in the beginning, Package Sets are a first class citizen of this design.

This repo will be the single source of truth for the package-sets - you can find an example [here](./v1/sets/20200911.dhall) - from
which we'll generate various metadata files to be used by the package manager. __Further details are yet to be defined__.

### Making your own Package Set

While the upstream package sets will only contain packages from the Registry,
it is common to have the need to create a custom package set that might contain
with packages that are not present in the Registry.

In this case the format in which the extra-Registry packages will depend on what
the client accepts.

One of such clients will be Spago, where we'll define an extra-Registry package as:
```dhall
let Registry = https://raw.githubusercontent.com/purescript/registry/master/v1/Registry.dhall

let SpagoPkg =
      < Repo : { repo : Registry.Location, ref : Text }
      | Local : Registry.Prelude.Location.Type
      >
```

..that is, an extra-Registry package in Spago could either point to a local path, or a remote repository.

Here's an example of a package set that is exactly like the upstream, except for the `effect`
package, that instead points to some repo from GitHub:

```dhall
-- We parametrize the upstream package set and the Address type by the package type that our client accepts:
let upstream = https://raw.githubusercontent.com/purescript/registry/master/v1/sets/20200418.dhall SpagoPkg
let Address = Registry.Address SpagoPkg

let overrides =
    { effect = Address.External (SpagoPkg.Repo
        { ref = "v0.0.1"
        , repo = Registry.Repo.GitHub
            { subdir = None Text
            , githubOwner = "someauthor"
            , githubRepo = "somerepo"
            }
        })
    }

in { compiler = upstream.compiler, packages = upstream.packages // overrides }
```

## Registry Trustees

The "Registry Trustees" mentioned all across this document are a group of trusted
janitors that have write access to this repo.

Their main task will be that of eventually publish - under very specific conditions - new versions/revisions of packages
that will need adjustments.

The reason why this is necessary (vs. only letting the authors publish new versions)
is that for version-solving to work in package managers the Registry will need maintenance.
This maintenance will ideally be done by package authors, but for a set of reasons authors
sometimes become unresponsive.

And the reason why such maintenance needs to happen is because otherwise older versions of packages with
bad bounds will still break things, even if newer versions have good bounds.
Registries which don’t support revisions will instead support another kind of "mutation" called "yanking",
which allows a maintainer to tell a solver not to consider a particular version any more when constructing build plans.
You can find [a great comparison between the two here](https://www.reddit.com/r/haskell/comments/gf7uw8/on_pvp_and_restrictive_bounds/fpv3dtg/)
illustrating the reason why we support revisions here.

Trustees will have to work under a set of constraints so that their activity will not cause disruption, unreproducible builds, etc.
They will also strive to involve maintainers in this process as much as possible while being friendly, helpful, and respectful.
In general, trustees aim to empower and educate maintainers about the tools at their disposal to better manage their own packages.
Being a part of this curation process is entirely optional and can be opted-out from.

Trustees will try to contact the maintainer of a package for __4 weeks__ before publishing a new revision, except if the author
has opted out from this process, in which case they won't do anything.

Trustees will __not__ change the source of a package, but only its metadata in the `Manifest` file.

Trustees are allowed to publish __new revisions__ (i.e. versions that bump the `pre-release` segment from SemVer), to:
- relax version bounds
- tighten version bounds
- add/remove dependencies to make the package build correctly

__Note: there is no API defined yet for this operation.__

## Name squatting and reassigning names

If you'd like to reuse a package name that has already been taken, you can open an issue in this repo, tagging the current owner (whose username you can find in the package's metadata file).

If no agreement with the current owner has not been found after __4 weeks__, then Registry Trustees will address it.

For more details see [the policy](https://www.npmjs.com/policies/disputes) that NPM has for this, that we will follow when
not otherwise specified.


## The Package Index

I.e. the answer to the question:

> How do I know which dependencies package X at version Y has?

Without an index of all the package manifests you'd have to fetch the right tarball and look at its `purs.json`.

That might be a lot of work to do at scale, and there are usecases - e.g. for package-sets - where
we need to lookup lots of manifests to build the dependency graph.
So we'll store _all the package manifests_ in a separate location **yet to be defined** (it's really an
implementation detail and will most likely be just another repository, inspired
by [the same infrastructure for Rust](https://github.com/rust-lang/crates.io-index)).


## Storage Backends

As noted above, this repository will hold all the metadata for packages, but the
actual data - i.e. package tarballs - will be stored somewhere else, and we call
each of these locations a "storage backend".

Clients will need to be pointed at place they can store package tarballs from,
so here we'll store a mapping between "name of the storage backend" to a function
that given (1) a package name and (2) a package version then returns the _URL_
where the tarball for that package version can be fetched.

We maintain the list of all the Storage Backends and the aforementioned mappings
[here](./v1/backends.dhall).

We also provide [a small utility](./v1/getBackendUrls.dhall) to demonstrate how
to use the mappings.

There can be more than one storage backend at any given time, and it's always possible
to add more - in fact this can easily be done by:
- looking at all the [package metadata file](#Package-metadata) for every package, to get all the published versions
- then downloading the tarballs from an existing backend, and uploading them to the new location
- update the [mappings file](./v1/backends.dhall) with the new Backend.

### Downloading a package

A package manager should download a specific version of a package in the following way:
1. given "package name" and "version", the URL to fetch the tarball can be computed as described above
2. fetch the tarball from one of the backends
3. lookup the SHA256 for that tarball in the [package metadata file](#Package-metadata)
4. verify that the SHA256 of the tarball downloaded in (2) matches the one from (3)
5. unpack the tarball and use the sources as desired

Note: we are ensuring that the package we download is the same file for all backends
because we are storing the SHA256 for every tarball in a separate location from
the storage backends (this repo).


## Implementation plan

It is paramount that _we provide the smoothest migration path_ that we can achieve
with the resources we have. This is because we feel the ecosystem is already close to
maturity (at this point breaking changes happen very rarely in practice), and we
don't want to unnecessarily mess up with everyone's workflow, especially if it's possible
to avoid that with some planning.

So a big chunk of our work is going towards ensuring that Bower packages are
gracefully grandfathered into the new system. This basically means that for each of them we will:
- generate a [package manifest](#The-Package-Manifest)
- upload them to the first [storage backend](#Storage-backends)
- keep doing that for a while so that package authors have some time to adjust to
  the new publishing flow

What has happened already:
- we're not relying on the Bower registry anymore for guaranteeing package uniqueness in the ecosystem.
  New packages are referenced [in this file](./new-packages.json), while all the packages from the Bower
  registry are referenced [here](./bower-packages.json)
- we have drafted how the registry should behave, what's the API, how things will look like, etc (this document)
- we set up the first [storage backend](#Storage-backends), maintained by the Packaging Team

What is happening right now:
- we're figuring out the last details of [the package `Manifest`](#The-Package-Manifest), which is the big blocker
  for proceeding further, since it will be baked into all the tarballs uploaded to the storage.
- writing up the [CI code](#The-Registry-CI) to import the Bower packages as described above

What will happen after this:
- we'll start using this repo as the source of truth for publishing new package sets
- we'll write the CI code to implement the [Registry API](#The-Registry-API), so that
  authors will be able to publish new packages (albeit manually at first)
- then implement automation to interact with the API in one package manager
  (most likely Spago)
- then only after that we'll adjust package managers to use the tarballs from the Registry in a way that is compliant with this spec.


### The Registry CI

All the Registry CI is implemented in PureScript and runs on GitHub Actions.
Source can be found in the [`ci` folder](./ci), while the [workflows folder](./.github/workflows)
contains the various CI flows.

#### Checks on new packages

**Yet to be defined**: [see this issue](https://github.com/purescript/registry/issues/23)


### Mirroring the Registry

As noted above, "The Registry" is really just:
- this git repo containing metadata
- plus various places that store the package tarballs

Mirroring all of this to an alternative location would consist of:
- mirroring the git repo - it's just another git remote and there are plenty of providers
- copying all the release artifacts to another hosting location. This can be done by looking at the package metadata and literally downloading all the packages listed there, then reuploading them to the new location
- add another "tarball upload destination" to the [registry CI](#The-Registry-CI), to keep all the backends in sync
- add another [Storage Backend](#Storage-backends) in this repo

Additionally we could keep some kind of "RSS feed" in this repo with all the notifications from package uploads,
so other tools will be able to listen to these events and act on that information.


## FAQ

### Why not use X instead?

We have of course investigated other registries before rolling one.

Our main requirement is to have "dependency flattening": there should be only one version
of every package installed for every build.

All the general-purpose registries (i.e. not very tied to a specific language) that we looked at
do not seem to support this.

E.g. it would be possible for us to upload packages to NPM, but installing the packages from there
would not work, because NPM might pull multiple versions of every package from there according
to the needs of every package.

### Why not a webserver like everyone else?

These are the main reasons why we prefer to handle this with git+CI, rather than deploying a separate service:
- _visibility_: webserver logs are hidden, while CI happens in the open and everyone can audit what happens
- _maintenance_: a webserver needs to be deployed and kept up, CI is always there

### How do I conform JSON to a Dhall type?

[Install dhall](https://github.com/dhall-lang/dhall-haskell), then:

```bash
$ cat "your-file.json" | json-to-dhall --records-loose --unions-strict "./YourDhallType.dhall"
```

## Authors of this proposal

This design is authored by [**@f-f**](https://github.com/f-f), with suggestions and
ideas from:
- [**@reactormonk**](https://github.com/reactormonk)
- [**@justinwoo**](https://github.com/justinwoo)
- [**@thomashoneyman**](https://github.com/thomashoneyman)
- [**@hdgarrood**](https://github.com/hdgarrood)
