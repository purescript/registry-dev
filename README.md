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

# Proposed design: "Just a GitHub Repo"

The gist of this design is that we'd have a GitHub repo (this one) to act as "the registry".

That is, **the registry is a git repo, consisting in a collection of package manifests**.

This repo will contain a file for *every version* of *every package* that is published.
Each one of these files will contain all the info about a package at a specific version, e.g. **the package name, the version, source code location, dependencies, bounds, etc**
This info should conform to a schema that we define as [the `Package` type](#the-Package-schema).

## Features

This section is largely similar to the "goals" above, but goes a bit more in the details of our implementation. A big inspiration for this
feature set has been [this discussion](https://github.com/ziglang/zig/issues/943).

Features of the current design:
- **immutability**: packages are largely immutable - once a version has been published
  then its source code is forever packaged in the tarball uploaded to our storage
  - ..but **unpublishing** will be possible for some time after publishing
  - ..as well as **overriding the package manifest by trustees**, so that e.g. version bounds
  can be updated without having to ask authors to cut a new release.
- **ease of publishing a release** is optimized for and entirely automated.
- package manifests are **declarative**: authors need not to concern themselves with
  the "how", but just declare properties about their packages.
- **no lockfiles**: we don't need to use any as we can lean on Dhall's
  [secure hashing features](https://docs.dhall-lang.org/discussions/Safety-guarantees.html)
  and Package Sets in order to get replicable builds.
- packages **need not to depend only on published packages** in general (i.e. users
  can always have local/external overrides), but published packages will need to.
- package manifests **integrate build, package management and publishing info**, a là Cabal.
- **no webserver**: all the software running the Registry is designed in such a way that
  we don't need to authenticate people ourselves, nor need them to upload anything, nor
  need to expose any webserver in general. This greatly reduces the amount of attack vectors
  from the security standpoint.
- **first class support for Package Sets**: see the [relevant section for more info](#Package-Sets).

## The `Package` schema

See [the `Package` type definition](./v1/Package.dhall) for an up to date representation.

Here we embed a copy for ease of consultation:

```dhall
{-

The type of a Package version in the Registry.

Note: not all fields are compulsory when compiling the definition, and we do 
provide meaningful defaults for many of them in the ./Registry.dhall file.

-}
let Map = (./Prelude.dhall).Map.Type

let Target = (./Target.dhall).Type

let Package =
      -- The name of the package
      { name : Text
      -- The SPDX code for the license under which the code is released
      , license : ./License.dhall
      -- The git repo the package is published at
      , repository : Optional ./Repo.dhall
      -- The source for the package versions listed in the `targets`.
      -- Can be either the Registry or a PackageSet
      , packages : ./Index.dhall
      -- Compilation targets for the Package
      , targets : Map Text Target
      }

in Package
```

It's useful to embed the definition for `Target` too, since it's the main component of a `Package`:

```dhall
{-

A "compilation target".

Every target can have its own dependencies, source globs, etc.
By convention a package needs to have at least one target called `lib`.

Other common ones are `app`, `test`, `dev`, `bench`, etc. 

-}

let Map = (./Prelude.dhall).Map.Type

let TargetType =
      -- A mapping between package names (as published on the Registry or
      -- included in the Package Set) and SemVer ranges for them.
      { dependencies : Map Text Text
      -- Source globs for the local project to include in the compilation
      -- alongside its dependencies.
      , sources : List Text
      -- Optional output folder where the compiler will put its result.
      -- If not specified, the compiler will use "output"
      , output : Optional Text
      -- A target might not be pointing at the JS backend - if that's the case
      -- we can specify here the command for the alternate backend.
      -- Example values: `psgo`, `pskt`, etc.
      , backend : Optional Text
      }

let default =
      { dependencies = (toMap {=}) : Map Text Text
      , sources = [] : List Text
      , output = None Text
      , backend = None Text
      }

in  { default = default, Type = TargetType }
```


## Adding a package to the registry

Can be done by *opening a pull request* to the repo - this operation can (and should) be entirely
automated by the package manager the author is using.

If the author is already including a package manifest that conforms to the `Package` schema
(in the case of a build tool using it as build manifest as well - e.g. Spago), then the only
content of this file would be a Dhall import.

E.g. if we're publishing version `v5.0.0` of the `prelude` package, then we just need
to add a file called `packages/prelude/v5.0.0.dhall`, containing the following:

```dhall
https://raw.githubusercontent.com/purescript/purescript-prelude/v5.0.0/spago.dhall
```

The above will import the `Package` manifest directly from the repo location, and it would
be as if the file in this repo will have the same contents (when evaluated by Dhall).

If there's no such manifest file in the source files (as it's the case of all the packages
published on the Bower registry before 2020), then we'll have to include a full fledged
`Package` manifest here. E.g. if we'd have to import the same `v5.0.0` of the `prelude` from
Bower we'd make the following manifest in `packages/prelude/v5.0.0.dhall`:

```dhall
let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "prelude"
    , license = Registry.License.BSD-3-Clause
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "purescript"
            , repo = "purescript-prelude"
            , version = "v5.0.0"
            }
        )
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies = [] : Registry.Dependencies
            }
          }
    }
```

----

Once the PR is open, the following will happen:
- the [registry curator](#The-Registry-Curator) will vet if it's good to merge
- if it is, then it will merge it to `master`, and note that a new package version has landed in the Registry
- then it goes to the relevant git tag, and fetches the sources from the package address
- packages them in a tarball together with any overrides we might have for that package manifest
  (see the [Overrides section](#Overriding-a-Package) for more details)
- computes the SHA256 of the tarball
- uploads the tarball and the hash **to a GitHub release** for hosting. 
  Note: every package has a dedicated GitHub release that holds all the versions of that package.
  E.g. for the `prelude` package there will be a release called `prelude`, that holds a tarball
  (and its hash) for each of the versions: `v4.1.1`, `v4.1.0`, etc. 
- generates docs for that version and uploads them to Pursuit

Note: upgrading an existing package consists of the same process, 
since it involves creating a new file containing a `Package` manifest.

The above process covers many of the goals we defined above:
- we store a copy of the packages' sources at the specified version, and this guarantees *immutability* for published packages (of course trusting that the registry itself would not change these)
- we protect the *integrity* of published packages by storing their hash
- we host the packages at no cost since they are normal GitHub releases
- we avoid implementing authentication/authorization ourselves by piggybacking on GitHub's accounts, avoiding potential security vulnerabilities from this
- we avoid implementing a backend for uploading packages, since every upload is done by the [curator](#The-Registry-Curator), that is not exposed to the public internet. This also avoids some security concerns
- there's a way for "Registry Trustees" to override details about some package version - e.g. outdated version bounds - without the need for the original author to republish a package.

## Downloading a package

A package manager should download packages in the following way:
- fetch the tarball and the hash with the right version from the package's GitHub release in this repo
- compute the SHA256 of the tarball and check that it matches the published one
- unpack the tarball and use the sources as desired in the compilation process


## Unpublishing a package/release

Unpublishing a version for a package can be done by removing the version file
associated with it.
This is allowed for security reasons (e.g. if some package was taken over maliciously),
but for a different set of reasons (i.e. the `leftpad` problem) it's allowed only for a set period of time.

The only allowed actors that can validate/approve such unpublishing are:
- the Registry Trustees
- and/or the Package Author

Proposed window for possible unpublishing: one week from the publishing of that version.

## Package Sets

As noted in the beginning, Package Sets are a first class citizen of this design.

We'll publish Package Sets inside this repo - you can find an example [here](./v1/sets/20200418.dhall).

Package authors/users can choose to use a Package Set rather than the Registry to be
the source of the packages in their builds, and to do that they just need to assign
the `package` key to such package set.

Example: if we have the following package manifest:

```dhall
let Registry = https://raw.githubusercontent.com/purescript/registry/master/v1/Registry.dhall

in  Registry.Package::{
    , name = "my-package"
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies = toMap { effect = "^2.0.0" }
            }
          }
    }
```

it's pulling the `effect` package from the Registry, so potentially it could pick up
the package `effect@2.0.1` if that would get published.

But maybe we don't want that, so to use a Package Set what we have to do is to
specify that in the `package` key, so that our manifest will change to be the following:

```dhall
let Registry = https://raw.githubusercontent.com/purescript/registry/master/v1/Registry.dhall

let upstream = https://raw.githubusercontent.com/purescript/registry/master/v1/sets/20200418.dhall

in  Registry.Package::{
    , name = "my-package"
    , packages = 
        Registry.Index.PackageSet
          { compiler = upstream.compiler
          , packages = toMap upstream.packages 
          }
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies = toMap { effect = "^2.0.0" }
            }
          }
    }
```

..and then we'd use that package set as the provider of our package versions.

### Overriding a Package

Building on the example above, what if we want to override the version of `effect`
in the set with a local package?

We'd then change our package manifest to look like this:

```dhall
let Registry = https://raw.githubusercontent.com/purescript/registry/master/v1/Registry.dhall

let upstream = https://raw.githubusercontent.com/purescript/registry/master/v1/sets/20200418.dhall

let overrides = { effect = From.Local ( ../my-effect/spago.dhall as Location ) }

in  Registry.Package::{
    , name = "my-package"
    , packages = 
        Registry.Index.PackageSet
          { compiler = upstream.compiler
          , packages = toMap (upstream.packages // overrides)
          }
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies = toMap { effect = "^2.0.0" }
            }
          }
    }
```

In general any [`Address`](./v1/Address.dhall) works as an override.

This is all good for Package Sets, but how about adding local packages when just
using the Registry as our provider?

It works in a similar way - e.g. to add our local copy of the `effect` package:

```dhall
let Registry = https://raw.githubusercontent.com/purescript/registry/master/v1/Registry.dhall

let overrides = { effect = From.Local ( ../my-effect/spago.dhall as Location ) }

in  Registry.Package::{
    , name = "my-package"
    , packages = Registry.Index.Registry (toMap overrides)
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies = toMap { effect = "^2.0.0" }
            }
          }
    }
```

## Registry Trustees and manifest editing

The "Registry Trustees" mentioned all across this document are a group of trusted
janitors that have write access to this repo.

Their main task will be that of editing package manifests that will need correction - most
commonly this is necessary because version bounds will be inaccurate.

> But you cannot publish two different package manifests under the same package version!  
> That's not quite a "reproducible" build..

This is right, and in fact the Trustees will have to work under a set of constraints
so that their activity will not cause disruption, unreproducible builds, etc.

First of all, if you read carefully you might notice that every package tarball has a
SHA256 associated with it - we're not planning on changing that SHA ever, so we cannot
actually publish a different tarball under the same version.

So how will Trustees actually be able to change manifests?

The constraints are:
- we'll enforce SemVer for package versions (via CI), and authors will have the
  `major`, `minor`, `patch` and `pre-release` segments available to them
- ..while the `build-metadata` is reserved for Trustees to publish new versions

So to recap, when Trustees need to edit a package manifest:
- they will make a new version file
- change whatever is necessary to override
- and bump the `build-metadata` in the version

## Name squatting and reassigning names

I don't feel like committing to anything right now, but I'd like to have something
[the `npm` policy](https://www.npmjs.com/policies/disputes) in this regard, where
people are expected to deal with name-related matters privately first and then
they are allowed to escalate to the Registry Trustees after a certain time.


## Migration path

Right now we hold the Bower registry as the "source of truth" for "which package does a name refer to".
This is to avoid naming inconsistencies, such as two different repos being regarded as the `prelude` package.

In order to provide a smooth migration path to have this Registry be the source of truth instead,
we will pre-populate this new registry with all the PureScript packages existing on the Bower registry.
A list of them (together with their location) can be found in the [`bower-packages.json`](./bower-packages.json) file - it has been obtained by crawling the Bower registry (through `libraries.io`) for all the packages starting with the `purescript-` prefix.

For all these published packages `Package` manifests have been generated, and are stored in this repo.
This the only thing we need in order to repackage these releases in order to upload them to the registry.

## The "Registry Curator"

What we refer throughout this document as "the registry curator" is a piece of software,
affectionately called [pacchettibotti](https://github.com/spacchetti/pacchettibotti).

See the repo for more documentation on it. The TLDR is that it automates everything
that should be automated in the Registry.

About the name: it's an "Italian-ish" pormanteau of "pacchetti" - "small packages" - and
"bot".
Note on pronunciation for English speakers: "che" is roughly pronounced "kae" in Italian.


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