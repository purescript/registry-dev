# Registry Spec

The untitled first 1-2 paragraphs, which explain what the registry is and what major concepts the spec will cover.

## 1. Introduction

A longer summary of the registry, why it came to exist, its high-level design. Akin to the introduction to a book (in spirit, not in length!).

**1.1 Important Terminology**

Quick definitions of words that will be used frequently throughout the document. Readers can breeze through this section and refer back to it if they forget to what a common term refers. Some of these terms are in common use (like "hash"), but they're included because we mean something specific when we use it (ie. "a base64-encoded subresource integrity hash using the SHA256 algorithm.")

Terms include:

"package", "package name", "package version", "version range", "SRI hash", "package location", "package manager", "registry", "metadata index", "manifest index", "storage backend", "registry trustee", "package set", "license"

## 2. Package Publishing

A simple walkthrough of how a package is published, possibly including diagrams. Includes example JSON payloads going through the registry `Addition` operation, an example of the input manifest, the output metadata and package sets entry. All extremely minimal, but giving a sense of the data interchange beginning with the package manager and ending with the registry metadata, index, and storage backend.

## 3. Schemas: Data Type Representations Used in the Registry

Types alone can't capture the set of invariants applied to data processed by the registry, so this section provides the full set of rules for common data types.

### 3.1 Compatibility Guarantees

A description of our forward-compatibility guarantees and what that means.

### 3.2 Atomic Data

Schemas for `PackageName`, `Version`, `Range`, `Location`, `Sha256`, `Owner`, `License`, and other simple data used in the registry. Each is a subsection which includes its rules, the representations in Dhall and JSON, and a link to the PureScript implementation in `Schema.purs`.

#### PackageName

**[Source](./lib/src/Registry/PackageName.purs)**
**[Spec](./types/v1/Manifest.dhall)**

Packages are uniquely identified by their `PackageName`. No two packages in the registry can share the same name. A package name represented as a `string`, with the following restrictions:

- Must be no more than 50 characters long
- Must contain only letters and digits, optionally separated by hyphens
- Must begin with a letter or digit
- Cannot contain consecutive hyphens
- Cannot begin with `purescript-`

The final point deserves elaboration. Historically, PureScript packages have been registered in the Bower registry using a `purescript-` prefix so as to avoid naming conflicts with JavaScript packages. However, the prefix is not actually part of the package name, so package managers like Spago refer to the package without the prefix. For example, the Bower-registered name `purescript-prelude` refers to the `prelude` package, which is located in the GitHub repository `purescript/purescript-prelude`. This prefix is not used in the PureScript registry. The `purescript-prelude` Bower package is simply `prelude` in the PureScript registry.

We still encourage users to name their repositories with a `purescript-` prefix to make it easier to find PureScript packages on platforms like GitHub. However, to prevent users from accidentally registering their packages with an unnecessary prefix, the registry will not accept packages that begin with `purescript-` unless the package author confirms this is what they want with the Registry Trustees.

#### Version

**[Source](./lib/src/Registry/Version.purs)**
**[Spec](./types/v1/Manifest.dhall)**

Packages are associated with one or more versions, representing their source code at a point in time. The registry uses a restricted version of the SemVer spec which only allows versions with major, minor, and patch places (that means no build metadata and no prerelease identifiers). A version is represented as a `string` with the following restrictions:

- The string must have the form `"X.Y.Z"`, representing major, minor, and patch places.
- The major, minor, and patch places must each be natural numbers (ie. whole numbers equal to or greater than 0).

We never use a `v` prefix on versions.

The major, minor, and patch places of a version describe the public interface of the library (ie. everything exported by its modules, such as types, instances, functions, or values). Each place has a specific meaning:

- `MAJOR`: Indicates a breaking change to the public interface, such as removing a function, changing its type, or changing its behavior in a significant way. Upgrading any dependency to a new major version also counts as a breaking change.
- `MINOR`: Indicates a non-breaking change to the public interface, such as adding a new function. Updating any dependency to a new minor version also counts as a minor change.
- `PATCH`: Indicates a documentation change or an internal change that does not change the public interface, such as fixing a bug in an implementation.

The `X.Y.Z` form maps on to these three kinds of change with the following rules:

1. If a package uses all three places (ie. it begins with a non-zero number, such as `"1.0.0"`), then the version implies `MAJOR.MINOR.PATCH`.
2. If a package only uses two places (ie. it begins with a zero, such as `"0.1.0"`), then the version implies `0.MAJOR.MINOR`.
3. If a package uses only one place (ie. it begins with two zeros, such as `"0.0.1"`), then all changes are potentially breaking changes, ie. `0.0.MAJOR`.

#### Range

**[Source](./lib/src/Registry/Range.purs)**
**[Spec](./types/v1/Manifest.dhall)**

It is sometimes necessary to refer to a range of versions, such as when specifying dependency versions or supported compiler versions. The registry uses a restricted form of SemVer ranges; a version range is represented as a `string` with the following restrictions:

- The string is always of the form `">=X.Y.Z <X.Y.Z"`.
- Both versions (`X.Y.Z`) must be valid [`Version`](#version)s.
- The first version must be less than the second version.

When comparing versions, the major place takes precedence, then the minor place, and then the patch place. For example:

- `1.0.0` is greater than `0.12.0`
- `0.1.0` is greater than `0.0.12`
- `0.0.1` is greater than `0.0.0`

For example: `">=1.0.1 <2.0.0"`

#### Location

**[Source](./lib/src/Registry/Location.purs)**
**[Spec](./types/v1/Location.dhall)**

The registry operates on a pull-based model, in which package authors ask the registry to fetch their code and produce a package instead of uploading their package themselves. Accordingly, every package is associated with the location the registry will use to fetch it. A location is represented in JSON as an `object`.

Packages can be located anywhere online so long as that location is publicy-accessible and the registry understands how to fetch code from it.

By convention, locations are represented in JSON using the following rules:

- Common fields (`url`, `owner`, `repo`) are prefixed with the provider name, ie. `githubOwner` or `gitlabRepo` or `gitUrl`.
- Providers that support a typical filesystem structure must support an optional `subdir` key so that the registry can provide monorepo support for that location type.

The currently-supported location types are listed below.

**Git Repository**

An arbitrary Git repository is represented by a JSON `object` with a required `gitUrl` key and optional `subdir` key. The Git URL must be of the form: `http[s]://host.xz/path/to/repo[.git]`. The registry uses the `git` CLI tool to fetch your repository. A JSON example:

```json
{
  "gitUrl": "https://git.sr.ht/~rj/purescript-image",
  "subdir": "lib"
}
```

**GitHub Repository**

The registry has special support for fetching Git repositories hosted on GitHub. Instead of cloning the repository it will fetch the tarball of the package contents. To specify a GitHub repository you must provide the name of the owner and of the repository, along with an optional subdirectory containing the package source. A JSON example:

```json
{
  "githubOwner": "purescript",
  "githubRepo": "purescript-prelude"
}
```

Or, using a monorepo:

```json
{
  "githubOwner": "purescript",
  "githubRepo": "purescript-core",
  "subdir": "prelude"
}
```

#### Owner

**[Source](./lib/src/Registry/Owner.purs)**
**[Spec](./types/v1/Owner.dhall)**

The registry relies on SSH key pairs to verify package ownership for the purposes of sensitive API operations like unpublishing versions or transferring packages. An `Owner` is made up of the three components of an SSH public key in text format ([RFC4253](https://www.rfc-editor.org/rfc/rfc4253#section-6.6)). That format looks like this:

`[keytype] [ssh-public-key] [comment (optional)]`

Note that the comment is optional and is referred to as the "id" in the registry. A JSON example:

```jsonc
{
  "keytype": "ssh-ed25519",
  "public": "ABCD3FGzaC1lZDI1NTE5AAAAINq4q0EHXacxMzmcG7TNC1DJpSxpK5dhJA6uAlZ",
  "id": "john@abc"
}
```

Alternately, this can be written without an id:

```jsonc
{
  "keytype": "ssh-ed25519",
  "public": "ABCD3FGzaC1lZDI1NTE5AAAAINq4q0EHXacxMzmcG7TNC1DJpSxpK5dhJA6uAlZ"
}
```

#### License

**[Source](./lib/src/Registry/License.purs)**
**[Spec](./types/v1/License.dhall)**

All packages in the registry must have a license that grants permission for redistribution of the source code. Concretely, the registry requires that all packages use an SPDX license and specify an [SPDX license identifier](https://spdx.dev/ids/). `AND` and `OR` conjunctions are allowed, and licenses can contain exceptions using the `WITH` preposition. The SPDX specification describes [how licenses can be combined and exceptions applied](https://spdx.dev/ids#how).

A `License` is represented as a string, which must be a valid SPDX identifier. For example:

`"MIT OR APACHE-2.0"`

#### Sha256

**[Source](./lib/src/Registry/Sha256.purs)**
**[Spec](./types/v1/Sha256.dhall)**

The registry produces an archive file when publishing a package. We use [tarballs](https://wiki.debian.org/TarBall) as our archive format. The registry produces a tarball when publishing a package. The hash of this tarball is recorded so that package managers can verify the integrity of packages they download from the registry. The hash is stored in the [subresource integrity (SRI) format](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity). A Sha256 is represented as a `string` in JSON. For example:

`"sha256-uy7gpfhgyj+3Ylw65ROY6YOXHoC0M7Acb11Cd7pf1GU="`

### 3.3 Manifest

**[Source](./lib/src/Registry/Manifest.purs)**
**[Spec](./types/v1/Manifest.dhall)**

All packages in the registry contain a `purs.json` manifest file in their root directory. The manifest file specifies information necessary for the registry to package the source code, and it also serves as a lingua franca for package managers: all package managers are expected to support this manifest format. The manifest file is an object with the following fields:

- `name`: a valid [`PackageName`](#packagename)
- `version`: a valid [`Version`](#version)
- `license`: a valid [`License`](#license)
- `location`: a valid [`Location`](#location)
- `owners` (optional): a non-empty array of [`Owner`](#owner)
- `description` (optional): a description of your library as a plain text string, not markdown, up to 300 characters
- `files` (optional): a non-empty array of globs, where globs are used to match files outside the `src` directory you want included in your package tarball
  - Globs must contain only `*`, `**`, `/`, `.`, `..`, and characters for Linux file paths. It is not possible to negate a glob (ie. the `!` character), and globs cannot represent a path out of the package source directory.
- `dependencies`: dependencies of your package as key-value pairs where the keys are [`PackageName`](#packagename)s and values are [`Range`](#range)s; this is a required field, but if you have no dependencies you can provide an empty object.
  - All dependencies you provide must exist in the registry, and the dependency ranges must be solvable (ie. it must be possible to produce a single version of each dependency that satisfies the provided version bounds, including any transitive dependencies).

For example:

```json
{
  "name": "control",
  "version": "4.2.0",
  "description": "Common control structures for PureScript",
  "license": "BSD-3-Clause",
  "location": {
    "githubOwner": "purescript",
    "githubRepo": "purescript-control"
  },
  "files": ["test/**/*.purs"],
  "dependencies": { "newtype": ">=3.0.0 <4.0.0", "prelude": ">=4.0.0 <5.0.0" }
}
```

### 3.4 Metadata

**[Source](./lib/src/Registry/Metadata.purs)**
**[Spec](./types/v1/Metadata.dhall)**

All packages in the registry have an associated metadata file, which is located in the `metadata` directory of the `registry` repository under the package name. For example, the metadata for the `aff` package is located at: https://github.com/purescript/registry/blob/main/metadata/aff.json. Metadata files are the source of truth on all published and unpublished versions for a particular package for what there content is and where the package is located. Metadata files are produced by the registry, not by package authors, though they take some information from package manifests.

Each published version of a package records three fields:

- `hash`: a [`Sha256`](#Sha256) of the compressed archive fetched by the registry for the given version
- `bytes`: the size of the tarball in bytes
- `publishedTime`: the time the package was published as an `ISO8601` string

Each unpublished version of a package records three fields:

- `reason`: a plain text string up to 300 characters long explaining why the version was unpublished
- `publishedTime`: the time the `version` was published, as an `ISO8601` string
- `unpublishedTime`: the time the `version` was unpublished, as an `ISO8601` string

The metadata for a given package contains up to four fields:

- `location`: a valid [`Location`](#location) representing where the registry is currently fetching this package's source code from
- `owners`: an optional non-empty array of [`Owner`](#owner)s, representing who is able to take authenticated actions for this package
- `published`: a map of package [`Version`](#version)s to published metadata as described above
- `unpublished`: a map of package [`Version`](#version)s to unpublished metadata as described above

If there are no published or unpublished versions then these fields contain empty objects. A JSON example:

```json
{
  "location": {
    "githubOwner": "purescript",
    "githubRepo": "purescript-prelude"
  },
  "published": {
    "1.0.0": {
      "bytes": 5579,
      "hash": "sha256-00bKlr9eKgTwrAsF+AE5rX7LN1rqij5yH9A78UKCL/I=",
      "publishedTime": "2020-02-27T21:10:55.0Z"
    }
  },
  "unpublished": {
    "1.0.1": {
      "reason": "Accidentally committed credentials",
      "publishedTime": "2020-02-27T22:10:55.0Z",
      "unpublishedTime": "2020-02-28T00:02:35.0Z"
    }
  }
}
```

### 3.5 Package Set

**[Source](./lib/src/Registry/PackageSet.purs)**

Package sets are stored in the `registry` repository under the `package-sets` directory. For example, the first package set was published at https://github.com/purescript/registry/blob/main/package-sets/0.0.1.json. Each package set is stored as a JSON file with the following fields:

1. `version`, which is a string containing the package set [`Version`](#version) for this set, which follows the version rules described in [Releasing the Package Set](#releasing-the-package-set).
2. `compiler`, which is a string containing the compiler [`Version`](#version) used to verify the package set.
3. `published`, which is a date string in YYYY-MM-DD format that describes which day this package set was produced.
4. `packages`, which is an object in which keys are [`PackageName`](#packagename)s and values are [`Version`](#version)s

For example, in JSON:

```json
{
  "version": "2.3.1",
  "published": "2024-04-19",
  "compiler": "0.16.3",
  "packages": {
    "aff": "9.0.0",
    "affjax": "7.0.0"
  }
}
```

### 3.6 Manifest Index

**[Source](./lib/src/Registry/ManifestIndex.purs)**

The registry maintains a cache of all package manifests in the manifest index, which is stored in the [`registry-index`](https://github.com/purescript/registry-index) repository. This index makes it convenient for package managers to look up the manifest for a particular package version at any time. The manifest index – just like the Registry itself - maintains the invariant that all dependencies of packages in the index are themselves in the index. The index can be regenerated from the package tarballs and metadata alone.

All manifests for a given package in the registry are cached in the manifest index according to the following rules, which follows the one [documented in the Cargo book](https://doc.rust-lang.org/cargo/reference/registries.html#index-format).

- Each package entry is a JSON Lines file where each line is a package manifest encoded in JSON and stored in sorted order ascending by version.
- Packages with 1-character names are stored in the directory named `1`.
- Packages with 2-character names are stored in the directory named `2`.
- Packages with 3-character names are stored in the directory `3/{first-character}` where `{first-character}` is the first character of the package name. For example, the `aff` package is stored in the `3/a` directory.
- All other packages are stored in directories named `{first-two}/{second-two}` where the top directory is the first two characters of the package name, and the subdirectory is the third and fourth characters of the package name. For example, the `prelude` package is stored in the `pr/el` directory.

## 4. Registry Infrastructure

A section specifying how the registry should relate to other infrastructure and the responsibility of that infrastructure as it relates to the registry.

**4.1 Package Storage**
Specifies how the storage backend works (data format, naming conventions, location)

**4.2 Manifest Index**
Specifies how the manifest index works (data format, purpose of it existing, where it is located)

**4.3 Metadata Index**
Specifies where package metadata is stored and in what format, naming conventions.

**4.4 Package Sets**
Specifies where the package sets are stored and in what format, naming conventions.

## 5. Registry Operations

The big section! This builds on everything provided so far and summarizes the role of the API and its major operations. Each operation is detailed along with its failure modes.

This is a good time for diagrams! (cc: @AndrewCondon, @JordanMartinez).Two notes:

1. This section is only about _behavior_. The data types used in the API have already been described in prior sections.
2. This section is only about _registry_ concerns. Actions taken after an operation (such as pushing to package sets, pursuit, the manifest index) are in the next section.

### Package Operations

The registry supports three package operations: publishing, unpublishing, and transferring. These operations are exposed via an HTTP API at [registry.purescript.org/api](https://registry.purescript.org). You are expected to provide a JSON body matching the operation data type in a POST request.

- `/api/v1/publish`: The `Publish` operation
- `/api/v1/unpublish`: The `Unpublish` operation
- `/api/v1/transfer`: The `Transfer` operation

Each operation is described below.

#### 5.1 Publish a Package

**[Source](./lib/src/Registry/Operation.purs)**

Publishing a package version results in a few steps:

1. The package source is fetched by the registry.
2. The JSON payload sent to the API, the package manifest, the source code, and the package metadata (if it exists) are verified together.
3. The source code is packaged into a tarball.
4. The package tarball is uploaded to the registry storage backend so package managers can download it.
5. The metadata index and manifest index are updated to record the newly-published version.
6. The package version is added to the candidates for the automatic package sets releases.

To submit a `Publish` operation, you are expected to POST a JSON object with the below fields to the registry HTTP API:

- `name`: A [`PackageName`](#packagename).
- `location`: A [`Location`](#location). Optional if the package has had other versions published; required if this is the first version of the package to be published.
- `ref`: A `string` representing a reference (for example, a Git commit or Git tag) at the target location to use to fetch the source code.
- `compiler`: A [`Version`](#version) representing which compiler version to use to compile the package in the Registry verification checks.
- `resolutions`: An optional `object` containing dependency resolutions, where keys are [`PackageName`](#packagename)s and values are [`Version`](#version)s.

For example, in JSON:

```json
{
  "name": "prelude",
  "location": { "githubOwner": "purescript", "githubRepo": "prelude" },
  "ref": "v5.0.0",
  "compiler": "0.15.0"
}
```

**Verification Steps**

For a `Publish` operation to succeed, the provided JSON payload must decode successfully and the following verification checks must be satisfied. Package verification is separated into stages depending on what information the registry has available.

First, the registry uses the [`PackageName`](#packagename) listed in the JSON payload to read any existing [`Metadata`](#34-metadata) for the package. To be publishable, one of these two conditions must be true:

1. The package name is not registered (ie. no metadata exists) AND a location is provided in the payload. In this case, the registry will create new metadata to register this package at the given location. It is possible to register a package at the same location as one that already exists.
2. The package name is registered (ie. metadata exists) AND either no location is provided in the payload OR the provided location matches the location recorded in the metadata. When no location is provided, the registry will use the location from the metadata.

Next, the registry will use the provided [`Location`](#location) to fetch the package source at the provided `ref`. Packages must contain a `purs.json` manifest or `spago.yaml` manifest in the package root (in the presence of multiple manifest files, the `purs.json` file takes precedence and others are ignored). The registry will verify that the manifest matches with the JSON payload and metadata:

1. The manifest MUST be usable to produce a well-formed [`Manifest`](#33-manifest).
2. The manifest package name and JSON payload package name MUST match.
3. The manifest location and JSON payload location (if provided) MUST match.
4. The manifest version MUST NOT have been published or unpublished before, according to the metadata file.
5. Either a `resolutions` key was provided in the JSON payload, in which case the resolutions MUST include every dependency listed in the manifest dependencies at a version within the specified version bounds for each dependency, OR the manifest dependencies MUST be solvable by the registry solver. ALL package versions indicated in the resolutions MUST already be registered.
6. If the package source code includes a LICENSE file and/or a license listed in a bower.json or package.json manifest, then all licenses MUST match with the license specified in the PureScript [`Manifest`](#33-manifest). For example, if the package.json file specifies the "MIT" license and a LICENSE file specifies "BSD-3-Clause", then the PureScript license should admit at least "MIT AND BSD-3-Clause".

Next, the registry will verify the package source code.

1. The package source MUST contain a `src` directory, which itself must contain at least one file with a `.purs` extension.
2. The package source MUST compile successfully using the compiler version provided in the JSON payload and either the dependencies provided via the `resolutions` key in the JSON payload or the registry-solved resolutions. The registry will compile all PureScript code contained in the `src` directory.

Finally, the registry will perform some processing on the source code, package the source into a tarball, and complete the publishing process. Along the way, we do some final verification:

1. The package `src` directory, always-included files (listed below), and any files explicitly included via the manifest `files` key are copied into a temporary directory.
2. If the project only contained a `spago.yaml` file, then a `purs.json` file is generated and copied into the temporary directory. Package tarballs **always** include a `purs.json` file.
3. Always-ignored files (listed below) are removed from the source code.
4. The remaining code is packaged into a tarball. The tarball MUST NOT exceed 2,000kb, and a warning will be issued for packages over 200kb.

These files are always included in the tarball, if present:

- The full contents of the `src` directory.
- The `purs.json`, `spago.yaml`, `spago.dhall` and `packages.dhall`, `bower.json`, and `package.json` manifest formats (in the root of the package).
- Any README or LICENSE files (in the root of the package).

These files are always excluded from the tarball, regardless of what is specified in the `files` key:

- `.psci`, `.psci_modules`, `.spago`, `node_modules`, `bower_components`, `.git`, `CVS`, `.svn`, and `.hg` directories.
- `package-lock.json`, `yarn.lock`, and `pnpm-lock.yaml` files.
- `.swp`, `._*`, and `.DS_Store` files.

Mandatory file includes and excludes are subject to change at the registry's discretion over time.

**Metadata Changes**

When a package is published its metadata entry is updated according to the following rules:

1. If the metadata entry did not exist, then a new [`Metadata`](#34-metadata) is created for the package with all fields taken from the manifest file.
2. If the metadata entry did exist, then the file is updated: the new version is inserted into the `published` field, and if the `owners` field in the manifest differs from the metadata then the metadata is overwritten.

#### 5.2 Authentication

Anyone can publish a package version to the registry, but only package [`Owner`](#owner)s can unpublish a package version or transfer a package. In this section we will describe how the registry handles authentication; [Section 5.3](#53-unpublish-a-package-authenticated) and [Section 5.4](#54-transfer-a-package-authenticated) describe the unpublish and transfer operations in particular.

A package "owner" is a person with their public SSH key listed in the `owners` field of the package [`Manifest`](#33-manifest), which is also mirrored to the package [`Metadata`](#34-metadata). By extension, anyone with access to the package source and therefore access to change the package manifest is able to set themselves or others as a package owner.

Being a package "owner" grants the ability to take sensitive actions on behalf of a package: specifically, to unpublish versions (with restrictions) or to transfer the package to a new location. The registry `Transfer` and `Unpublish` operations (detailed in the following two sections) enable package owners to take these actions.

> Note: the @pacchettibotti GitHub account is controlled by the Registry Trustees and is able to act as a package "owner" for any package. If the Registry Trustees ever need to unpublish a package version or transfer a package, they must do so by signing an authenticated operation using the @pacchettibotti SSH keys.

The registry relies on SSH to authenticate package owners. An `Owner` is a public SSH key, and this SSH key can be used to sign data using the SSH library functions provided in the registry library and (most likely) exposed by your package manager. The registry in turn verifies the signed data using the `Owner`. To submit an authenticated operation, you are required to take the following steps:

1. Construct the JSON payload for the operation
2. Sign the JSON payload using an SSH key listed in the package's `owners` metadata field, where the signature is hex-encoded.
3. Construct the JSON payload for an authenticated operation using the JSON payload from step (1) and the signature from step (2).

For example, here's a stringified JSON payload for an `Unpublish` operation unpublishing `prelude@1.0.1` because credentials were accidentally committed:

```json
"{ \"name\": \"prelude\", \"version\": \"1.0.1\", \"reason\": \"Accidentally committed credentials\" }"
```

Then, assemble this information into a JSON `object` with three fields:

- `payload`: The JSON string representing the `Unpublish` or `Transfer` operation
- `signature`: A hex-encoded SSH signature (you can use your package manager, such as Spago, or you can use functions from the `registry-lib` directory to sign a payload and get a hex-encoded signature back).

For example, in JSON:

```json
{
  "payload": "{ \"name\": \"prelude\", \"version\": \"1.0.1\", \"reason\": \"Accidentally committed credentials\" }",
  "signature": "1f4967eaa5de1076bb2185b818ea4fb7c18cfe83af951ab32c3bcb4a300dfe9b3795daaae1e7a6d5fb9f72c4cec8003f79a452f2dc9da9ec8cfa63b243c80503"
}
```

**Verifying SSH Signatures**

When the registry receives an authenticated operation it takes the following steps:

1. The registry will retrieve all package owners from the package's metadata
2. The registry will use each key listed in the package owners to attempt to verify the signature on the authenticated operation. A pacchettibotti signature is also always considered valid; only Registry Trustees have access to this key and can submit authenticated operations as pacchettibotti.
3. If the signature was valid and the JSON operation payload was well-formed, then the registry will execute the provided operation.

#### 5.3 Unpublish a Package (Authenticated)

**[Source](./lib/src/Registry/Operation.purs)**

Unpublishing a package version means that the following things happen:

1. The package version is moved from the `published` to the `unpublished` section of the package's metadata entry in the metadata index.
2. The package version is removed from package storage and the manifest index, which means that package managers can no longer install it and it will no longer be included in build plans.

Notably, the package version will remain listed in the package sets, rendering those package sets invalid. To minimize the potential disruption caused by unpublishing a package, the following conditions must be met for a package to be unpublished:

1. The package version must have been published (ie. you cannot unpublish a version that does not exist).
2. The version must have been published within the last 48 hours OR the registry must legally unpublish the package (for example, the registry has received a DMCA takedown notice).

To unpublish a package you must construct a JSON payload of the form below and authenticate it according to the process described in [Section 5.2 (Authentication)](#52-authentication). An `Unpublish` operation is represented as an `object` with three fields:

- `name`: A [`PackageName`](#packagename)
- `version`: A [`Version`](#version)
- `reason`: A `string` of up to 300 characters describing why the package version was unpublished.

For example, in JSON:

```json
{
  "name": "halogen-hooks",
  "version": "2.0.0",
  "reason": "Accidentally committed key."
}
```

#### 5.4 Transfer a Package (Authenticated)

**[Source](./lib/src/Registry/Operation.purs)**

Transferring a package version means that the following things happen:

1. The package `location` is changed in the package's metadata index entry to the new location

The package location is only used when publishing package versions, so this change will not affect any versions of the package which have previously been published. When verifying a transfer operation, the registry will ensure:

1. The package must have been published before (ie. you cannot transfer a package that has not yet been registered).
2. The location provided in the JSON payload MUST NOT be the same as the location listed in the package's metadata or in any other package's metadata (ie. the location cannot already be in use by the indicated package or any other).

To transfer a package you must construct a JSON payload of the form below and authenticate it according to the process described in [Section 5.2 (Authentication)](#52-authentication). A `Transfer` operation is represented as an `object` with two fields:

- `name`: A [`PackageName`](#packagename)
- `newLocation`: A [`Location`](#location)

For example, in JSON:

```json
{
  "name": "halogen-hooks",
  "newLocation": {
    "githubOwner": "purescript-halogen",
    "githubRepo": "purescript-halogen-hooks"
  }
}
```

### Package Set Operations

The registry supports one package set operation: a bulk package set update. This operation is exposed via GitHub issues in the [purescript/registry](https://github.com/purescript/registry) repository. You are expected to open an issue containing a JSON body matching the bulk package set update schema.

#### 5.5 Update the Package Set

**[Source](./lib/src/Registry/Operation.purs)**

Anyone can suggest a package set update to the registry. However, community members can only add or upgrade packages in the package set. To remove a package or downgrade a package version, the update must be submitted by a member of the packaging team or a Registry Trustee.

A package set update must be submitted to the registry via GitHub issues on the registry repository. The body of the issue should contain the JSON object specified below, optionally within a code fence.

A package set update is an object with two keys: `compiler`, an optional field that, if set, will update the compiler version used to compile the package sets (it cannot be downgraded), and `packages`, an object where keys are package names and values are either a version number or `null`. A version number indicates the package should be added to the set or updated to the given version, and `null` indicates the package should be dropped from the package set.

```jsonc
{ // Sets the package set compiler version to 0.15.2
  "compiler": "0.15.2",
  "packages" {
    // Updates the `aff` package to v8.0.0
    "aff": "8.0.0",
    // Removes the `argonaut` package from the package sets altogether
    "argonaut": null
  }
}
```

If the package set operation JSON is well-formed, then we take three steps:

1. Authenticate the operation
2. Verify the new package set
3. Release the new package set

##### Authentication

Only Registry Trustees are allowed to advance the compiler version of a package set, remove packages, or downgrade packages. If any of these actions is implied by the JSON payload then the operation must be authenticated.

Authentication is handled via GitHub. The registry will verify that the user ID used to submit the package set update is a member of the @purescript/packaging team (ie. the Registry Trustees) under the PureScript organization. If not, the operation is rejected. If so, the operation may continue.

##### Verifying the Package Set

The suggested new package set can only be released if:

1. All packages in the set depend only on other packages in the set
2. All packages in the set can be compiled together

To verify the package set, we take the following steps:

1. We apply the suggested changes to the package set and verify that the package set is still self-contained. If not, the update is rejected.
2. We install the previous package set and compile it. This ensures we are beginning from a known good state.
3. We install the new package set (uninstalling any packages that are removed) and compile the package set again. If compilation fails then the update is rejected.

When we have verified the package set is self-contained and all packages compile together then we can release the package set.

##### Releasing the Package Set

"Releasing" a package set means assigning it a Registry version and writing the set to the `package-sets` directory in the registry repository. To calculate the new version for a package set we follow these rules:

1. If the highest SemVer upgrade in the set was a major version, then the package set increments a major version.
2. If the highest SemVer upgrade in the set was a minor version, or any new packages were added to the package set, then the package set increments a minor version.
3. If the highest SemVer upgrade in the set was a patch version, then the package set increments a patch version.

For example, if the previous release was `2.1.1`, and the package set update adds a new package, then the new version would be `2.2.0`.

## 6. Post-Publishing Operations

Summarizes what operations the registry will take after a package is published, transferred, or unpublished. This section _could_ be folded back into the previous section, but I worry that doing so would make that section too long and unfocused.

#### 6.1 Update Manifest Index

A spec of how the registry attempts to update the manifest index.

#### 6.2 Publish to Pursuit

A spec of how the registry attempts to publish to Pursuit, including compiling the documentation using the user-provided build plan, and any checks we perform. A note that if this step fails you can retry by resubmitting a publish operation.

#### 6.3 Publish to Package Sets

The registry attempts to produce a new package set automatically every day, so long as packages have been uploaded that could be added or updated. No packages are ever dropped from a package set automatically; the only time packages are dropped from the package sets are during manual releases.

Every day, the registry executes the following steps:

First, we read the contents of the latest package set release and gather all package versions that have been uploaded to the registry since that release. These package versions are the "batch" of packages that we are considering for automatic inclusion to the next package set.

Second, we filter out any packages where, based on their metadata and manifest files alone, we know they can't be added to the package set. This happens for one of three reasons:

1. They have a dependency that is neither in the package sets nor in the batch that is up for consideration
2. They have had multiple releases since the last package set, in which case we only take the highest version
3. They already have a higher version published in a previous package set

Third, we attempt to add the rest of the batch of package versions to the package set. Processing the batch follows these steps:

1. We install the previous package set and compile it.
2. We attempt to upgrade all package versions from the batch at once in the package set. Once the new versions are installed, we compile the package set. If it succeeds, then we're done.
3. If we couldn't compile the whole batch, then we order the batch first by their dependencies and then by their upload time. Packages with no dependencies on other packages in the batch go first, and ties are broken by upload time: older uploads go first. Then, we attempt to add packages to the package set one-by-one.
4. If a package fails to compile with the rest of the package set, then it is filtered from the batch.
5. Once there are no more packages to consider in the batch, the new package set is ready.

Fourth, we release the new package set (if we could produce one). Automatic package sets follow the versioning policy described in [Section 5.5 (Update the Package Set)](#releasing-the-package-set).

## 7. Non-JavaScript Backends

A section summarizing the aliasing solution used to support alternate backends, along with a status note stating that this is currently un-implemented.

## 8. Package Managers

Specifies how package managers should relate to the registry, ie. open a GitHub issue with a payload according to the `Operation` data type, get package information from `registry-index`, download packages from the storage backend, etc. Refer to the 'constants' module.

This section is meant to be reasonably self-contained, so someone writing a new package manager can understand what they need to do and what resources they have available in one place.

## 9. Policies

Policies followed by the registry — though maybe these are better-suited to be stored as separate specs apart from this document.

### 9.1 Registry Trustees

A summary of the actions that registry trustees are permitted to take and when they should be taken.

### 9.2 Name Squatting & Reassigning Names

Our policy on changing package names

### 9.3 Package Sets

The registry provides a curated package set that lists packages known to build together with a particular compiler version. Package managers such as Spago use this package set to decide what versions of your dependencies to install. To learn more about the package set file format and where package sets are stored, see the [3.5 Package Set](#35-package-set) specification.

#### Package Set Naming Conventions

Package sets use the same versioning scheme as packages in the registry: semantic versions with no build metadata or prerelease identifiers. The package set version can be used to determine if is is safe for your project to update to a new package set. We version according to the following rules:

1. The major version is incremented when breaking changes occur in the package set: a package is removed, or a major version of a package in the set is incremented.
2. The minor version is incremented when new packages are added to the package set, or when there was a minor version update in the package set.
3. The patch version is incremented when a patch version was updated in the package set.

#### Package Sets Release Schedule

PureScript package sets are released once per day if there have been any changes (one or more packages has been added, removed, or updated). Most package set releases are automated, but there are various scenarios in which the PureScript packaging team (@purescript/packaging) must manually intervene to resolve conflicts. It is possible for there to be multiple package sets released on the same day due to manual releases.

#### Manual Intervention in the Package Sets via the Package Sets API

Most changes in the package set are handled automatically. However, sometimes the package sets require manual intervention. For example, when a new package version introduces changes which are incompatible with other packages in the set, then it is not automatically added because that would entail dropping other packages from the set, and that is never done automatically. Or, sometimes several packages update together over the course of a few days, and while none of them can be added individually, they could all be upgraded together in one batch.

In these situations, contributors to the registry can use the package sets API to manually suggest one or more updates to packages in the package set. If the suggested updates result in a valid new package set, then it is accepted, built, and released automatically by the registry. If they do not, then the update is rejected and there is no release.

Please see [Section 5.5 (Update the Package Set)](#55-update-the-package-set) for instructions on processing a package set update.

The Registry Trustees will use their discretion when deciding how to process a package update that will entail dropping packages from the set. If the package update must be processed immediately (for example, there is a security issue), then they will act immediately. But in most cases they will use the following process:

1. Notify the authors of to-be-dropped packages that their package will be dropped unless it is updated
2. Wait for a period of time (usually a week) to give package authors a chance to update their packages
3. Perform a batch update to the package set

This will result in a new major version package set release.
