# Registry Spec

The untitled first 1-2 paragraphs, which explain what the registry is and what major concepts the spec will cover.

## 1. Introduction

A longer summary of the registry, why it came to exist, its high-level design. Akin to the introduction to a book (in spirit, not in length!).

**1.1 Important Terminology**

Quick definitions of words that will be used frequently throughout the document. Readers can breeze through this section and refer back to it if they forget to what a common term refers. Some of these terms are in common use (like "hash"), but they're included because we mean something specific when we use it (ie. "a base64-encoded subresource integrity hash using the SHA256 algorithm.") 

Terms include:

"package", "package name", "package version", "version range", "SRI hash", "package location", "package metadata", "package manager", "registry", "registry index", "storage backend", "registry trustee", "package set", "license"

## 2. Package Publishing

A simple walkthrough of how a package is published, possibly including diagrams. Includes example JSON payloads going through the registry `Addition` operation, an example of the input manifest, the output metadata and package sets entry. All extremely minimal, but giving a sense of the data interchange beginning with the package manager and ending with the registry metadata, index, and storage backend.

## 3. Schemas: Data Type Representations Used in the Registry

Types alone can't capture the set of invariants applied to data processed by the registry, so this section provides the full set of rules for common data types.

**3.1 Compatibility Guarantees**
A description of our forward-compatibility guarantees and what that means.

**3.2 Formats**
A brief overview of different formats used to represent Registry data (Dhall, JSON, and PureScript), and when they are used.

**3.3 Atomic Data**
Schemas for `PackageName`, `Version`, `Range`, `Location`, `Hash`, `Owner`, `License`, and other simple data used in the registry. Each is a subsection which includes its rules, the representations in Dhall and JSON, and a link to the PureScript implementation in `Schema.purs`.

**3.4 Manifest**
Schema for the manifest format.

**3.5 Metadata**
Schema for the metadata format.

**3.6 Registry API**
Schemas for the registry API, including `Addition`, `Update`, `Transfer`, and `Unpublish`. Also includes `Authenticated` constructor.

**3.7 Package Sets**
Schemas for the package sets

## 4. Registry Infrastructure

A section specifying how the registry should relate to other infrastructure and the responsibility of that infrastructure as it relates to the registry.

**4.1 Registry Index**
Specifies how the registry index works (data format, purpose of it existing, where it is located)

**4.2 Storage Backend**
Specifies how the storage backend works (data format, naming conventions, location)

**4.3 Package Metadata**
Specifies where package metadata is stored and in what format, naming conventions.

**4.4 Package Sets**
Specifies where the package sets are stored and in what format, naming conventions.

## 5. Registry Operations

The big section! This builds on everything provided so far and summarizes the role of the API and its major operations. Each operation is detailed along with its failure modes.

This is a good time for diagrams! (cc: @AndrewCondon, @JordanMartinez).Two notes:

1. This section is only about _behavior_. The data types used in the API have already been described in prior sections.
2. This section is only about _registry_ concerns. Actions taken after an operation (such as pushing to package sets, pursuit, the registry index) are in the next section.

**5.1 Register a Package**
Walkthrough of package registration. Includes the various checks performed to ensure a package is OK.

**5.2 Update a Package**
Walkthrough of updating a package version.

**5.3 Authenticated Operations**
Walkthrough of how authentication works in the registry, note that it applies to the following operations.
  
  **5.3.1 Transfer a Package**
  Walkthrough of the `Transfer` operation, assuming the operation was properly authenticated.
  
  **5.3.2 Unpublish a Package**
  Walkthrough of the `Unpublish` operation, assuming the operation was properly authenticated.

## 6. Post-Publishing Operations

Summarizes what operations the registry will take after a package is added / published, transferred, or unpublished. This section _could_ be folded back into the previous section, but I worry that doing so would make that section too long and unfocused.

**6.1 Update Registry Index**
A spec of how the registry attempts to update the registry index.

**6.2 Publish to Pursuit**
A spec of how the registry attempts to publish to Pursuit, including compiling the documentation using the user-provided build plan, and any checks we perform. A note that if this step fails you can retry by resubmitting a publish operation.

**6.3 Publish to Package Sets**
A spec of how the registry attempts to add the package to the package sets, how it handles failure (package doesn't compile with the set). Link to the 'package sets policy' section that lays out our full package sets policy.

## 7. Non-JavaScript Backends

A section summarizing the aliasing solution used to support alternate backends, along with a status note stating that this is currently un-implemented. 

## 8. Package Managers

Specifies how package managers should relate to the registry, ie. open a GitHub issue with a payload according to the `Operation` data type, get package information from `registry-index`, download packages from the storage backend, etc.

This section is meant to be reasonably self-contained, so someone writing a new package manager can understand what they need to do and what resources they have available in one place.

## 9. Policies

Policies followed by the registry — though maybe these are better-suited to be stored as separate specs apart from this document.

### 9.1 Registry Trustees
A summary of the actions that registry trustees are permitted to take and when they should be taken.

### 9.2 Name Squatting & Reassigning Names
Our policy on changing package names

### 9.3 Package Sets

The registry provides a curated package set that lists packages known to build together with a particular compiler version. Package managers such as Spago use this package set to decide what versions of your dependencies to install.

#### Package Set Files

Package sets are stored in the `registry` repository under the `package-sets` directory. Each package set is stored as both a Dhall file and a JSON file. A package set is an object or record comprised of the following fields:

1. `version`, which is a string containing the SemVer package set version for this set.
2. `compiler`, which is a string containing the SemVer compiler version used to verify the package set.
3. `date`, which is a date string in YYYY-MM-DD format that describes which day this package set was produced.
4. `packages`, which is an object in which keys are package names and values are package versions (a SemVer version).

For example, in JSON:

```json
{
  "version": "2.3.1",
  "date": "2022-04-19",
  "compiler": "0.14.9",
  "packages": {
    "aff": "5.0.0",
    "affjax": "5.0.0"
  }
}
```

#### Package Set Naming Conventions

Package sets use the following naming convention:

`MAJOR.MINOR.PATCH+YYYY-MM-DD-purs-MAJOR.MINOR.PATCH`

The first major/minor/patch version refers to the package set version. The date refers to the date the package set was produced. The `purs-` major/minor/patch version refers to the compiler version used to produce the set. Here's an example package set version:

`2.3.1+2022-04-19-purs-0_14_9`

This information is also contained in the package set file itself.

The package sets version number can be used to determine if it is safe for your project to update to a new package set. We version according to the following rules:

1. The major version is incremented when breaking changes occur in the package set: a package is removed, or a major version of a package in the set is incremented.
2. The minor version is incremented when new packages are added to the package set, or when there was a minor version update in the package set.
3. The patch version is incremented when a patch version was updated in the package set.

#### Package Sets Release Schedule

PureScript package sets are released once per day if there have been any changes (one or more packages has been added, removed, or updated). Most package set releases are automated, but there are various scenarios in which the PureScript packaging team (@purescript/packaging) must manually intervene to resolve conflicts. It is possible for there to be multiple package sets released on the same day due to manual releases.

#### Automatic Package Sets Process

The registry attempts to produce a new package set automatically every day, so long as packages have been uploaded that could be added or updated. No packages are ever dropped from a package set automatically; the only time packages are dropped from the package sets are during manual releases.

Every day, the registry CI executes the following steps:

First, we read the contents of the latest package set release and gather all package versions that have been uploaded to the registry since that release. These package versions are the "batch" of packages that we are considering for automatic inclusion to the next package set.

Second, we filter out any packages where, based on their metadata and manifest files alone, we know they can't be added to the package set. This happens for one of three reasons: they have a dependency that isn't in the package sets, or they had multiple releases since the last package set, in which case we only take the highest version, or c) they already have a higher version number released  in the previous package set. In any of these cases, we remove the package from consideration.

Third, we attempt to add the rest of the batch of package versions to the package set. Processing the batch follows these steps:

1. We install the previous package set and compile it.
2. We attempt to upgrade all package versions from the batch at once in the package set. Once the new versions are installed, we compile the package set. If it succeeds, then we're done.
3. If we couldn't compile the whole batch, then we order the batch first by their dependencies and then by their upload time. Packages with no dependencies on other packages in the batch go first, and ties are broken by upload time: older uploads go first. Then, we attempt to add packages to the package set one-by-one.
4. If a package fails to compile with the rest of the package set, then it is filtered from the batch.
5. Once there are no more packages to consider in the batch, the new package set is ready.

Fourth, we release the new package set (if we could produce one). Automatic package sets follow the below versioning policy:

1. If the highest SemVer upgrade in the set was a major version, then the package set increments a major version.
2. If the highest SemVer upgrade in the set was a minor version, or any new packages were added to the package set, then the package set increments a minor version.
3. If the highest SemVer upgrade in the set was a patch version, then the package set increments a patch version.

For example, if the previous release was `2.1.1+2022-06-01-purs-0_15_2` (`2.1.1` for short), and the next day no packages changed versions but a new package was registered, the new version would be `2.2.0+2022-06-02-purs-0_15_2` (`2.2.0` for short).

#### Manual Intervention in the Package Sets

Most changes in the package set are handled automatically. However, sometimes packages will introduce changes that cause the package set to no longer compile. In these instances, the now-incompatible package will not be automatically upgraded. Instead, the new version will be recorded and the Registry Trustees will need to manually intervene to fix the package set.

There are two options for fixing a package set:

1. The updated package can remain in the set at its old version (this is the default).
2. The updated package can be upgraded to its new version, and all packages that fail to compile with the new version will be dropped from the package set.

The Registry Trustees will use their discretion when deciding when to process a package update when it will cause other packages to be dropped from the set. They will judge how critical the package update is (does it fix a serious issue in a widely-used library?) and use that to determine if the update needs to happen right away. If not, then they will:

1. Notify authors of packages that will be dropped from the set that their package will be dropped unless it is updated
2. Wait for a period of time (usually a week) to give package authors a chance to update their packages
3. Perform a batch update to the package set, updating as many packages as possible in one go and dropping any packages that are no longer compatible with the set.

This will result in a new major version package set release.
