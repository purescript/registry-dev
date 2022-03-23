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

**9.1 Registry Trustees**
A summary of the actions that registry trustees are permitted to take and when they should be taken.

**9.2 Name Squatting & Reassigning Names**
Our policy on changing package names

**9.3 Package Sets**
The package sets release schedule, how we address conflicts when a package causes breakage, naming conventions, etc. This is the "soft" part — not the schema or location, just our release schedule and how we handle conflicts and name things.
