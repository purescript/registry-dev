# Dependency upgrade audit — 2026-10-11

## Scope and method

Baseline: the pushed `maintenance/nix-2026-10` branch (PR #802). Versions below
are **locked versions**, not the lower bounds of the old manifest ranges.
Targets were checked against npm's live `latest` dist-tags on 2026-10-11;
prereleases were excluded. All 15 distinct direct third-party npm dependencies
are covered, including the four already at latest. Eleven advance.

For changed packages, this review compared the old and new **published npm
tarballs**, upstream tagged source, intervening commits, and release notes or
changelogs where available. This matters for generated distributions such as
AWS and Octokit. The links below identify the exact comparison, not just a
project homepage. Development-only upstream changes do not affect our runtime.

The binding review checked imports/exports, arguments/options/defaults, result
shapes, exceptions, Promise/callback behavior, and runtime/native requirements.
Real dependencies were exercised through our FFI, including real loopback HTTP
with the actual SDKs, serializers, signers, transport, retry and throttling
plugins. Those probes do not substitute SDK mocks for the packages being
upgraded. They do not claim compatibility with a live GitHub or DigitalOcean
account; no production service or credential was used.

### Manifest and tooling inventory

- npm: root `package.json` and `package-lock.json`; workspaces
  `app/package.json`, `foreign/package.json`, `lib/package.json`. There are no
  first-party devDependencies or additional workspace npm lockfiles. Local
  `registry-*` file dependencies remain local, not npm release targets.
- Spago: root `spago.yaml` and `spago.lock`; package configurations in `app/`,
  `app-e2e/`, `dashboard/`, `foreign/`, `lib/`, `scripts/`, and `test-utils/`.
- The package manifests under `app/fixtures/licenses/{ambiguous-gfdl,
  deprecated-agpl,halogen-hooks}/` are historical license-detection inputs,
  not installed first-party dependencies. Changing them would change the tests.
- `.agents/setup` runs the root npm/Spago commands using Nix. The dashboard
  workflow installs `purescript@0.15.16`, `spago@1.0.4`, and unversioned `esbuild`
  with `--no-save`. Compiler and Spago pins are already the latest published
  stable versions. esbuild currently resolves to 0.28.2; it has no old npm lock
  entry. Tooling verification is recorded below.
- Nix's toolchain and `flake.lock` remain those of PR #802. The npm dependency
  hash in `nix/overlay.nix` is regenerated. No package overrides, Node patches,
  SQLite source-build hooks, compiler inputs, or new packaging exceptions were
  introduced. `CONTRIBUTING.md` and `deploy.yml` are unchanged.

## Direct JavaScript dependencies

### @aws-sdk/client-s3: 3.1009.0 → 3.1149.0 (foreign)

Sources: [full comparison](https://github.com/aws/aws-sdk-js-v3/compare/v3.1009.0...v3.1149.0),
[S3 changelog](https://github.com/aws/aws-sdk-js-v3/blob/v3.1149.0/clients/client-s3/CHANGELOG.md),
[command builder](https://github.com/aws/aws-sdk-js-v3/blob/v3.1149.0/clients/client-s3/src/commandBuilder.ts),
[runtime configuration](https://github.com/aws/aws-sdk-js-v3/blob/v3.1149.0/clients/client-s3/src/runtimeConfig.ts).
The upstream [upgrade guide](https://github.com/aws/aws-sdk-js-v3/blob/v3.1149.0/UPGRADING.md)
is a v2-to-v3 guide; this upgrade stays within v3.

Changes across this interval include binary-decision-diagram endpoint
resolution and endpoint cache fixes; thinner generated command classes; S3
middleware consolidation; internal XML parsing; checksum implementation
consolidation and additional MD5, SHA512 and XXHash algorithms; retry fixes
(including errors in HTTP 200 responses, adaptive throttling and transient DNS
errors); and core error fallback/metrics changes. Service-model additions cover
directory-bucket metrics/inventory, annotations, AWS Backup access points,
Object Lock event holds, inventory reference dates, region/storage-class and
checksum enum additions, and outpost access-point validation. TypeScript and
internal import reorganizations do not change our command constructors.

`foreign/src/Foreign/S3.js` uses named exports `S3`, `ListObjectsCommand`,
`PutObjectCommand`, `DeleteObjectCommand`, and `DeleteObjectsCommand`. It passes
explicit endpoint, region, static credentials, and `forcePathStyle: false`;
commands use `Bucket`, `Prefix`, `Marker`, `Key`, buffer `Body`, `ACL`, and
`Delete: { Objects: [{ Key }], Quiet: true }`. All remain supported. The four
commands still return Promises with the same consumed `Contents`, `ETag`,
delete header fields and pagination fields. Missing `Contents` still maps to
`[]`. New optional output fields are ignored. Default checksum calculation
continues to run; it is not disabled to make tests pass. Node requirement is
>=20, satisfied by the inherited Node 24.21.0.

**Validation:** actual local HTTP checked XML `&amp;` decoding, numeric size,
Date conversion, missing `Contents`, buffer bytes, public-read ACL, SigV4
authorization, default CRC32 headers on PUT and bulk DELETE, ETag, delete-marker
and version headers, AccessDenied/403 rejection, retry of an embedded
InternalError in HTTP 200, and two-page bulk deletion with the last-key marker
fallback. Isolated integration also exercises uploads/deletes through the real
SDK. **No binding change required.**

Existing caveat, not introduced here: the PureScript delete response declares
fields that S3 can omit for an unversioned bucket. This upgrade neither makes
those fields required upstream nor changes the binding's existing behavior.

### @octokit/plugin-retry: 8.1.0 → 8.1.1 (foreign)

Sources: [comparison](https://github.com/octokit/plugin-retry.js/compare/v8.1.0...v8.1.1),
[release](https://github.com/octokit/plugin-retry.js/releases/tag/v8.1.1),
[retry implementation](https://github.com/octokit/plugin-retry.js/blob/v8.1.1/src/index.ts).
Published runtime source is unchanged except the version constant. Changes
update dependencies (`@octokit/types` 16 → 17 and request-error's minimum),
tests and development tooling. Peer requirement remains Octokit core >=7;
Node >=20 remains sufficient. Default retry status exclusions, retry count and
quadratic backoff are unchanged.

`foreign/src/Foreign/Octokit.js` composes the named `retry` plugin with REST
and throttling and passes failures to `onError`. **Validation:** an actual 500
followed by 200 produced two requests and a success callback; 401 was not
retried. Error callback status and response handling remain correct.
**No binding change required.**

### @octokit/plugin-throttling: 11.0.3 → 11.0.5 (foreign)

Sources: [comparison](https://github.com/octokit/plugin-throttling.js/compare/v11.0.3...v11.0.5),
[release](https://github.com/octokit/plugin-throttling.js/releases/tag/v11.0.5),
[implementation](https://github.com/octokit/plugin-throttling.js/blob/v11.0.5/src/index.ts).
The material runtime change defers Bottleneck instantiation until the first
request for Cloudflare Workers compatibility. Groups and the retry limiter are
initialized lazily; existing supplied limiters are preserved. The later patch
updates Octokit type dependencies to v17. No callback argument or return
contract change; Node >=20 and Octokit core >=7 are unchanged.

Our `onRateLimit` reads `options.request.retryCount` and returns true while it
is <=2; `onSecondaryRateLimit` does not opt into retries. **Validation:** the
first real request succeeded after lazy initialization, a persistent primary
403 exhausted the existing policy after four total attempts, and a secondary
403 was returned after one attempt. Pagination still produced one combined
array. **No binding change required.**

### @octokit/rest: 22.0.1 → 22.0.1 (foreign; already latest)

Sources: [release](https://github.com/octokit/rest.js/releases/tag/v22.0.1),
[package definition](https://github.com/octokit/rest.js/blob/v22.0.1/package.json).
There is no direct-version source delta. The existing named `Octokit` export,
`.plugin()`, constructor auth/baseUrl options, `.request(route, args)` response
object and `.paginate(route, args)` array remain correct. Node >=20 is met.
The lock refresh updates compatible transitive packages; see the separate
Octokit transitive review below. **Validation:** the combined real HTTP probes
and the registry's GitHub integration paths, not only mocked unit tests.
**No binding change required.**

### better-sqlite3: 13.0.3 → 13.0.3 (app and foreign; already latest)

Sources: [release](https://github.com/WiseLibs/better-sqlite3/releases/tag/v13.0.3),
[API](https://github.com/WiseLibs/better-sqlite3/blob/v13.0.3/docs/api.md).
PR #802 already supplies this version. There is no new major-version
transition or source delta here. `app/src/App/SQLite.js` uses the default
Database export, synchronous constructor with `fileMustExist`/`verbose`,
`pragma`, prepared `.get/.all/.run` statements and synchronous `.transaction`
callbacks; these remain the v13 API. Statement result and error behavior are
unchanged. Node >=22 is required, and the addon must match Node's ABI.

**Validation:** ordinary Nix npm packaging built successfully on Node
24.21.0; real SQLite job/deduplication tests, the server API integration and
VM smoke passed. No addon override was needed. **No binding change required.**
Darwin execution of this stack is still needed, despite PR #802's earlier
Apple Silicon validation.

### fast-glob: 3.3.3 → 3.3.3 (foreign; already latest)

Sources: [release](https://github.com/mrmlnc/fast-glob/releases/tag/3.3.3),
[API/options](https://github.com/mrmlnc/fast-glob/tree/3.3.3#api).
There is no direct source delta. `foreign/src/Foreign/FastGlob.js` imports the
default function and passes pattern arrays with cwd, ignore, file/directory,
case sensitivity, dotfile and uniqueness options. It still returns a Promise
of paths and rejects on filesystem errors. Node >=8.6 is met. `fastq` advances
1.20.1 → 1.20.3 and `picomatch` 2.3.1 → 2.3.2 within declared dependency ranges.

**Validation:** real temporary files checked brace expansion, dotfiles and
ignore filtering; existing foreign glob tests and integration package-file
selection passed. **No binding change required.** The unresolved braces
security advisory below is not a successful security remediation claim.

### fs-extra: 11.3.4 → 11.4.1 (foreign)

Sources: [comparison](https://github.com/jprichardson/node-fs-extra/compare/11.3.4...11.4.1),
[complete interval changelog](https://github.com/jprichardson/node-fs-extra/blob/11.4.1/CHANGELOG.md).
11.3.5 fixes Windows identical-link detection, timestamp error handling and
synchronous descriptor cleanup; 11.3.6 prevents recursive copy/move through
symlinked destination ancestors; 11.4.0 reports EEXIST for a broken destination
symlink; 11.4.1 propagates emptyDir read errors and allows Unicode-normalization
renames on APFS. There is no change to defaults or exports of our APIs.

`foreign/src/Foreign/FSExtra.js` uses named `ensureDir`, `remove`, and `copy`
from the package root, converting their Promises to Aff. `copy` passes the
Boolean `preserveTimestamps`; mkdir/remove return values are ignored. The
timestamp and symlink-copy fixes affect our possible inputs, not our calling
convention. We do not call move, emptyDir or ensureSymlink. Node >=14.14 is met.

**Validation:** real directory creation/removal, copied bytes and preserved
mtime, ENOENT Promise rejection, and rejection of copying a directory through
a destination symlink back into itself. Integration package copies also passed.
**No binding change required.** APFS-specific rename behavior was not executed
on Linux and is not used by these bindings.

### fuse.js: 7.1.0 → 7.5.0 (foreign)

Sources: [comparison](https://github.com/krisk/Fuse/compare/v7.1.0...v7.5.0),
[changelog](https://github.com/krisk/Fuse/blob/v7.5.0/CHANGELOG.md).
7.2 adds runtime plugins, search caching/heap optimizations and match/index
fixes. 7.3 adds token search, static match and BigInt support, rewrites source
to TypeScript, removes UMD builds, changes removeAt's return, and makes empty
queries return all documents. 7.4 adds workers, token options and index/cache,
field-normalization, highlighting and worker-result fixes. 7.4.1/7.4.2 fix
declaration exports and move the build to tsdown. 7.5 fixes field word counts,
key-weight normalization, top-N tie selection and minMatchCharLength; upstream
explicitly warns that scores and ordering can change.

`foreign/src/Foreign/SPDX.js` uses the default ESM Fuse constructor on a fixed
array of current/deprecated license strings with default options and maps
`search(identifier)` results' `item` field. No worker, token search, custom
weights, limit, mutation, highlights or UMD path is used. Constructor/result
shapes remain correct. Node >=10 is met. We accept corrected fuzzy scoring,
but an empty license must not suddenly suggest every license.

**Binding adaptation:** return `[]` for the empty string. The existing public
`fuzzyMatchLicense` test now includes that input; before this guard it failed
with a list of suggested licenses, afterward it passed. A real-data probe
checked that **all 734 current/deprecated identifiers** remain their own top
exact result, and checked a misspelled BSD query. Existing Apache/BSD typo
tests remain unchanged. No scores or new private interaction tests were pinned.

### jsonrepair: 3.13.3 → 3.15.0 (foreign)

Sources: [comparison](https://github.com/josdejong/jsonrepair/compare/v3.13.3...v3.15.0),
[changelog](https://github.com/josdejong/jsonrepair/blob/v3.15.0/CHANGELOG.md).
3.14 repairs backslash-escaped newlines. 3.14.1 fixes invalid number repair,
unquoted strings beginning with a keyword, unescaped quotes before closing
characters/end-of-string, and nested trailing-comma repair. 3.15 adds
HTML-entity-encoded JSON repair and fixes infinite recursion for a string
followed by a backslash. Both regular and streaming implementations change.

`foreign/src/Foreign/JsonRepair.js` only uses the synchronous named `jsonrepair`
function, passing a string and returning repaired text through `onSuccess` or
the existing `onError` value after a throw. No streaming, option, export or
async contract change. New repair capabilities are intentional improvements
for legacy input; actual quoted strings must still preserve literal entities.
No native requirement is introduced.

**Validation:** entity-encoded JSON decodes `&amp;` correctly; ordinary quoted
`&amp;` stays literal; `[2., trueble]` repairs to numeric 2 and string
`trueble`; an outside-string backslash calls the failure branch rather than
recursing. Existing repair tests passed. **No binding change required.**

### semver: 7.7.4 → 7.8.5 (foreign)

Sources: [comparison](https://github.com/npm/node-semver/compare/v7.7.4...v7.8.5),
[changelog](https://github.com/npm/node-semver/blob/v7.8.5/CHANGELOG.md).
7.8 adds `truncate` and a CLI warning; 7.8.1 fixes prerelease subset logic and
strips build metadata before comparator trimming; 7.8.2 fixes incrementing
dotted prerelease identifiers; 7.8.3/7.8.5 correct caret/tilde lower bounds when
including prereleases; 7.8.4 rejects numeric components after wildcard
components. The remaining changes are documentation/development tooling.

`foreign/src/Foreign/SemVer.js` uses default CJS-to-ESM import and synchronous
`validRange(string, { loose: true, includePrerelease: false })`, returning a
normalized comparator string or null. New functions/CLI and includePrerelease
true behavior are unused. Build metadata stripping and invalid wildcard
ordering are relevant input corrections. Node >=10 is met.

**Validation:** exact normalized caret/loose-v results, build-metadata removal,
and `1.x.3` rejection; existing lenient-range public tests passed.
**No binding change required.**

### tar: 7.5.21 → 7.5.22 (foreign)

Sources: [comparison](https://github.com/isaacs/node-tar/compare/v7.5.21...v7.5.22),
[options source](https://github.com/isaacs/node-tar/blob/v7.5.22/src/options.ts).
The only functional-source delta broadens the TypeScript `transform` callback
return type to permit falsey values already accepted at runtime. It does not
change listing, extraction behavior, defaults or exports.

`foreign/src/Foreign/Tar.js` uses named `list` with file, sync, filter and
onReadEntry options and consumes `entry.path`. It supplies no transform.
Execution stays synchronous with filesystem errors thrown. Node >=18 is met.
**Validation:** created a real gzip tarball, checked only the top-level
directory is returned and checked missing-file ENOENT; existing CLI archive
tests/integration passed. **No binding change required.**

### tmp: 0.2.5 → 0.2.7 (foreign)

Sources: [comparison](https://github.com/raszi/node-tmp/compare/v0.2.5...v0.2.7),
[implementation](https://github.com/raszi/node-tmp/blob/v0.2.7/lib/tmp.js).
0.2.6 uses relative-path containment checks instead of a raw prefix check.
0.2.7 rejects non-string prefix/postfix/template values and traversal substrings
that previously could evade validation. Generation and cleanup contracts remain
unchanged.

`foreign/src/Foreign/Tmp.js` imports named `setGracefulCleanup` and `dirSync`,
enables cleanup once and always passes the literal string template `XXXXXX`.
It consumes the returned object's `name`. These inputs pass the stricter
validation; the API remains synchronous. Node >=14.14 is met.
**Validation:** a real directory was created; a child process using the binding
created an empty directory which was gone after graceful exit. Existing
temporary-directory users in tests passed. **No binding change required.**

### yaml: 2.8.2 → 2.9.1 (app and foreign)

Sources: [comparison](https://github.com/eemeli/yaml/compare/v2.8.2...v2.9.1),
[2.8.3](https://github.com/eemeli/yaml/releases/tag/v2.8.3),
[2.8.4](https://github.com/eemeli/yaml/releases/tag/v2.8.4),
[2.9.0](https://github.com/eemeli/yaml/releases/tag/v2.9.0),
[2.9.1](https://github.com/eemeli/yaml/releases/tag/v2.9.1).
2.8.3 adds a flow-stringification trailing-comma option and catches composition
stack overflow; 2.8.4 fixes alias-disable handling, invalid Unicode escapes and
decimal minFractionDigits. 2.9 avoids large-array push/apply and recursive lexer
stack exhaustion and **removes the documentation promise that parseDocument
never throws**. 2.9.1 limits recursive merge aliases and simplifies quoted-string
line unfolding. Stringification options and YAML 1.1 merge handling are unused
by our default parser. Node >=14.6 is met.

`foreign/src/Foreign/Yaml.js` imports the default package object, calls
`parseDocument(string)`, checks `doc.errors`, and converts with `doc.toJSON()`.
Those methods and JSON shapes remain correct, but both parse and conversion
errors must stay inside `yamlParser :: String -> Either String JSON`.

**Binding adaptation:** catch parse/conversion exceptions and return their
message through the failure callback, preserving existing doc.errors handling.
The success callback remains outside the catch. A real 11-by-11 alias expansion
previously threw `ReferenceError: Excessive alias count indicates a resource
exhaustion attack` past the public `parseYaml` interface; the new regression
test failed before the fix and passes after it. This uncaught conversion error
was also possible in the old dependency; the review found it while checking
the weakened upstream throwing guarantee, not as a new v2.9 invention.
Ordinary JSON values, malformed syntax and invalid Unicode were also exercised.

### spdx-expression-parse: 4.0.0 → 5.0.0 (lib; major)

Sources: [complete comparison](https://github.com/jslicense/spdx-expression-parse.js/compare/v4.0.0...v5.0.0),
[parser source](https://github.com/jslicense/spdx-expression-parse.js/blob/v5.0.0/parse.js).
There is no separate migration guide. Published package comparison shows the
entire parser delta: LicenseRef nodes can now carry a `WITH` exception, and
truncated DocumentRef input gets an explicit `Unexpected end of input` Error
instead of dereferencing a null token. The package version is the other delta;
scanner, entrypoint, dependency declarations and ordinary conjunction ASTs are
unchanged. It remains synchronous CJS with a default ESM interop import.

`lib/src/License.js` reads leaf `license`, `plus`, optional `exception`, and
branch `left`, `right`, `conjunction`; it already understands the added optional
field and catches thrown parse errors. PureScript `Registry.License` still
requires recognized SPDX identifiers, so accepting LicenseRef WITH upstream
does not admit arbitrary custom licenses into registry manifests.

**Validation:** actual v5 AST decoding preserved the LicenseRef exception;
truncated DocumentRef reached the error callback; public `License.parse`
continued to reject custom refs. Nested AND/OR round-tripping and deprecated
GPL-plus canonicalization passed, as did existing library tests. **No binding
change required despite the major.**

### ssh2: 1.17.0 → 1.17.0 (lib; already latest)

Sources: [tagged source](https://github.com/mscdex/ssh2/tree/v1.17.0),
[key-parser API](https://github.com/mscdex/ssh2/tree/v1.17.0#utilities).
There is no direct-version delta. `lib/src/SSH.js` uses the default CJS interop
import and `utils.parseKey(buffer, passphrase)`; parsed-key sign/verify,
type/comment, isPrivateKey, equals and getPublicSSH remain synchronous.
parseKey's possible Error/array results have not changed. Node >=10.16 is met.

The lock refresh updates `nan` 2.25.0 → 2.29.0 for native-build compatibility:
[comparison](https://github.com/nodejs/nan/compare/v2.25.0...v2.29.0),
[changelog](https://github.com/nodejs/nan/blob/v2.29.0/CHANGELOG.md).
The interval adds V8 embedder/external-pointer type tags, fixes the associated
weak-pointer header/copy-paste issues and adds Node 26/V8 15.3 support; it does
not change the first-party JS API. The optional cpu-features version remains
0.0.10 and is omitted by Nix's existing `--omit=optional`. **Validation:**
existing real-key parsing/signing/verifying/ownership library tests and signed
integration operations passed under Node 24. **No binding change required.**

## Material transitive changes

### AWS runtime consolidation

`@aws-sdk/core` 3.973.20 → 3.978.2, `@smithy/core` 3.23.11 → 3.35.2,
`@smithy/node-http-handler` 4.4.16 → 4.12.1,
`@smithy/fetch-http-handler` 5.3.15 → 5.8.0,
`@smithy/signature-v4` 5.3.12 → 5.7.4,
`@aws-sdk/middleware-sdk-s3` 3.972.20 → 3.972.78 and
`@aws-sdk/xml-builder` 3.972.11 → 3.972.41 accompany the generated-client change.
`@aws-sdk/checksums` 3.1001.2 is new. Many small AWS/Smithy packages and the
fast-xml-parser/fast-xml-builder/strnum chain disappear, being replaced by the
SDK's consolidated internal modules rather than our own replacements.
Credential-provider packages are refreshed, but our explicit static credential
object does not use their discovery chain.

The S3 command/runtime source comparisons linked above show the moved checksum,
endpoint, retry and XML import paths and preserved command middleware settings.
The real-wire S3 checks specifically exercised those risky paths, including
signed request headers, XML scalar conversion, default checksums and an HTTP
200 error retry. No private AWS/Smithy submodule is imported by first-party FFI.

### Octokit transport and types

`@octokit/request` 10.0.8 → 10.0.16:
[comparison](https://github.com/octokit/request.js/compare/v10.0.8...v10.0.16).
Published fetch-wrapper changes replace fast-content-type-parse with
`content-type` 3.1.1, keep octet-stream responses binary even with utf-8 charset,
and safely extract errors from null/primitive JSON bodies. `.request` still
returns response headers/status/data and throws a RequestError for HTTP errors.
Real HTTP probes verified ordinary JSON, a null-bodied 401 error and binary
bytes `[0, 255, 128]` with a misleading charset. Our declared GitHub routes
consume JSON, including base64 repository content, not raw binary downloads.

`@octokit/core` 7.0.6 → 7.0.8 and `@octokit/endpoint` 11.0.3 → 11.0.5 have no
published runtime-source difference except version constants;
`@octokit/request-error` 7.1.0 → 7.1.2 has no published runtime-source difference.
Sources: [core](https://github.com/octokit/core.js/compare/v7.0.6...v7.0.8),
[endpoint](https://github.com/octokit/endpoint.js/compare/v11.0.3...v11.0.5),
[request-error](https://github.com/octokit/request-error.js/compare/v7.1.0...v7.1.2).

`json-with-bigint` 3.5.7 → 3.5.12 remains the transport's JSON codec. Sources:
[old published code](https://registry.npmjs.org/json-with-bigint/-/json-with-bigint-3.5.7.tgz),
[new published code](https://registry.npmjs.org/json-with-bigint/-/json-with-bigint-3.5.12.tgz).
The distribution adds iterative parse/reviver and stringify fallbacks for deep
objects, tightens string-marker regular expressions, and generates its CJS
distribution from ESM. Native context.source parsing and rawJSON serialization
remain fast paths. The real Octokit HTTP round-trip checked safe numeric
9007199254740991, positive/negative BigInt 9007199254740993, literal `123n` and
fractional values, then parsed a 10,000-deep array without losing its BigInt.
No user reviver/replacer or private codec API is exposed by our bindings.

The root `@octokit/types` advances 16 → 18 and openapi-types 27 → 29.0.1;
retry/throttling resolve types 17/openapi-types 28 separately, while REST's
endpoint/pagination plugins retain their v16 declarations. These are
declaration-only packages, not new JS implementations or wire coercions.
No TypeScript declaration is used by the PureScript FFI. Real response,
error, retry and pagination checks establish compatibility independently of
those type versions.

### SPDX data and glob security

`spdx-license-ids` 3.0.23 → 3.0.24 is imported directly by our License/SPDX FFI
but supplied transitively by spdx-expression-parse. Published JSON comparison
adds 13 current identifiers and does not remove/reclassify old IDs or change
the deprecated list. Sources: [comparison](https://github.com/jslicense/spdx-license-ids/compare/v3.0.23...v3.0.24),
[current data](https://github.com/jslicense/spdx-license-ids/blob/v3.0.24/index.json).
All 734 current/deprecated exact fuzzy matches were checked; normal/canonical
license parser tests passed. This intentionally expands accepted current IDs.

`picomatch` 2.3.1 → 2.3.2 adds null-prototype lookup tables and extglob
anti-ReDoS handling, fixing
[method injection](https://github.com/advisories/GHSA-3v7f-55p6-f55p) and
[extglob ReDoS](https://github.com/advisories/GHSA-c2c7-rcm5-vvqj).
Source: [comparison](https://github.com/micromatch/picomatch/compare/2.3.1...2.3.2).
Real glob checks and package inclusion/exclusion integration passed. Highly
complex nested extglob behavior can be restricted by the security fix; we do
not weaken it with a custom option or override.

**Unresolved security limitation:** `npm audit` still reports three high-severity
package entries (`braces`, propagated to `micromatch` and `fast-glob`) for one
[deeply nested brace stack-exhaustion advisory](https://github.com/advisories/GHSA-vfj7-8cjw-p6xm).
braces 3.0.3 and fast-glob 3.3.3 are already latest and npm reports no fix.
Manifest include/exclude patterns can reach glob processing, so this cannot be
dismissed as development-only. Thomas explicitly excluded remediation from this
task; it is recorded here, not a publication blocker. This PR does not claim a
clean security audit, silently pin an older release, or invent an override.

## PureScript and tooling outcome

Per Thomas's direction, use the latest package set, **68.1.1 → 81.4.0** (published
2026-10-09), not a solver migration or per-package overrides. Source:
[package set](https://github.com/purescript/registry/blob/main/package-sets/81.4.0.json).
Both Git overrides are removed because the set now provides published registry
packages at newer versions. Effective resolved package changes are:

| Package | Old | New |
| --- | --- | --- |
| b64 | 0.0.8 | 0.0.9 |
| dodo-printer | Git v2.2.1 | registry 2.2.3 |
| encoding | 0.0.9 | 0.0.10 |
| http-methods | 6.0.0 | 6.1.0 |
| language-cst-parser | Git v0.13.0 | registry 0.14.1 |
| parsing | 10.3.0 | 11.0.0 |
| spec | 8.1.1 | 8.1.2 |

Other edits in the lock's package-set snapshot describe packages this workspace
does not resolve; they are not additional installed upgrades. Some published
releases are newer than the set (for example node-path 6 and web-dom 7); retaining
the curated latest set rather than overriding those is intentional and explicit.
All seven workspace packages compile without warnings/errors. No dashboard
source, styles, Halogen, or DOM dependency version changed in the final stack.

Tooling: PureScript 0.15.16 and Spago 1.0.4 already match npm latest and the
dashboard workflow pins. The dashboard bundle was built with both inherited
Nix esbuild 0.27.2 and npm latest esbuild **0.28.2**, matching the workflow's
unversioned install. The former remains part of the parent PR's Nix toolchain,
not an npm lock dependency upgraded here. Sources:
[esbuild comparison](https://github.com/evanw/esbuild/compare/v0.27.2...v0.28.2),
[0.28.0 notes](https://github.com/evanw/esbuild/releases/tag/v0.28.0),
[0.28.1 notes](https://github.com/evanw/esbuild/releases/tag/v0.28.1),
[0.28.2 notes](https://github.com/evanw/esbuild/releases/tag/v0.28.2).
The 0.28 transition adds text imports and integrity checking for fallback binary
downloads, updates Go, and fixes dev-server traversal, Deno integrity, JS/TS
tree shaking/printing/evaluation, CSS minification and output-overwrite handling.
Our CLI uses browser/Node bundling, distinct input/output paths and JSON import
attributes; it does not use Deno, wasm, text imports or CSS minification. Existing
CLI flags remain valid. No esbuild JS API binding or custom binary install exists.

## Validation and limitations

Executed on Chantilly, Linux x86_64, inherited Node 24.21.0/npm 11.19.0:

- `spago build`: all seven workspace packages, zero warnings/errors.
- `spago test -p registry-lib`: **129/129**.
- `spago test -p registry-foreign`: **18/18**, including the empty-license
  regression that failed before its binding fix.
- `spago test -p registry-app`: **153/153**, including the YAML regression
  that threw before its binding fix.
- Targeted actual-dependency probes: **11/11 groups**, with assertions on
  observable values and failures as described in each package section. Test
  HTTP servers bound ephemeral loopback ports and were shut down afterward.
- Dashboard browser bundle: passed with esbuild 0.27.2 and 0.28.2.
- PureScript formatting check: passed.
- `nix flake check -L`: Linux `spago-test`, `nix-format`, `purescript-format`,
  `verify-dhall`, `integration`, and `smoke`: **all six passed** on the final
  binding changes. Integration passed **29/29** tests, uses isolated mocks,
  and smoke uses a NixOS VM, not the shared interactive test environment.
- `npm outdated --json`: `{}`. `npm audit` is **not clean**: the three high
  package entries described above remain without an available upstream fix.

Darwin source compatibility is assessed, not executed: APIs remain portable,
Node requirements are met, no new platform-specific FFI or packaging override
was added, and both removed Spago Git dependencies now use registry tarballs.
The existing Darwin Spago cache-path handling remains intact. The unchanged
SQLite version still needs its native addon on the target Node ABI; fs-extra's
APFS-only fix was not exercised here. Thomas explicitly waived Darwin execution
for this task; it is not a publication blocker, and no Darwin test pass is
claimed. The dashboard workflow's Node 22 path was not separately executed
here. No push, PR publication, merge or deployment is part of these local
validation results.
