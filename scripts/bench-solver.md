# Dependency solver benchmarks

Run from the repository root, using the pinned development environment:

```sh
nix develop --command npm ci
nix develop --command spago build
nix develop --command spago test -p registry-lib
nix develop --command node scripts/bench-solver.mjs > before.json
```

The benchmark calls the real `Registry.Solver.solve` and `solveWithCompiler`
exports from compiled PureScript. It uses Node's built-in timing, assertion,
and profiling APIs; no benchmark dependency or public library API is added.
The generated-module interop is confined to this script.

Each case has three warmups, then seven samples of three solves. JSON on stdout
contains every sample, the median milliseconds per solve, complete resolutions
or formatted errors, runtime/CPU information, and the compiled solver SHA-256.
Progress goes to stderr. `SAMPLES`, `ITERATIONS`, and `CASE` (a label substring)
override the defaults. Use more iterations for very short cases. Timings are
not CI assertions: JIT, GC, CPU frequency, and other local processes affect them.

Index construction, input decoding, compiler-index construction, IO, and result
validation are outside the timed region. Solver-internal preparation, including
reachability and diagnostic initialization, is inside it. Each sample retains
its results until validation; keep the same iteration count in both runs.

## Workloads and correctness

Six deterministic synthetic cases cover a long dependency chain, a wide shared
dependency diamond, irrelevant index entries, a narrow interval in a package
with many versions, conflicting newest choices requiring backtracking, and an
unsatisfiable pair of roots. Successes must equal independently specified plans;
failure must be unsatisfiable. Every success is also checked for dependency
closure, range satisfaction, existing versions, and absence of extra packages.

An optional snapshot adds the latest manifests' dependency sets for `prelude`,
`effect`, `aff`, `web-storage`, `halogen`, `spec`, `tidy`, and
`language-cst-parser`, each with and without compiler 0.15.16:

```sh
nix develop --command node scripts/bench-solver.mjs snapshot.json > before.json
```

The input is a local JSON object with `manifests` (an array of registry manifest
objects), `metadata` (package name to registry metadata object), and `compilers`
(a nonempty array of version strings). Optional `commits` records snapshot
provenance. The script never fetches a registry or contacts production. Input
bytes are hashed so comparisons cannot silently switch snapshots. Use the same
saved file for both runs. These are dependency solves, not builds of the named
packages, and `prelude` deliberately provides an empty-dependency control.
Compiler results are checked against the metadata's min/max support bounds,
matching the existing compiler-index contract, not a discrete support set.

The library tests separately enumerate all assignments for 4,096 small cyclic
version graphs to detect invalid plans and false unsatisfiability. Targeted tests
check alphabetical/latest selection, backtracking, exact/flexible compilers,
and structured error provenance. Benchmarks complement these tests, not replace
them; real-snapshot satisfiability alone is not an independent correctness oracle.

## Before/after and CPU profiles

Preserve a baseline build before changing the solver, then rebuild normally:

```sh
mkdir -p scratch
cp -a output scratch/solver-before
# After editing and rebuilding:
SOLVER_OUTPUT=scratch/solver-before nix develop --command \
  node scripts/bench-solver.mjs snapshot.json > before.json
BASELINE=before.json nix develop --command \
  node scripts/bench-solver.mjs snapshot.json > after.json
```

Keep the preserved output inside this checkout (for example in ignored `scratch/`)
so its FFI can resolve this checkout's `node_modules`. `BASELINE` requires identical
workload labels, snapshot provenance, complete plans, and formatted diagnostics.
Repeat in before/after/after/before order without concurrent builds or test runs.
Report distributions as well as medians; don't compare one cold run to warm runs.

```sh
CASE=halogen SAMPLES=1 ITERATIONS=100 PROFILE=halogen.cpuprofile \
  nix develop --command node scripts/bench-solver.mjs snapshot.json > profile.json
```

Open the profile in a V8 CPU-profile viewer. Profiling starts after fixture setup
and includes warmups, solving, and validation. Do not use profiled timings as the
headline measurements. A snapshot result is not a production latency, package
compilation, server request, or matrix-scheduling benchmark.
