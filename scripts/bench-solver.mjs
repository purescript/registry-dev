// Run after `spago build`: node scripts/bench-solver.mjs [snapshot.json]
// PureScript's generated modules are intentionally used only at this benchmark
// boundary. No network, database, scheduling, or registry IO occurs in a sample.
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { Session } from "node:inspector/promises";
import { cpus } from "node:os";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

const output = resolve(process.env.SOLVER_OUTPUT || "output");
const load = (name) => import(pathToFileURL(`${output}/${name}/index.js`));
const [Solver, Map, Foldable, Unfoldable, Tuple, Either, Name, Version, Range, CJ, Manifest, Metadata, NEL] =
  await Promise.all([
    "Registry.Solver", "Data.Map", "Data.Foldable", "Data.Unfoldable", "Data.Tuple",
    "Data.Either", "Registry.PackageName", "Registry.Version", "Registry.Range",
    "Data.Codec.JSON", "Registry.Manifest", "Registry.Metadata", "Data.List.NonEmpty",
  ].map(load));
const right = (value) => {
  assert(value instanceof Either.Right, "Invalid benchmark input");
  return value.value0;
};
const version = (s) => right(Version.parse(s));
const name = (s) => right(Name.parse(s));
const range = (s) => right(Range.parse(s));
const pairs = Map.toUnfoldable(Unfoldable.unfoldableArray);
const from = (ord, entries) => Map.fromFoldable(ord)(Foldable.foldableArray)(
  entries.map(([k, v]) => new Tuple.Tuple(k, v)),
);
const packages = (entries) => from(Name.ordPackageName, entries);
const versions = (entries) => from(Version.ordVersion, entries);
const requirements = (deps) => packages(Object.entries(deps).map(([p, r]) => [name(p), range(r)]));
const indexFrom = (index) => packages(Object.entries(index).map(([p, vs]) => [
  name(p), versions(Object.entries(vs).map(([v, deps]) => [version(v), requirements(deps)])),
]));
const printPlan = (plan) => Object.fromEntries(pairs(plan).map((t) => [Name.print(t.value0), Version.print(t.value1)]));
const cmp = (a, b) => {
  const av = a.split(".").map(Number), bv = b.split(".").map(Number);
  return av[0] - bv[0] || av[1] - bv[1] || av[2] - bv[2];
};
// Independent of the solver's range propagation and sourced intersections.
function includes(r, v) {
  const [, lo, hi] = /^>=(\d+\.\d+\.\d+) <(\d+\.\d+\.\d+)$/.exec(r);
  return cmp(lo, v) <= 0 && cmp(v, hi) < 0;
}
function validate(index, goals, plan) {
  const seen = new Set();
  function visit(deps) {
    for (const [p, r] of Object.entries(deps)) {
      assert(plan[p] && includes(r, plan[p]), `${p}@${plan[p]} violates ${r}`);
      assert(index[p]?.[plan[p]], `Unknown ${p}@${plan[p]}`);
      if (!seen.has(p)) {
        seen.add(p);
        visit(index[p][plan[p]]);
      }
    }
  }
  visit(goals);
  assert.deepEqual(Object.keys(plan).sort(), [...seen].sort(), "Extraneous packages");
}
const cases = [];
const indexes = new WeakMap();
function add(label, raw, goals, expected, compiler = null) {
  if (!indexes.has(raw)) indexes.set(raw, indexFrom(raw));
  const index = indexes.get(raw), required = requirements(goals);
  const compilerRange = Range.exact(version("0.15.16"));
  const run = compiler
    ? () => Solver.solveWithCompiler(compilerRange)(compiler.index)(required)
    : () => Solver.solve(index)(required);
  const check = (result) => {
    if (expected === null) {
      assert(result instanceof Either.Left, `${label}: expected unsatisfiable`);
      return { errors: NEL.toUnfoldable(Unfoldable.unfoldableArray)(result.value0).map(Solver.printSolverError) };
    }
    assert(result instanceof Either.Right, `${label}: unexpectedly unsatisfiable`);
    let resolved = result.value0;
    if (compiler) {
      assert.equal(Version.print(resolved.value0), "0.15.16");
      resolved = resolved.value1;
    }
    const plan = printPlan(resolved);
    validate(raw, goals, plan);
    if (compiler) {
      for (const [p, v] of Object.entries(plan)) {
        const supported = compiler.metadata[p].published[v]?.compilers;
        if (supported?.length) {
          const sorted = [...supported].sort(cmp);
          assert(cmp(sorted[0], "0.15.16") <= 0 && cmp("0.15.16", sorted.at(-1)) <= 0,
            `${p}@${v}: compiler outside metadata bounds`);
        }
      }
    }
    if (expected) assert.deepEqual(plan, expected);
    return plan;
  };
  cases.push({ label, run, check, packages: Object.keys(raw).length,
    versions: Object.values(raw).reduce((n, vs) => n + Object.keys(vs).length, 0) });
}
const v = (i) => `${i}.0.0`;
const r = (lo, hi) => `>=${v(lo)} <${v(hi)}`;
const chain = {};
for (let i = 0; i < 60; i++) {
  chain[`chain-${i}`] = Object.fromEntries([1, 2, 3].map((j) => [v(j), i === 59 ? {} : { [`chain-${i + 1}`]: r(1, 4) }]));
}
add("chain-60", chain, { "chain-0": r(1, 4) }, Object.fromEntries(Object.keys(chain).map((p) => [p, v(3)])));
const diamond = { shared: { [v(1)]: {}, [v(2)]: {} }, root: { [v(1)]: {} } };
for (let i = 0; i < 40; i++) {
  const p = `branch-${i}`;
  diamond.root[v(1)][p] = r(1, 11);
  diamond[p] = Object.fromEntries(Array.from({ length: 10 }, (_, j) => [v(j + 1), { shared: r(1, 3) }]));
}
add("diamond-40x10", diamond, { root: r(1, 2) }, { root: v(1), shared: v(2), ...Object.fromEntries(Array.from({ length: 40 }, (_, i) => [`branch-${i}`, v(10)])) });
const unrelated = { target: { [v(1)]: {} } };
for (let i = 0; i < 2000; i++) unrelated[`unused-${i}`] = { [v(1)]: { target: r(1, 2) } };
add("unreachable-2000", unrelated, { target: r(1, 2) }, { target: v(1) });
const many = { releases: Object.fromEntries(Array.from({ length: 1000 }, (_, i) => [v(i), {}])) };
add("narrow-1000", many, { releases: r(498, 500) }, { releases: v(499) });
// Each branch alone is viable; their shared dependency forces backtracking.
const conflict = {
  a: { [v(1)]: { z: r(1, 2) }, [v(2)]: { z: r(2, 3) } },
  b: { [v(1)]: { z: r(2, 3) }, [v(2)]: { z: r(1, 2) } },
  z: { [v(1)]: {}, [v(2)]: {} },
};
add("selection-backtrack", conflict, { a: r(1, 3), b: r(1, 3) }, { a: v(2), b: v(1), z: v(2) });
add("incompatible-roots", conflict, { a: r(2, 3), b: r(2, 3) }, null);

let snapshotInfo = null;
if (process.argv[2]) {
  const bytes = readFileSync(process.argv[2]);
  const snapshot = JSON.parse(bytes);
  const raw = {}, decoded = {};
  for (const m of snapshot.manifests) {
    (raw[m.name] ??= {})[m.version] = m.dependencies;
    (decoded[m.name] ??= {})[m.version] = right(CJ.decode(Manifest.codec)(m));
  }
  const manifests = packages(Object.entries(decoded).map(([p, vs]) => [name(p), versions(Object.entries(vs).map(([s, m]) => [version(s), m]))]));
  const metadata = packages(Object.entries(snapshot.metadata).map(([p, m]) => [name(p), right(CJ.decode(Metadata.codec)(m))]));
  const ci = Solver.buildCompilerIndex(snapshot.compilers.map(version))(manifests)(metadata);
  snapshotInfo = { sha256: createHash("sha256").update(bytes).digest("hex"), commits: snapshot.commits, manifests: snapshot.manifests.length };
  for (const p of ["prelude", "effect", "aff", "web-storage", "halogen", "spec", "tidy", "language-cst-parser"]) {
    assert(raw[p], `Snapshot missing ${p}`);
    const latest = Object.keys(raw[p]).sort(cmp).at(-1);
    const deps = raw[p][latest];
    add(`${p}@${latest}/solve`, raw, deps);
    add(`${p}@${latest}/compiler`, raw, deps, undefined, { index: ci, metadata: snapshot.metadata });
  }
}
const samples = Number(process.env.SAMPLES || 7);
const iterations = Number(process.env.ITERATIONS || 3);
assert(Number.isInteger(samples) && samples > 0 && Number.isInteger(iterations) && iterations > 0);
const profiler = process.env.PROFILE ? new Session() : null;
if (profiler) {
  profiler.connect();
  await profiler.post("Profiler.enable");
  await profiler.post("Profiler.start");
}
const results = [];
for (const c of cases) {
  if (process.env.CASE && !c.label.includes(process.env.CASE)) continue;
  let canonical;
  for (let i = 0; i < 3; i++) canonical = c.check(c.run());
  const times = [];
  for (let s = 0; s < samples; s++) {
    const start = performance.now();
    const answers = Array.from({ length: iterations }, c.run);
    times.push((performance.now() - start) / iterations);
    for (const answer of answers) assert.deepEqual(c.check(answer), canonical);
  }
  const sorted = [...times].sort((a, b) => a - b);
  results.push({ label: c.label, packages: c.packages, versions: c.versions, medianMs: sorted[Math.floor(samples / 2)], samplesMs: times, result: canonical });
  console.error(`${c.label}: ${results.at(-1).medianMs.toFixed(3)} ms`);
}
assert(results.length, "CASE matched no workloads");
if (profiler) {
  const { profile } = await profiler.post("Profiler.stop");
  writeFileSync(process.env.PROFILE, JSON.stringify(profile));
  profiler.disconnect();
}
if (process.env.BASELINE) {
  const before = JSON.parse(readFileSync(process.env.BASELINE));
  assert.equal(snapshotInfo?.sha256, before.snapshot?.sha256, "Different snapshots");
  assert.deepEqual(results.map((r) => r.label), before.results.map((r) => r.label), "Different workloads");
  for (const [i, r] of results.entries()) {
    assert.deepEqual(r.result, before.results[i].result, `${r.label}: changed resolution or diagnostics`);
  }
}
const solverSha256 = createHash("sha256").update(readFileSync(`${output}/Registry.Solver/index.js`)).digest("hex");
console.log(JSON.stringify({ node: process.version, cpu: cpus()[0].model, output, solverSha256, samples, iterations, snapshot: snapshotInfo, results }, null, 2));
