-- | Local dependency-solver benchmarks; run with --help for options.
module Registry.Scripts.BenchSolver where

import Registry.App.Prelude

import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Node.Process as Process
import Performance.Minibench as Bench
import Registry.Internal.Codec as Codec
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver as Solver
import Registry.Test.Utils as Utils
import Registry.Version as Version

type Plan = Map PackageName Version
type Solution = { compiler :: Maybe Version, resolutions :: Plan }
type Outcome = Either Solver.SolverErrors Solution

data Expected = Unsolvable | Solvable (Maybe Plan)

type Workload =
  { label :: String
  , index :: Solver.DependencyIndex
  , goals :: Map PackageName Range
  , expected :: Expected
  , metadata :: Maybe (Map PackageName Metadata)
  , run :: Unit -> Outcome
  }

type Snapshot =
  { manifests :: Array Manifest
  , metadata :: Map PackageName Metadata
  , compilers :: NonEmptyArray Version
  }

snapshotCodec :: CJ.Codec Snapshot
snapshotCodec = CJ.Record.object
  { manifests: CJ.array Manifest.codec
  , metadata: Codec.packageMap Metadata.codec
  , compilers: CJ.Common.nonEmptyArray Version.codec
  }

type Report =
  { node :: String
  , snapshot :: Maybe Sha256
  , samples :: Int
  , results :: Array { label :: String, timing :: Bench.BenchResult, result :: Either (Array String) Solution }
  }

reportCodec :: CJ.Codec Report
reportCodec = CJ.Record.object
  { node: CJ.string
  , snapshot: CJ.Record.optional Sha256.codec
  , samples: CJ.int
  , results: CJ.array $ CJ.Record.object
      { label: CJ.string
      , timing: CJ.Record.object { mean: CJ.number, stdDev: CJ.number, min: CJ.number, max: CJ.number }
      , result: CJ.Common.either (CJ.array CJ.string) $ CJ.Record.object
          { compiler: CJ.Record.optional Version.codec
          , resolutions: Codec.packageMap Version.codec
          }
      }
  }

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  options <- require $ lmap Arg.printArgError $ Arg.parseArgs "bench-solver" "Benchmark local solver workloads (Minibench timings in nanoseconds)."
    ( Arg.fromRecord
        { snapshot: Arg.optional $ Arg.argument [ "--snapshot" ] "Local registry snapshot JSON"
        , baseline: Arg.optional $ Arg.argument [ "--baseline" ] "Compare complete results with this report"
        , samples: Arg.default 40 $ Arg.int $ Arg.argument [ "--samples" ] "Timed solves per case (at least two)"
        , filter: Arg.default "" $ Arg.argument [ "--case" ] "Workload label substring"
        }
    )
    args
  require $ ensure (options.samples >= 2) "--samples must be at least two"
  snapshot <- traverse (require <=< readJsonFile snapshotCodec) options.snapshot
  hash <- traverse Sha256.hashFile options.snapshot
  real <- require $ maybe (Right []) snapshotWorkloads snapshot

  let cases = Array.filter (String.contains (String.Pattern options.filter) <<< _.label) (syntheticWorkloads <> real)

  require $ ensure (not (Array.null cases)) "--case matched no workloads"
  results <- for cases \work -> do
    -- Validate and warm up outside Minibench's timed region.
    for_ (Array.range 1 3) \_ -> require $ validate work (work.run unit)

    let result = work.run unit

    timing <- liftEffect $ Bench.benchWith' options.samples work.run
    require $ ensure (work.run unit == result) (work.label <> ": result changed after timing")
    Console.error $ work.label <> ": mean " <> Bench.withUnits timing.mean
    pure { label: work.label, timing, result: lmap (map Solver.printSolverError <<< Array.fromFoldable) result }
  for_ options.baseline \path -> do
    before <- require =<< readJsonFile reportCodec path
    require $ ensure (before.snapshot == hash && before.samples == options.samples && before.node == Process.version) "Different snapshot, sample count, or Node version"
    require $ ensure (map (\r -> { label: r.label, result: r.result }) before.results == map (\r -> { label: r.label, result: r.result }) results) "Changed workloads, resolutions, or diagnostics"
  Console.log $ printJson reportCodec { node: Process.version, snapshot: hash, samples: options.samples, results }

require :: forall a. Either String a -> Aff a
require = either (Aff.throwError <<< Aff.error) pure

ensure :: Boolean -> String -> Either String Unit
ensure condition message = if condition then Right unit else Left message

-- | Inspect only selected versions, independently of solver propagation/search.
validate :: Workload -> Outcome -> Either String Unit
validate work result = case result, work.expected of
  Left _, Unsolvable -> Right unit
  Left _, _ -> Left $ work.label <> ": unexpectedly unsatisfiable"
  Right _, Unsolvable -> Left $ work.label <> ": expected unsatisfiable"
  Right solved, Solvable expected -> do
    for_ expected \plan -> ensure (plan == solved.resolutions) (work.label <> ": incorrect selection")
    reached <- visit Set.empty (Map.toUnfoldable work.goals)
    ensure (reached == Map.keys solved.resolutions) "Extraneous packages in resolution"
    for_ work.metadata \metadata -> do
      ensure (solved.compiler == Just compiler) "Wrong compiler selection"
      forWithIndex_ solved.resolutions \name selected -> do
        Metadata meta <- note "Missing compiler metadata" $ Map.lookup name metadata
        for_ (Map.lookup selected meta.published) \entry ->
          ensure (Foldable.minimum entry.compilers <= Just compiler && Just compiler <= Foldable.maximum entry.compilers) "Compiler outside metadata bounds"
    where
    visit seen pending = case Array.uncons pending of
      Nothing -> Right seen
      Just { head: Tuple name bounds, tail } -> do
        selected <- note "Missing dependency" $ Map.lookup name solved.resolutions
        ensure (Range.includes bounds selected) "Dependency outside required range"
        selectedDeps <- note "Unknown package version" $ Map.lookup name work.index >>= Map.lookup selected
        if Set.member name seen then visit seen tail
        else visit (Set.insert name seen) ((Map.toUnfoldable selectedDeps :: Array _) <> tail)

workload :: String -> Solver.DependencyIndex -> Map PackageName Range -> Expected -> Workload
workload label index goals expected =
  { label
  , index
  , goals
  , expected
  , metadata: Nothing
  , run: \_ -> map (\resolutions -> { compiler: Nothing, resolutions }) $ Solver.solve index goals
  }

compiler :: Version
compiler = Utils.unsafeVersion "0.15.16"

snapshotWorkloads :: Snapshot -> Either String (Array Workload)
snapshotWorkloads snapshot = do
  let
    index = Array.foldl (\acc (Manifest m) -> Map.insertWith Map.union m.name (Map.singleton m.version m.dependencies) acc) Map.empty snapshot.manifests
    seed = Solver.buildCompilerIndex snapshot.compilers ManifestIndex.empty Map.empty

  withMetadata <- for snapshot.manifests \manifest@(Manifest m) ->
    Tuple manifest <$> note ("Missing metadata for " <> PackageName.print m.name) (Map.lookup m.name snapshot.metadata)

  let compilerIndex = Array.foldl (\acc (Tuple manifest meta) -> Solver.updateCompilerIndex acc manifest meta) seed withMetadata

  Array.concat <$> for [ "prelude", "effect", "aff", "web-storage", "halogen", "spec", "tidy", "language-cst-parser" ] \name -> do
    latest <- note ("Snapshot missing " <> name) $ Map.lookup (Utils.unsafePackageName name) index >>= Map.findMax

    let
      label = name <> "@" <> Version.print latest.key
      plain = workload (label <> "/solve") index latest.value (Solvable Nothing)

    pure
      [ plain
      , plain
          { label = label <> "/compiler"
          , metadata = Just snapshot.metadata
          , run = \_ -> Solver.solveWithCompiler (Range.exact compiler) compilerIndex latest.value
              # map (\(Tuple selected resolutions) -> { compiler: Just selected, resolutions })
          }
      ]

syntheticWorkloads :: Array Workload
syntheticWorkloads = do
  let
    chain = Map.fromFoldable $ map
      (\i -> Utils.unsafePackageName ("chain-" <> show i) /\ releases 1 3 (\_ -> if i == 59 then Map.empty else deps [ ("chain-" <> show (i + 1)) /\ range 1 4 ]))
      (Array.range 0 59)

    diamondPlan = map (\i -> Utils.unsafePackageName ("branch-" <> show i) /\ version 10) (Array.range 0 39)
      # Array.cons (Utils.unsafePackageName "root" /\ version 1)
      # Array.cons (Utils.unsafePackageName "shared" /\ version 2)
      # Map.fromFoldable

    diamond = map (\i -> Utils.unsafePackageName ("branch-" <> show i) /\ releases 1 10 (\_ -> deps [ "shared" /\ range 1 3 ])) (Array.range 0 39)
      # Array.cons (Utils.unsafePackageName "shared" /\ releases 1 2 (const Map.empty))
      # Array.cons (Utils.unsafePackageName "root" /\ releases 1 1 (\_ -> deps $ map (\i -> ("branch-" <> show i) /\ range 1 11) (Array.range 0 39)))
      # Map.fromFoldable

    unrelated = map (\i -> Utils.unsafePackageName ("unused-" <> show i) /\ releases 1 1 (\_ -> deps [ "target" /\ range 1 2 ])) (Array.range 0 1999)
      # Array.cons (Utils.unsafePackageName "target" /\ releases 1 1 (const Map.empty))
      # Map.fromFoldable

    conflict = Map.fromFoldable
      [ Utils.unsafePackageName "a" /\ releases 1 2 (\i -> deps [ "z" /\ range i (i + 1) ])
      , Utils.unsafePackageName "b" /\ releases 1 2 (\i -> deps [ "z" /\ range (3 - i) (4 - i) ])
      , Utils.unsafePackageName "z" /\ releases 1 2 (const Map.empty)
      ]

  [ workload "chain-60" chain (deps [ "chain-0" /\ range 1 4 ]) $ Solvable $ Just $ Map.fromFoldable $ map (\i -> Utils.unsafePackageName ("chain-" <> show i) /\ version 3) (Array.range 0 59)
  , workload "diamond-40x10" diamond (deps [ "root" /\ range 1 2 ]) $ Solvable $ Just diamondPlan
  , workload "unreachable-2000" unrelated (deps [ "target" /\ range 1 2 ]) $ Solvable $ Just $ Map.singleton (Utils.unsafePackageName "target") (version 1)
  , workload "narrow-1000" (Map.singleton (Utils.unsafePackageName "releases") (releases 0 999 (const Map.empty))) (deps [ "releases" /\ range 498 500 ]) $ Solvable $ Just $ Map.singleton (Utils.unsafePackageName "releases") (version 499)
  , workload "selection-backtrack" conflict (deps [ "a" /\ range 1 3, "b" /\ range 1 3 ]) $ Solvable $ Just $ Map.fromFoldable [ Utils.unsafePackageName "a" /\ version 2, Utils.unsafePackageName "b" /\ version 1, Utils.unsafePackageName "z" /\ version 2 ]
  , workload "incompatible-roots" conflict (deps [ "a" /\ range 2 3, "b" /\ range 2 3 ]) Unsolvable
  ]

version :: Int -> Version
version = Utils.unsafeVersion <<< (_ <> ".0.0") <<< show

range :: Int -> Int -> Range
range lo hi = unsafeFromJust $ Range.mk (version lo) (version hi)

deps :: Array (Tuple String Range) -> Map PackageName Range
deps = Map.fromFoldable <<< map (lmap Utils.unsafePackageName)

releases :: Int -> Int -> (Int -> Map PackageName Range) -> Map Version (Map PackageName Range)
releases lo hi dependencies = Map.fromFoldable $ map (\i -> version i /\ dependencies i) (Array.range lo hi)
