module Z3 (main) where

import Registry.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Foreign.Object as Object
import Node.Process (exit)
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..))
import Registry.Version (Range, Version)
import Registry.Version as Version
import Unsafe.Coerce (unsafeCoerce)

newtype Var = Var String

derive instance Eq Var
derive instance Ord Var
instance Show Var where
  show (Var s) = show s

type VarDecl = { typ :: String, name :: String }

foreign import data Z3 :: Type
foreign import data Solver :: Type
foreign import data Variables :: Type
foreign import newZ3Impl :: Effect (Promise Z3)
foreign import newSolverImpl :: EffectFn2 Z3 String Solver
foreign import addVariablesImpl :: EffectFn2 Solver (Array VarDecl) Variables
foreign import solveImpl :: EffectFn3 Solver Variables JSClause (Promise (Object Int))

newZ3 :: Aff Z3
newZ3 = Promise.toAffE newZ3Impl

newSolver :: Z3 -> String -> Effect Solver
newSolver z3 = runEffectFn2 newSolverImpl z3

addVariables :: Solver -> Array VarDecl -> Aff Variables
addVariables s = liftEffect <<< runEffectFn2 addVariablesImpl s

solve :: Solver -> Variables -> Z3Clause Int -> Aff (Object Int)
solve s vs clause = do
  let
    toJsClause :: Z3Clause Int -> JSClause
    toJsClause = case _ of
      ZGE pkg v -> { op: "ge", l: unsafeCoerce (PackageName.print pkg), r: unsafeCoerce v }
      ZLT pkg v -> { op: "lt", l: unsafeCoerce (PackageName.print pkg), r: unsafeCoerce v }
      ZEQ pkg v -> { op: "eq", l: unsafeCoerce (PackageName.print pkg), r: unsafeCoerce v }
      ZOR cs -> { op: "or", l: unsafeCoerce (map toJsClause cs), r: unsafeCoerce unit }
      ZAND cs -> { op: "and", l: unsafeCoerce (map toJsClause cs), r: unsafeCoerce unit }
      ZIMP lhs rhs -> { op: "implies", l: unsafeCoerce (toJsClause lhs), r: unsafeCoerce (toJsClause rhs) }
      ZIFF (Var lhs) rhs -> { op: "iff", l: unsafeCoerce lhs, r: unsafeCoerce (toJsClause rhs) }
      ZVAR (Var var) -> { op: "var", l: unsafeCoerce var, r: unsafeCoerce unit }
      ZTRU -> { op: "tru", l: unsafeCoerce unit, r: unsafeCoerce unit }
  Promise.toAffE $ runEffectFn3 solveImpl s vs (toJsClause clause)

type JSClause = { op :: String, l :: Foreign, r :: Foreign }

data Z3Clause a
  = ZOR (Array (Z3Clause a))
  | ZAND (Array (Z3Clause a))
  | ZIMP (Z3Clause a) (Z3Clause a)
  | ZIFF Var (Z3Clause a)
  | ZEQ PackageName a
  | ZGE PackageName a
  | ZLT PackageName a
  | ZVAR Var
  | ZTRU

instance Show a => Show (Z3Clause a) where
  show = case _ of
    ZGE pkg v -> show pkg <> " >= " <> show v
    ZLT pkg v -> show pkg <> " < " <> show v
    ZEQ pkg v -> show pkg <> " == " <> show v
    ZOR cs -> "OR " <> show cs
    ZAND cs -> "AND " <> show cs
    ZIMP lhs rhs -> "IMPLIES: " <> show lhs <> " => " <> show rhs
    ZIFF lhs rhs -> "IFF: " <> show lhs <> " <=> " <> show rhs
    ZVAR (Var var) -> var
    ZTRU -> "TRUE"

derive instance Eq a => Eq (Z3Clause a)
derive instance Ord a => Ord (Z3Clause a)
derive instance Functor Z3Clause

main :: Effect Unit
main = launchAff_ do
  registryIndex <- liftAff $ Index.readRegistryIndex "/Users/fabrizio/Library/Caches/spago-nodejs/registry-index"
  log $ show $ Map.size registryIndex

  -- Pick the package to solve
  let lookupVersions pkg = just $ Map.lookup (right $ PackageName.parse pkg) registryIndex
  let pkg = "node-child-process"
  let vs = "2.0.0"
  -- let pkg = "halogen"
  -- let vs = "7.0.0"
  let
    Manifest manifest = just do
      versions <- Map.lookup (right $ PackageName.parse pkg) registryIndex
      manifest <- Map.lookup (right $ Version.parseVersion Version.Lenient vs) versions
      pure manifest

  -- Construct the clause tree
  clausesCacheRef <- liftEffect $ Ref.new Map.empty
  freshNamesRef <- liftEffect $ Ref.new 0
  let
    memoizeClausesForVersion implicationKey innerManifest = do
      clausesCache <- liftEffect $ Ref.read clausesCacheRef
      case Map.lookup implicationKey clausesCache of
        -- if we have seen this clause before then we just return its name
        Just { var } -> pure $ ZVAR var
        -- if not, we make a new variable, recur, and store the result in the cache
        Nothing -> do
          if Map.isEmpty innerManifest.dependencies then
            pure ZTRU
          else do
            clauses <- clausesForManifest innerManifest
            newVar <- map (\n -> "var__" <> show n) $ liftEffect $ Ref.modify (_ + 1) freshNamesRef
            let
              newClause = case clauses of
                [] -> ZTRU
                [ singleClause ] -> singleClause
                _ -> ZAND clauses
            liftEffect $ Ref.modify_ (Map.insert implicationKey { var: Var newVar, clause: newClause }) clausesCacheRef
            pure $ ZVAR (Var newVar)
    clausesForManifest manifest = for (Map.toUnfoldable manifest.dependencies :: Array (Tuple PackageName Range))
      -- For every dependency range, add a constraint for the range itself, and a bunch of implications for each version in the range (recursive step)
      \(Tuple packageName range) -> do
        let
          manifests = map unwrap $ versionsForRange (lookupVersions (PackageName.print packageName)) range
          rangeClause = ZAND [ ZGE packageName (Version.greaterThanOrEq range), ZLT packageName (Version.lessThan range) ]
          implicationKey version = ZEQ packageName version
          mkImplication innerManifest = do
            rhsClause <- memoizeClausesForVersion (implicationKey innerManifest.version) innerManifest
            -- don't emit implication if rhs is just TRUE
            case rhsClause of
              ZTRU -> pure $ Nothing
              _ -> pure $ Just $ ZIMP (implicationKey innerManifest.version) rhsClause
        implicationClause :: Z3Clause Version <- map (ZAND <<< Array.catMaybes) $ traverse mkImplication manifests
        pure case implicationClause of
          ZAND [] -> rangeClause
          ZAND [ singleClause ] -> ZAND [ rangeClause, singleClause ]
          _ -> ZAND [ rangeClause, implicationClause ]

  manifestClauses <- clausesForManifest manifest
  cachedClauses <- map (Map.values >>> List.toUnfoldable >>> map \{ var, clause } -> ZIFF var clause) $ liftEffect $ Ref.read clausesCacheRef
  let (packageClause :: Z3Clause Version) = ZAND $ manifestClauses <> cachedClauses

  log $ show packageClause
  log "------------------------------------------------------------------------"
  -- Here we map from versions to ints - to avoid mismatches and missing versions we first traverse the tree,
  -- gather all the versions for every package in a set, then sort them and map them to ints
  let lookupTable = gatherVersions registryIndex packageClause
  let (clauseToSolve :: Z3Clause Int) = convertToInts lookupTable packageClause
  let names = getNames packageClause
  -- log $ show names
  log "Getting new solver"
  z3 <- newZ3
  solver <- liftEffect $ newSolver z3 "main"
  log "Adding variables"
  vars <- addVariables solver (Set.toUnfoldable names)
  log "Solving"
  log "------------------------------------------------------------------------"
  result :: Object Int <- solve solver vars clauseToSolve
  resultsArray <- for (Object.toUnfoldable result :: Array (Tuple String Int)) \(Tuple pkgStr n) -> do
    let pkg' = unsafePackageName pkgStr
    newVersion <- liftEffect $ toVersion registryIndex pkg' n
    pure (Tuple pkg' newVersion)
  log "Found a build plan:"
  void $ for (Array.sort resultsArray) \(Tuple p v) -> log $ "  - " <> show p <> ": " <> show v
  liftEffect $ exit 0

just :: forall a. Maybe a -> a
just = fromJust' (\_ -> unsafeCrashWith "this should be a just")

right :: forall a b. Either a b -> b
right = fromRight' (\_ -> unsafeCrashWith "this should be a right")

versionsForRange :: Map Version Manifest -> Range -> Array Manifest
versionsForRange versions range = List.toUnfoldable $ Map.values $ Map.filterWithKey (\v _m -> Version.rangeIncludes range v) versions

type LookupTable =
  { toInt :: Map Version Int
  , toVersion :: Map Int Version
  }

cacheRef :: Ref (Map PackageName LookupTable)
cacheRef = unsafePerformEffect (Ref.new Map.empty)

gatherVersions :: RegistryIndex -> Z3Clause Version -> Map PackageName LookupTable
gatherVersions registryIndex clause = map toLookupTable (go Map.empty clause)
  where
  toLookupTable :: Set Version -> LookupTable
  toLookupTable vs =
    let
      (versions :: Array Version) = Array.fromFoldable vs
    in
      { toInt: Map.fromFoldable $ Array.mapWithIndex (\i v -> Tuple v i) versions
      , toVersion: Map.fromFoldable $ Array.mapWithIndex (\i v -> Tuple i v) versions
      }

  -- In the nothing case the package was not in the memo, so we read it from the index too
  updateVersionSet pkg v Nothing = Just $ fromMaybe (Set.singleton v) $ map Map.keys $ Map.lookup pkg registryIndex
  updateVersionSet _pkg v (Just vs) = Just (Set.insert v vs)

  go acc = case _ of
    ZTRU -> acc
    ZVAR _ -> acc
    ZGE pkg v -> Map.alter (updateVersionSet pkg v) pkg acc
    ZLT pkg v -> Map.alter (updateVersionSet pkg v) pkg acc
    ZEQ pkg v -> Map.alter (updateVersionSet pkg v) pkg acc
    ZIFF _ c -> go acc c
    ZIMP l r -> Array.foldl go acc [ l, r ]
    ZOR cs -> Array.foldl go acc cs
    ZAND cs -> Array.foldl go acc cs

toVersion :: RegistryIndex -> PackageName -> Int -> Effect (Maybe Version)
toVersion _registryIndex packageName version = do
  cache <- Ref.read cacheRef
  case Map.lookup packageName cache of
    Just lt -> do
      log $ "Discarding package " <> show packageName
      pure $ Map.lookup version lt.toVersion
    Nothing -> unsafeCrashWith "wrong"

toInt :: Map PackageName LookupTable -> PackageName -> Version -> Int
toInt lookupTable pkg v = just do
  pkgTables <- Map.lookup pkg lookupTable
  Map.lookup v pkgTables.toInt

convertToInts :: Map PackageName LookupTable -> Z3Clause Version -> Z3Clause Int
convertToInts lookupTable = case _ of
  ZGE pkg v -> ZGE pkg (toInt lookupTable pkg v)
  ZLT pkg v -> ZLT pkg (toInt lookupTable pkg v)
  ZEQ pkg v -> ZEQ pkg (toInt lookupTable pkg v)
  ZOR cs -> ZOR $ map (convertToInts lookupTable) cs
  ZAND cs -> ZAND $ map (convertToInts lookupTable) cs
  ZIMP lhs rhs -> ZIMP (convertToInts lookupTable lhs) (convertToInts lookupTable rhs)
  ZIFF l rhs -> ZIFF l (convertToInts lookupTable rhs)
  ZVAR v -> ZVAR v
  ZTRU -> ZTRU

getNames :: forall a. Z3Clause a -> Set VarDecl
getNames = case _ of
  ZGE pkg _ -> intVar pkg
  ZLT pkg _ -> intVar pkg
  ZEQ pkg _ -> intVar pkg
  ZOR cs -> Set.unions (map getNames cs)
  ZAND cs -> Set.unions (map getNames cs)
  ZIMP l r -> Set.unions [ getNames l, getNames r ]
  ZIFF l r -> Set.unions [ boolVar l, getNames r ]
  ZVAR v -> boolVar v
  ZTRU -> Set.empty
  where
  intVar pkg = Set.singleton { typ: "int", name: PackageName.print pkg }
  boolVar (Var var) = Set.singleton { typ: "bool", name: var }

unsafePackageName :: String -> PackageName
unsafePackageName = right <<< PackageName.parse

-- TODO: when we'll want to extract the unsat core, we need to set the
-- core to be minimal:
-- def set_core_minimize(s):
--  s.set("sat.core.minimize","true")  # For Bit-vector theories
--  s.set("smt.core.minimize","true")  # For general SMT
