module Registry.Scripts.BowerImport.Process where

import Registry.Prelude

import Control.Apply (lift2)
import Control.Monad.Except as Except
import Control.Parallel as Parallel
import Data.Argonaut as Json
import Data.Array as Array
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Newtype as Newtype
import Data.Time.Duration (Hours)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Effect.AVar as Effect.AVar
import Effect.Aff.AVar as AVar
import Effect.Now (nowDateTime) as Time
import Foreign.Jsonic as Jsonic
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Registry.Scripts.BowerImport.Error (ImportError, ImportErrorKey, PackageFailures(..), RawPackageName, RawVersion)
import Registry.Scripts.BowerImport.Error as BowerImport.Error

type ProcessedPackages k a =
  { failures :: PackageFailures
  , packages :: Map k a
  }

type ProcessedPackageVersions k1 k2 a = ProcessedPackages k1 (Map k2 a)

-- | Execute the provided transform on every package in the input packages map
-- | collecting failures into `PackageFailures` and saving transformed packages.
forPackage
  :: forall k1 k2 a b
   . Ord k1
  => Ord k2
  => ProcessedPackages k1 a
  -> (k1 -> RawPackageName)
  -> (k1 -> a -> ExceptT ImportError Aff (Tuple k2 b))
  -> Aff (ProcessedPackages k2 b)
forPackage input keyToPackageName f = do
  var <- AVar.new { failures: input.failures, packages: Map.empty }
  parBounded input.packages \key value ->
    Except.runExceptT (f key value) >>= case _ of
      Left err -> do
        let
          errorType = BowerImport.Error.printImportErrorKey err
          name = keyToPackageName key
          failure = Map.singleton name (Left err)
          modify state = state { failures = insertFailure errorType failure state.failures }
        state <- AVar.take var
        AVar.put (modify state) var
      Right (Tuple newKey result) -> do
        let
          insertPackage = Map.insert newKey result
          modify state = state { packages = insertPackage state.packages }
        state <- AVar.take var
        AVar.put (modify state) var
  AVar.read var

-- | Execute the provided transform on every package in the input packages map,
-- | at every version of that package, collecting failures into `PackageFailures`
-- | and preserving transformed packages.
forPackageVersion
  :: forall k1 k2 a b
   . Ord k1
  => Ord k2
  => ProcessedPackageVersions k1 k2 a
  -> (k1 -> RawPackageName)
  -> (k2 -> RawVersion)
  -> (k1 -> k2 -> a -> ExceptT ImportError Aff b)
  -> Aff (ProcessedPackageVersions k1 k2 b)
forPackageVersion input keyToPackageName keyToTag f = do
  var <- AVar.new { failures: input.failures, packages: Map.empty }
  parBounded input.packages \k1 inner ->
    parBounded inner \k2 value -> do
      Except.runExceptT (f k1 k2 value) >>= case _ of
        Left err -> do
          let
            errorType = BowerImport.Error.printImportErrorKey err
            name = keyToPackageName k1
            tag = keyToTag k2
            failure = Map.singleton name $ Right $ Map.singleton tag err
            modify state = state { failures = insertFailure errorType failure state.failures }
          state <- AVar.take var
          AVar.put (modify state) var
        Right result -> do
          let
            newPackage = Map.singleton k2 result
            insertPackage = Map.insertWith Map.union k1 newPackage
            modify state = state { packages = insertPackage state.packages }
          state <- AVar.take var
          AVar.put (modify state) var
  AVar.read var

forPackageVersionKeys
  :: forall k1 k2 k3 k4 a
   . Ord k1
  => Ord k2
  => Ord k3
  => Ord k4
  => ProcessedPackageVersions k1 k2 a
  -> (k1 -> RawPackageName)
  -> (k2 -> RawVersion)
  -> (k1 -> k2 -> ExceptT ImportError Aff (Tuple k3 k4))
  -> Aff (ProcessedPackageVersions k3 k4 a)
forPackageVersionKeys input keyToPackageName keyToTag f = do
  var <- AVar.new { failures: input.failures, packages: Map.empty }
  parBounded input.packages \k1 inner ->
    parBounded inner \k2 value -> do
      Except.runExceptT (f k1 k2) >>= case _ of
        Left err -> do
          let
            errorType = BowerImport.Error.printImportErrorKey err
            name = keyToPackageName k1
            tag = keyToTag k2
            failure = Map.singleton name $ Right $ Map.singleton tag err
            modify state = state { failures = insertFailure errorType failure state.failures }
          state <- AVar.take var
          AVar.put (modify state) var
        Right (Tuple k3 k4) -> do
          let
            newPackage = Map.singleton k4 value
            insertPackage = Map.insertWith Map.union k3 newPackage
            modify state = state { packages = insertPackage state.packages }
          state <- AVar.take var
          AVar.put (modify state) var
  AVar.read var

insertFailure
  :: ImportErrorKey
  -> Map RawPackageName (Either ImportError (Map RawVersion ImportError))
  -> PackageFailures
  -> PackageFailures
insertFailure key value failures = do
  let insert = Map.insertWith (Map.unionWith (lift2 Map.union)) key value
  Newtype.over PackageFailures insert failures

type Serialize e a =
  { encode :: a -> String
  , decode :: String -> Either e a
  }

jsonSerializer :: forall a. Json.EncodeJson a => Json.DecodeJson a => Serialize String a
jsonSerializer =
  { encode: Json.encodeJson >>> Json.stringifyWithIndent 2
  , decode: (Jsonic.parseJson >=> Json.decodeJson) >>> lmap Json.printJsonDecodeError
  }

stringSerializer :: Serialize String String
stringSerializer = { encode: identity, decode: pure }

-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall e a
   . Serialize e a
  -> (ImportError -> Boolean)
  -> FilePath
  -> Maybe Hours
  -> ExceptT ImportError Aff a
  -> ExceptT ImportError Aff a
withCache { encode, decode } cacheFailure path maybeDuration action = do
  let
    cacheFolder = ".cache"
    objectPath = cacheFolder <> "/" <> path

    onCacheMiss = do
      log $ "No cache hit for " <> show path

      let
        writeEncoded :: Either ImportError String -> Aff Unit
        writeEncoded = writeJsonFile objectPath

      liftAff (Except.runExceptT action) >>= case _ of
        Right result -> do
          liftAff $ writeEncoded $ Right $ encode result
          pure result
        -- We want to cache some files that we process, even if they fail, so that
        -- we don't attempt to process them again.
        Left importError | cacheFailure importError -> do
          liftAff $ writeEncoded $ Left importError
          throwError importError
        Left importError ->
          throwError importError

    isCacheHit = liftAff do
      exists <- FS.exists objectPath
      expired <- case exists, maybeDuration of
        _, Nothing -> pure false
        false, _ -> pure false
        true, Just duration -> do
          lastModified <- FS.stat objectPath <#> unsafePartial fromJust <<< JSDate.toDateTime <<< _.mtime <<< (\(Stats s) -> s)
          now <- liftEffect $ Time.nowDateTime
          let expiryTime = unsafePartial fromJust $ Time.adjust duration lastModified
          pure (now > expiryTime)
      pure (exists && not expired)

  lift $ unlessM (FS.exists cacheFolder) (FS.mkdir cacheFolder)

  isCacheHit >>= case _ of
    true -> do
      result :: Either _ (Either ImportError String) <- liftAff $ readJsonFile objectPath
      case result of
        Left error -> do
          log $ "Cache read failed: " <> Json.printJsonDecodeError error
          onCacheMiss
        Right (Left importError) ->
          throwError importError
        Right (Right res) -> case decode res of
          Left _ ->
            onCacheMiss
          Right a -> do
            pure a

    false -> do
      onCacheMiss

-- | Run a computation over an indexed structure in parallel, using a bounded
-- | queue to avoid issues with conflicts over resources like STDOUT.
--
-- Inspired by the work originally done in:
-- https://github.com/natefaubion/purescript-dodo-printer/blob/540dba0442abe686c0b211868d6f423e5df81b69/test/Snapshot.purs#L54-L61
parBounded
  :: forall k t a b
   . TraversableWithIndex k t
  => (t a)
  -> (k -> a -> Aff b)
  -> Aff Unit
parBounded t f = do
  block <- AVar.empty
  -- The number of threads here can be tweaked for performance, but push it too
  -- high and you'll run into issues with STDOUT when using CLI tools, which are
  -- used pervasively in processing.
  for_ (Array.range 1 3) \_ -> do
    liftEffect $ Effect.AVar.put unit block mempty
  parForWithIndex_ t \k v -> do
    AVar.take block
    _ <- f k v
    _ <- liftEffect $ Effect.AVar.put unit block mempty
    pure unit
  where
  parForWithIndex_ t' f' =
    void
      $ Parallel.sequential
      -- NOTE: This *must* use `TraversableWithIndex` instead of
      -- `FoldableWithIndex` (ie. `forWithIndex_`) for stack safety.
      $ forWithIndex t' (\i -> Parallel.parallel <<< f' i)
