module Registry.Scripts.LegacyImport.Process where

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
import Foreign.SemVer (SemVer)
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Registry.PackageName (PackageName)
import Registry.Scripts.LegacyImport.Error (ImportError(..), ImportErrorKey, PackageFailures(..), RawPackageName, RawVersion, RequestError(..))
import Registry.Scripts.LegacyImport.Error as LegacyImport.Error

type ProcessedPackages k a =
  { failures :: PackageFailures
  , packages :: Map k a
  }

type ProcessedPackageVersions k1 k2 a = ProcessedPackages k1 (Map k2 a)
type Address = { owner :: String, repo :: String }
type NameAndAddress = { address :: Address, name :: RawPackageName }
type NameOriginalAndAddress = { address :: Address, name :: PackageName, original :: RawPackageName }
type SemVerAndOriginal = { semVer :: SemVer, original :: RawVersion }

-- | Execute the provided transform on every package in the input packages map
-- | collecting failures into `PackageFailures` and saving transformed packages.
forPackage
  :: ProcessedPackages RawPackageName PackageURL
  -> (RawPackageName -> PackageURL -> ExceptT ImportError Aff (Tuple NameAndAddress (Map RawVersion Unit)))
  -> Aff (ProcessedPackages NameAndAddress (Map RawVersion Unit))
forPackage input f = do
  var <- AVar.new { failures: input.failures, packages: Map.empty }
  parBounded input.packages \name value ->
    Except.runExceptT (f name value) >>= case _ of
      Left err -> do
        let
          errorType = LegacyImport.Error.printImportErrorKey err
          failure = Map.singleton name (Left err)
        var # modifyAVar \state -> state { failures = insertFailure errorType failure state.failures }
      Right (Tuple newKey result) -> do
        let insertPackage = Map.insert newKey result
        var # modifyAVar \state -> state { packages = insertPackage state.packages }
  AVar.read var

-- | Execute the provided transform on every package in the input packages map,
-- | at every version of that package, collecting failures into `PackageFailures`
-- | and preserving transformed packages.
forPackageVersion
  :: forall a b
   . ProcessedPackageVersions NameOriginalAndAddress SemVerAndOriginal a
  -> (NameOriginalAndAddress -> SemVerAndOriginal -> a -> ExceptT ImportError Aff b)
  -> Aff (ProcessedPackageVersions NameOriginalAndAddress SemVerAndOriginal b)
forPackageVersion input f = do
  var <- AVar.new { failures: input.failures, packages: Map.empty }
  parBounded input.packages \k1@{ original: name } inner ->
    parBounded inner \k2@{ original: tag } value -> do
      Except.runExceptT (f k1 k2 value) >>= case _ of
        Left err -> do
          let
            errorType = LegacyImport.Error.printImportErrorKey err
            failure = Map.singleton name $ Right $ Map.singleton tag err
          var # modifyAVar \state -> state { failures = insertFailure errorType failure state.failures }
        Right result -> do
          let
            newPackage = Map.singleton k2 result
            insertPackage = Map.insertWith Map.union k1 newPackage
          var # modifyAVar \state -> state { packages = insertPackage state.packages }
  AVar.read var

forPackageVersionKeys
  :: ProcessedPackageVersions NameAndAddress RawVersion Unit
  -> (NameAndAddress -> RawVersion -> ExceptT ImportError Aff (Tuple NameOriginalAndAddress SemVerAndOriginal))
  -> Aff (ProcessedPackageVersions NameOriginalAndAddress SemVerAndOriginal Unit)
forPackageVersionKeys input f = do
  var <- AVar.new { failures: input.failures, packages: Map.empty }
  parBounded input.packages \k1@{ name } inner ->
    parBounded inner \tag value -> do
      Except.runExceptT (f k1 tag) >>= case _ of
        Left err -> do
          let
            errorType = LegacyImport.Error.printImportErrorKey err
            failure = Map.singleton name $ Right $ Map.singleton tag err
          var # modifyAVar \state -> state { failures = insertFailure errorType failure state.failures }
        Right (Tuple k3 k4) -> do
          let
            newPackage = Map.singleton k4 value
            insertPackage = Map.insertWith Map.union k3 newPackage
          var # modifyAVar \state -> state { packages = insertPackage state.packages }
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
  -> FilePath
  -> Maybe Hours
  -> ExceptT ImportError Aff a
  -> ExceptT ImportError Aff a
withCache { encode, decode } path maybeDuration action = do
  let
    cacheFolder = ".cache"
    objectPath = cacheFolder <> "/" <> path

    -- We also cache some failures if they relate to fetching a resource and
    -- the failure is not temporary (for example, a `404 Not Found` error).
    cacheFailure = case _ of
      ResourceError { error } -> case error of
        BadRequest -> true
        -- We won't re-attempt the status codes listed below, but we'll
        -- re-attempt all others.
        BadStatus status | status `Array.elem` [ 404 ] -> true
        BadStatus _ -> false
        DecodeError _ -> true
      _ -> false

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

modifyAVar :: forall state. (state -> state) -> AVar.AVar state -> Aff Unit
modifyAVar k var = do
  state <- AVar.take var
  AVar.put (k state) var
