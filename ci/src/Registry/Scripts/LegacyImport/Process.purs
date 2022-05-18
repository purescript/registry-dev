module Registry.Scripts.LegacyImport.Process where

import Registry.Prelude

import Control.Apply (lift2)
import Control.Monad.Except as Except
import Control.Monad.State as State
import Data.Array as Array
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Newtype as Newtype
import Data.Time.Duration (Hours)
import Effect.Now (nowDateTime) as Time
import Foreign.GitHub (PackageURL)
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Node.Path as Path
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.Scripts.LegacyImport.Error (ImportError(..), ImportErrorKey, PackageFailures(..), RequestError(..))
import Registry.Scripts.LegacyImport.Error as LegacyImport.Error
import Registry.Types (RawPackageName, RawVersion)
import Registry.Version (Version)

type ProcessedPackages k a =
  { failures :: PackageFailures
  , packages :: Map k a
  }

type ProcessedPackageVersions k1 k2 a = ProcessedPackages k1 (Map k2 a)

-- | Execute the provided transform on every package in the input packages map
-- | collecting failures into `PackageFailures` and saving transformed packages.
forPackage
  :: ProcessedPackages RawPackageName PackageURL
  -> ( RawPackageName
       -> PackageURL
       -> ExceptT ImportError Aff (Tuple { address :: GitHub.Address, name :: RawPackageName } (Map RawVersion Unit))
     )
  -> Aff (ProcessedPackages { address :: GitHub.Address, name :: RawPackageName } (Map RawVersion Unit))
forPackage input f =
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate = forWithIndex_ input.packages \name value ->
    lift (Except.runExceptT (f name value)) >>= case _ of
      Left err -> do
        let
          errorType = LegacyImport.Error.printImportErrorKey err
          failure = Map.singleton name (Left err)
        State.modify \state -> state { failures = insertFailure errorType failure state.failures }
      Right (Tuple newKey result) -> do
        let insertPackage = Map.insert newKey result
        State.modify \state -> state { packages = insertPackage state.packages }

-- | Execute the provided transform on every package in the input packages map,
-- | at every version of that package, collecting failures into `PackageFailures`
-- | and preserving transformed packages.
forPackageVersion
  :: forall a b
   . ProcessedPackageVersions
       { address :: GitHub.Address
       , name :: PackageName
       , original :: RawPackageName
       }
       { version :: Version, original :: RawVersion }
       a
  -> ( { address :: GitHub.Address
       , name :: PackageName
       , original :: RawPackageName
       }
       -> { version :: Version, original :: RawVersion }
       -> a
       -> ExceptT ImportError Aff b
     )
  -> Aff (ProcessedPackageVersions { address :: GitHub.Address, name :: PackageName, original :: RawPackageName } { version :: Version, original :: RawVersion } b)
forPackageVersion input f = do
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate = forWithIndex_ input.packages \k1@{ original: name } inner ->
    forWithIndex_ inner \k2@{ original: tag } value -> do
      lift (Except.runExceptT (f k1 k2 value)) >>= case _ of
        Left err -> do
          let
            errorType = LegacyImport.Error.printImportErrorKey err
            failure = Map.singleton name $ Right $ Map.singleton tag err
          State.modify \state -> state { failures = insertFailure errorType failure state.failures }
        Right result -> do
          let
            newPackage = Map.singleton k2 result
            insertPackage = Map.insertWith Map.union k1 newPackage
          State.modify \state -> state { packages = insertPackage state.packages }

forPackageVersionKeys
  :: ProcessedPackageVersions { address :: GitHub.Address, name :: RawPackageName } RawVersion Unit
  -> ( { address :: GitHub.Address
       , name :: RawPackageName
       }
       -> RawVersion
       -> ExceptT ImportError Aff
            ( Tuple
                { address :: GitHub.Address
                , name :: PackageName
                , original :: RawPackageName
                }
                { version :: Version, original :: RawVersion }
            )
     )
  -> Aff
       ( ProcessedPackageVersions
           { address :: GitHub.Address
           , name :: PackageName
           , original :: RawPackageName
           }
           { version :: Version, original :: RawVersion }
           Unit
       )
forPackageVersionKeys input f = do
  map snd $ State.runStateT iterate { failures: input.failures, packages: Map.empty }
  where
  iterate = forWithIndex_ input.packages \k1@{ name } inner ->
    forWithIndex_ inner \tag value ->
      lift (Except.runExceptT (f k1 tag)) >>= case _ of
        Left err -> do
          let
            errorType = LegacyImport.Error.printImportErrorKey err
            failure = Map.singleton name $ Right $ Map.singleton tag err
          State.modify \state -> state { failures = insertFailure errorType failure state.failures }
        Right (Tuple k3 k4) -> do
          let
            newPackage = Map.singleton k4 value
            insertPackage = Map.insertWith Map.union k3 newPackage
          State.modify \state -> state { packages = insertPackage state.packages }

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

jsonSerializer :: forall a. RegistryJson a => Serialize String a
jsonSerializer =
  { encode: Json.stringifyJson
  , decode: Json.parseJson
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
    objectPath = Path.concat [ cacheFolder, path ]

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
        writeEncoded :: String -> Aff Unit
        writeEncoded = FS.writeTextFile UTF8 objectPath

      liftAff (Except.runExceptT action) >>= case _ of
        Right result -> do
          liftAff $ writeEncoded $ encode result
          pure result
        -- We want to cache some files that we process, even if they fail, so that
        -- we don't attempt to process them again.
        Left importError | cacheFailure importError -> do
          liftAff $ writeEncoded $ Json.stringifyJson importError
          throwError importError
        Left importError -> do
          throwError importError

    isCacheHit = liftAff do
      exists <- FS.exists objectPath
      expired <- case exists, maybeDuration of
        _, Nothing -> pure false
        false, _ -> pure false
        true, Just duration -> do
          lastModified <- FS.stat objectPath <#> unsafeFromJust <<< JSDate.toDateTime <<< _.mtime <<< (\(Stats s) -> s)
          now <- liftEffect $ Time.nowDateTime
          let expiryTime = unsafeFromJust $ Time.adjust duration lastModified
          pure (now > expiryTime)
      pure (exists && not expired)

  liftAff $ FS.Extra.ensureDirectory cacheFolder

  isCacheHit >>= case _ of
    true -> do
      contents <- liftAff $ FS.readTextFile UTF8 objectPath
      case decode contents of
        Left _ -> do
          case Json.parseJson contents of
            Left _ ->
              log $ "Could not decode " <> objectPath
            Right importError -> do
              throwError importError
          onCacheMiss
        Right a ->
          pure a

    false -> do
      onCacheMiss
