module Registry.Scripts.LegacyImport.Process where

import Registry.Prelude

import Control.Apply (lift2)
import Control.Monad.Except as Except
import Control.Monad.State as State
import Data.Map as Map
import Data.Newtype as Newtype
import Foreign.GitHub (PackageURL)
import Foreign.GitHub as GitHub
import Registry.PackageName (PackageName)
import Registry.Scripts.LegacyImport.Error (ImportError, ImportErrorKey, PackageFailures(..))
import Registry.Scripts.LegacyImport.Error as LegacyImport.Error
import Registry.Types (RawPackageName, RawVersion)
import Registry.Version (Version)

type ProcessedPackages k a =
  { failures :: PackageFailures
  , packages :: Map k a
  }

type ProcessedPackageVersions k1 k2 a = ProcessedPackages k1 (Map k2 a)

type NameAddress = { address :: GitHub.Address, name :: RawPackageName }

-- | Execute the provided transform on every package in the input packages map
-- | collecting failures into `PackageFailures` and saving transformed packages.
forPackage
  :: ProcessedPackages RawPackageName PackageURL
  -> (RawPackageName -> PackageURL -> ExceptT ImportError Aff (Tuple NameAddress (Map RawVersion Unit)))
  -> Aff (ProcessedPackages NameAddress (Map RawVersion Unit))
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

type NameAddressOriginal = { address :: GitHub.Address, name :: PackageName, original :: RawPackageName }
type VersionOriginal = { version :: Version, original :: RawVersion }

-- | Execute the provided transform on every package in the input packages map,
-- | at every version of that package, collecting failures into `PackageFailures`
-- | and preserving transformed packages.
forPackageVersion
  :: forall a b
   . ProcessedPackageVersions NameAddressOriginal VersionOriginal a
  -> (NameAddressOriginal -> VersionOriginal -> a -> ExceptT ImportError Aff b)
  -> Aff (ProcessedPackageVersions NameAddressOriginal VersionOriginal b)
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
  :: ProcessedPackageVersions NameAddress RawVersion Unit
  -> (NameAddress -> RawVersion -> ExceptT ImportError Aff (Tuple NameAddressOriginal VersionOriginal))
  -> Aff (ProcessedPackageVersions NameAddressOriginal VersionOriginal Unit)
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
