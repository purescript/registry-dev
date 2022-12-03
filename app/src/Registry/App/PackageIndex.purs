module Registry.App.PackageIndex where

import Registry.App.Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Foreign.FastGlob (Include(..))
import Foreign.FastGlob as FastGlob
import Node.Path as Path
import Registry.App.RegistryM (GitHubEnv)
import Registry.Effect.Class (class MonadRegistry)
import Registry.Effect.Class as Registry
import Registry.Effect.Log as Log
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version

-- | Attempt to read a manifest index from disk, throwing an exception if the
-- | index is not valid or cannot be read.
readManifestIndexFromDisk :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => m ManifestIndex
readManifestIndexFromDisk = do
  registryIndex <- asks _.registryIndex
  packagePaths <- liftAff $ FastGlob.match' registryIndex [ "**/*" ] { include: FilesOnly, ignore: [ "config.json" ] }
  let packages = Array.mapMaybe (hush <<< PackageName.parse <<< Path.basename) packagePaths.succeeded
  entries <- for packages (ManifestIndex.readEntryFile registryIndex)
  let { fail, success } = partitionEithers entries
  case fail of
    [] -> case ManifestIndex.fromSet $ Set.fromFoldable $ Array.foldMap NonEmptyArray.toArray success of
      Left errors -> Log.die $ append "Invalid ManifestIndex (some packages are not satisfiable):\n" $ String.joinWith "\n" do
        Tuple name versions <- Map.toUnfoldable errors
        Tuple version dependency <- Map.toUnfoldable versions
        let
          dependencies = do
            Tuple depName depRange <- Map.toUnfoldable dependency
            [ PackageName.print depName <> "(" <> Range.print depRange <> ")" ]
        pure $ Array.fold [ PackageName.print name, "@", Version.print version, " cannot satisfy: ", String.joinWith ", " dependencies ]

      Right index ->
        pure index

    errors ->
      Log.die $ append "Invalid ManifestIndex (some package entries cannot be decoded):\n" $ String.joinWith "\n" errors

-- | Attempt to insert a manifest into the registry manifest index, committing
-- | the result.
writeInsertIndex :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => Manifest -> m (Either String Unit)
writeInsertIndex manifest@(Manifest { name }) = do
  registryIndexDir <- asks _.registryIndex
  ManifestIndex.insertIntoEntryFile registryIndexDir manifest >>= case _ of
    Left error -> pure (Left error)
    Right _ -> Registry.commitIndexFile name

-- | Attempt to delete a manifest from the registry manifest index, committing
-- | the result.
writeDeleteIndex :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => PackageName -> Version -> m (Either String Unit)
writeDeleteIndex name version = do
  registryIndexDir <- asks _.registryIndex
  ManifestIndex.removeFromEntryFile registryIndexDir name version >>= case _ of
    Left error -> pure (Left error)
    Right _ -> Registry.commitIndexFile name
