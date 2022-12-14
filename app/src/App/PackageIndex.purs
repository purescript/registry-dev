module Registry.App.PackageIndex where

import Registry.App.Prelude

import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Node.Path as Path
import Registry.App.RegistryM (RegistryM, commitIndexFile, throwWithComment)
import Registry.Foreign.FastGlob (Include(..))
import Registry.Foreign.FastGlob as FastGlob
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version

-- | Attempt to read a manifest index from disk, throwing an exception if the
-- | index is not valid or cannot be read.
readManifestIndexFromDisk :: RegistryM ManifestIndex
readManifestIndexFromDisk = do
  registryIndex <- asks _.registryIndex
  packagePaths <- liftAff $ FastGlob.match' registryIndex [ "**/*" ] { include: FilesOnly, ignore: [ "config.json" ] }
  let packages = Array.mapMaybe (hush <<< PackageName.parse <<< Path.basename) packagePaths.succeeded
  entries <- for packages (ManifestIndex.readEntryFile registryIndex)
  let { fail, success } = partitionEithers entries
  case fail of
    [] -> case ManifestIndex.fromSet $ Set.fromFoldable $ Array.foldMap NonEmptyArray.toArray success of
      Left errors -> throwWithComment $ append "Invalid ManifestIndex (some packages are not satisfiable):\n" $ String.joinWith "\n" do
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
      throwWithComment $ append "Invalid ManifestIndex (some package entries cannot be decoded):\n" $ String.joinWith "\n" errors

-- | Attempt to insert a manifest into the registry manifest index, committing
-- | the result.
writeInsertIndex :: Manifest -> RegistryM (Either String Unit)
writeInsertIndex manifest@(Manifest { name }) = do
  registryIndexDir <- asks _.registryIndex
  liftAff (ManifestIndex.insertIntoEntryFile registryIndexDir manifest) >>= case _ of
    Left error -> pure (Left error)
    Right _ -> commitIndexFile name

-- | Attempt to delete a manifest from the registry manifest index, committing
-- | the result.
writeDeleteIndex :: PackageName -> Version -> RegistryM (Either String Unit)
writeDeleteIndex name version = do
  registryIndexDir <- asks _.registryIndex
  liftAff (ManifestIndex.removeFromEntryFile registryIndexDir name version) >>= case _ of
    Left error -> pure (Left error)
    Right _ -> commitIndexFile name
