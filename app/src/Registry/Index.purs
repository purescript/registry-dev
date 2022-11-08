module Registry.Index where

import Registry.Prelude

import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Foreign.FastGlob (Include(..))
import Foreign.FastGlob as FastGlob
import Foreign.Node.FS as FS.Extra
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.Json as Json
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM)
import Registry.Version (Version)

-- | NOTE: Right now, this assumes that manifest files will parse
readIndexFromDisk :: RegistryM (Either _ ManifestIndex)
readIndexFromDisk = do
  registryIndex <- asks _.registryIndex
  packagePaths <- liftAff $ FastGlob.match' registryIndex [ "**/*" ] { include: FilesOnly, ignore: [ "config.json" ] }
  let packages = Array.mapMaybe (hush <<< PackageName.parse <<< Path.basename) packagePaths.succeeded
  entries :: Array (Either String (NonEmptyArray Manifest)) <- for packages (ManifestIndex.readEntryFile registryIndex)
  pure $ Left Map.empty

