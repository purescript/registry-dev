module Registry.App.Effect.Index where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Effect.Cache (CACHE)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.GitHub (WRITE_GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify (NOTIFY)
import Registry.App.Effect.Notify as Notify
import Registry.App.Json as Json
import Registry.Constants as Constants
import Registry.Foreign.FastGlob as FastGlob
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Except (FAIL)
import Run.Except as Exn

data Index a
  = ReadMetadataIndex (Map PackageName Metadata -> a)
  | ReadMetadata PackageName (Maybe Metadata -> a)
  | WriteMetadata PackageName Metadata a
  | ReadManifestIndex (ManifestIndex -> a)
  | ReadManifest PackageName Version (Maybe Manifest -> a)
  | WriteManifest Manifest a
  | DeleteManifest PackageName Version a

derive instance Functor Index

-- | An effect for interacting with Registry indices of package manifests and
-- | package metadata.
type INDEX r = (index :: Index | r)

_index :: Proxy "index"
_index = Proxy

readMetadataIndex :: forall r. Run (INDEX + r) (Map PackageName Metadata)
readMetadataIndex = Run.lift _index (ReadMetadataIndex identity)

readMetadata :: forall r. PackageName -> Run (INDEX + r) (Maybe Metadata)
readMetadata name = Run.lift _index (ReadMetadata name identity)

writeMetadata :: forall r. PackageName -> Metadata -> Run (INDEX + r) Unit
writeMetadata name metadata = Run.lift _index (WriteMetadata name metadata unit)

readManifestIndex :: forall r. Run (INDEX + r) ManifestIndex
readManifestIndex = Run.lift _index (ReadManifestIndex identity)

readManifest :: forall r. PackageName -> Version -> Run (INDEX + r) (Maybe Manifest)
readManifest name version = Run.lift _index (ReadManifest name version identity)

writeManifest :: forall r. Manifest -> Run (INDEX + r) Unit
writeManifest manifest = Run.lift _index (WriteManifest manifest unit)

deleteManifest :: forall r. PackageName -> Version -> Run (INDEX + r) Unit
deleteManifest name version = Run.lift _index (DeleteManifest name version unit)

type IndexEnv =
  { registryPath :: FilePath
  , registryIndexPath :: FilePath
  }

handleIndexGitHub :: forall r a. IndexEnv -> Index a -> Run (WRITE_GITHUB + CACHE + NOTIFY + LOG + FAIL + AFF + r) a
handleIndexGitHub { registryPath, registryIndexPath } = case _ of
  ReadMetadataIndex reply -> do
    let metadataDir = Path.concat [ registryPath, Constants.packageMetadataDirectory ]

    files <- Run.liftAff (Aff.attempt (FS.Aff.readdir metadataDir)) >>= case _ of
      Left err -> do
        Log.error $ "Could not read the metadata directory at path " <> metadataDir <> " due to an fs error: " <> Aff.message err
        Exn.fail
      Right paths ->
        pure paths

    let stripSuffix = note "No .json suffix" <<< String.stripSuffix (String.Pattern ".json")
    let packages = partitionEithers $ map (PackageName.parse <=< stripSuffix) files
    unless (Array.null packages.fail) do
      Notify.exit $ "Some entries in the metadata directory are not valid package names:" <> Array.foldMap (append "\n  - ") packages.fail

    entries <- Run.liftAff $ map partitionEithers $ for packages.success \name -> do
      result <- Json.readJsonFile Metadata.codec (Path.concat [ registryPath, Constants.packageMetadataDirectory, PackageName.print name <> ".json" ])
      pure $ map (Tuple name) result
    unless (Array.null entries.fail) do
      Notify.exit $ append "The metadata directory is invalid (some package metadata cannot be decoded):" $ Array.foldMap (append "\n  - ") entries.fail

    Log.debug "Successfully read metadata entries."
    pure $ reply $ Map.fromFoldable entries.success

  ReadMetadata name reply -> do
    Log.debug $ "Reading metadata for " <> PackageName.print name
    let path = Path.concat [ registryPath, Constants.packageMetadataDirectory, PackageName.print name <> ".json" ]
    Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 path)) >>= case _ of
      Left fsError -> do
        Log.debug $ "Could not find metadata file: " <> Aff.message fsError
        pure $ reply Nothing
      Right contents -> case Json.jsonParser contents of
        Left jsonError -> Notify.exit $ "Metadata file at path " <> path <> " is not valid JSON: " <> jsonError
        Right parsed -> case Json.decode Metadata.codec parsed of
          Left decodeError -> Notify.exit $ "Metadata file at path " <> path <> " could not be parsed: " <> Json.printJsonDecodeError decodeError
          Right metadata -> do
            Log.debug $ "Successfully read metadata for " <> PackageName.print name
            pure $ reply $ Just metadata

  WriteMetadata name metadata next -> do
    Log.debug $ "Writing metadata for " <> PackageName.print name
    let path = Path.concat [ registryPath, Constants.packageMetadataDirectory, PackageName.print name <> ".json" ]
    Run.liftAff (Aff.attempt (Json.writeJsonFile Metadata.codec path metadata)) >>= case _ of
      Left fsError -> Notify.exit $ "Could not write metadata file to " <> path <> " due to an fs error: " <> Aff.message fsError
      Right _ -> GitHub.commitMetadata name
    pure next

  ReadManifestIndex reply -> do
    Log.debug $ "Reading manifest index from " <> registryIndexPath
    paths <- Run.liftAff $ FastGlob.match' registryIndexPath [ "**/*" ] { include: FastGlob.FilesOnly, ignore: [ "config.json" ] }
    let packages = partitionEithers $ map (PackageName.parse <<< Path.basename) paths.succeeded
    Log.warn $ "Some entries in the manifest index are not valid package names: " <> Array.foldMap (append "\n  - ") packages.fail
    entries <- Run.liftAff $ map partitionEithers $ for packages.success (ManifestIndex.readEntryFile registryIndexPath)
    case entries.fail of
      [] -> case ManifestIndex.fromSet $ Set.fromFoldable $ Array.foldMap NonEmptyArray.toArray entries.success of
        Left errors -> Notify.exit $ append "Unable to read manifest index (some packages are not satisfiable): " $ Array.foldMap (append "\n  - ") do
          Tuple name versions <- Map.toUnfoldable errors
          Tuple version dependency <- Map.toUnfoldable versions
          let
            dependencies = do
              Tuple depName depRange <- Map.toUnfoldable dependency
              [ PackageName.print depName <> "(" <> Range.print depRange <> ")" ]
          pure $ Array.fold [ formatPackageVersion name version, " cannot satisfy: ", String.joinWith ", " dependencies ]

        Right index ->
          pure $ reply index

      failed ->
        Notify.exit $ append "Could not read manifest index (some package entries cannot be decoded): " $ Array.foldMap (append "\n  - ") failed

  ReadManifest name version reply -> do
    Log.debug $ "Reading manifest for " <> formatPackageVersion name version <> "..."
    Cache.get (Cache.Manifest name version) >>= case _ of
      Nothing -> do
        Run.liftAff (ManifestIndex.readEntryFile registryIndexPath name) >>= case _ of
          Left error -> do
            Log.warn $ "Could not read manifest index entry for package " <> PackageName.print name <> " because no entries were found: " <> error
            pure $ reply Nothing
          Right entries ->
            case NonEmptyArray.find (\(Manifest m) -> m.name == name && m.version == version) entries of
              Nothing -> do
                Log.warn $ "Found manifest index entry for package " <> PackageName.print name <> " but none for version " <> Version.print version
                pure $ reply Nothing
              Just entry -> do
                Cache.put (Cache.Manifest name version) entry
                pure $ reply $ Just entry
      Just cached ->
        pure $ reply $ Just cached.value

  WriteManifest manifest@(Manifest { name, version }) next -> do
    let formatted = formatPackageVersion name version
    Log.debug $ "Writing manifest for " <> formatted <> ":\n" <> Json.printJson Manifest.codec manifest
    Run.liftAff (ManifestIndex.insertIntoEntryFile registryIndexPath manifest) >>= case _ of
      Left error -> Notify.exit $ "Could not insert manifest for " <> formatted <> " into its entry file due to an error: " <> error
      Right _ -> do
        Cache.put (Cache.Manifest name version) manifest
        GitHub.commitManifest name version
        pure next

  DeleteManifest name version next -> do
    let formatted = formatPackageVersion name version
    Log.debug $ "Deleting manifest for " <> formatted
    Run.liftAff (ManifestIndex.removeFromEntryFile registryIndexPath name version) >>= case _ of
      Left error -> Notify.exit $ "Could not remove manifest for " <> formatted <> " from its entry file due to an error: " <> error
      Right _ -> do
        Cache.delete (Cache.Manifest name version)
        GitHub.commitManifest name version
        pure next
