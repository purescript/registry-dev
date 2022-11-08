-- | Implementation of the `ManifestIndex` data type as described in the registry
-- | spec. The manifest index records all packages in the registry along with the
-- | manifests associated with each version of the package.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#36-manifest-index
-- |
-- | The manifest index is published in the registry-index repository:
-- | https://github.com/purescript/registry-index
module Registry.ManifestIndex
  ( ManifestIndex
  , empty
  , fromSet
  , insertIntoEntryFile
  , lookup
  , maximalIndex
  , maximalIndexIgnoringBounds
  , packageEntryDirectory
  , packageEntryFilePath
  , parseEntry
  , printEntry
  , readEntryFile
  , removeFromEntryFile
  , toMap
  , toSortedArray
  , topologicalSort
  , writeEntryFile
  ) where

import Prelude

import Control.Monad.Error.Class as Error
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as MAp
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.FS.Perms as FS.Perms
import Node.FS.Sync as FS.Sync
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)
import Registry.Manifest (Manifest(..))
import Registry.Manifest as Manifest
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version)

-- | An index of package manifests, keyed by package name and version. The index
-- | is correct by construction: manifests only list dependencies on package
-- | versions which are also in the index.
newtype ManifestIndex = ManifestIndex (Map PackageName (Map Version Manifest))

derive instance Eq ManifestIndex

-- | Create an empty `ManifestIndex`
empty :: ManifestIndex
empty = ManifestIndex Map.empty

-- | Convert a manifest index into a `Map`
toMap :: ManifestIndex -> Map PackageName (Map Version Manifest)
toMap (ManifestIndex index) = index

-- | Produce an array of manifests topologically sorted by dependencies.
toSortedArray :: ManifestIndex -> Array Manifest
toSortedArray (ManifestIndex index) = topologicalSort $ Set.fromFoldable do
  Tuple _ versions <- Map.toUnfoldableUnordered index
  Tuple _ manifest <- MAp.toUnfoldableUnordered versions
  [ manifest ]

-- | Look up a package version's manifest in the manifest index.
lookup :: PackageName -> Version -> ManifestIndex -> Maybe Manifest
lookup name version (ManifestIndex index) =
  Map.lookup name index
    >>= Map.lookup version

-- | Insert a new manifest into the manifest index, failing if the manifest
-- | indicates dependencies that cannot be satisfied. Dependencies are not
-- | satisfied if a) the package is not in the index or b) there are no versions
-- | in the indicated range.
insert :: Manifest -> ManifestIndex -> Either (Map PackageName Range) ManifestIndex
insert manifest@(Manifest { name, version, dependencies }) (ManifestIndex index) = do
  let
    unsatisfied :: Map PackageName Range
    unsatisfied = Map.fromFoldable do
      Tuple dependency range <- Map.toUnfoldable dependencies
      case Map.lookup dependency index of
        Just versions | Array.any (Range.includes range) (Set.toUnfoldable (Map.keys versions)) ->
          []
        _ ->
          [ Tuple dependency range ]

  if Map.isEmpty unsatisfied then
    Right $ ManifestIndex $ Map.insertWith Map.union name (Map.singleton version manifest) index
  else
    Left unsatisfied

-- | Convert a set of manifests into a `ManifestIndex`. Reports all failures
-- | encountered rather than short-circuiting.
fromSet :: Set Manifest -> Either (Map PackageName (Map Version (Map PackageName Range))) ManifestIndex
fromSet manifests = do
  let Tuple failed index = maximalIndex manifests
  if Map.isEmpty failed then Right index else Left failed

-- | Produce the maximal `ManifestIndex` possible for the given set of
-- | `Manifest`s, collecting failures along the way.
maximalIndex :: Set Manifest -> Tuple (Map PackageName (Map Version (Map PackageName Range))) ManifestIndex
maximalIndex manifests = do
  let
    insertManifest (Tuple failed index) manifest@(Manifest { name, version }) = case insert manifest index of
      Left errors -> Tuple (Map.insertWith Map.union name (Map.singleton version errors) failed) index
      Right newIndex -> Tuple failed newIndex

  Array.foldl insertManifest (Tuple Map.empty empty) (topologicalSort manifests)

-- | Convert a set of manifests into an approximate manifest index, ignoring
-- | package version bounds. This is useful for verifying that a collection of
-- | packages is self-contained in situations where ranges are ignored, such as
-- | the package sets.
maximalIndexIgnoringBounds :: Set Manifest -> Tuple (Map PackageName (Map Version (Array PackageName))) (Map PackageName (Map Version Manifest))
maximalIndexIgnoringBounds manifests = do
  let
    insertIgnoringBounds :: Manifest -> Map PackageName (Map Version Manifest) -> Either (Array PackageName) (Map PackageName (Map Version Manifest))
    insertIgnoringBounds manifest@(Manifest { name, version, dependencies }) index = do
      let
        unsatisfied = do
          Tuple dependency _ <- Map.toUnfoldable dependencies
          Maybe.maybe [ dependency ] (const []) $ Map.lookup dependency index

      if not (Array.null unsatisfied) then
        Left unsatisfied
      else
        Right $ Map.insertWith Map.union name (Map.singleton version manifest) index

    foldFn (Tuple failed index) manifest@(Manifest { name, version }) = case insertIgnoringBounds manifest index of
      Left errors -> Tuple (Map.insertWith Map.union name (Map.singleton version errors) failed) index
      Right newIndex -> Tuple failed newIndex

  Array.foldl foldFn (Tuple Map.empty Map.empty) (topologicalSort manifests)

-- | Topologically sort a set of manifests so that each manifest in the array
-- | depends only on package versions that have already been encountered.
topologicalSort :: Set Manifest -> Array Manifest
topologicalSort manifests =
  Array.fromFoldable
    $ List.reverse
    $ List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph
  where
  -- An array of all versions associated with the given package.
  allPackageVersions :: Map PackageName (Array Version)
  allPackageVersions =
    Map.fromFoldableWith append
      $ Set.map (\(Manifest { name, version }) -> Tuple name [ version ]) manifests

  -- Every (Tuple PackageName Version) has a Manifest, and it has dependencies
  -- on a list of other (Tuple PackageName Version)
  graph :: Graph (Tuple PackageName Version) Manifest
  graph = Graph.fromMap $ Map.fromFoldable $ map resolveDependencies $ Array.fromFoldable manifests

  -- Given a `Manifest` containing a `Map PackageName Range` indicating
  -- dependencies, produce a list of all package versions that could possibly
  -- be satisfied by those ranges as a Graph-compatible vertex.
  resolveDependencies :: Manifest -> Tuple (Tuple PackageName Version) (Tuple Manifest (List (Tuple PackageName Version)))
  resolveDependencies manifest@(Manifest { name, version, dependencies }) =
    Tuple (Tuple name version) $ Tuple manifest $ List.fromFoldable do
      Tuple dependency range <- Map.toUnfoldable dependencies
      -- This case should not be possible: it means that the manifest indicates
      -- a dependency that does not exist at all. (TODO: Explain)
      let versions = Maybe.fromMaybe [] $ Map.lookup dependency allPackageVersions
      included <- Array.filter (Range.includes range) versions
      [ Tuple dependency included ]

-- | Calculate the directory containing this package in the registry index,
-- | using the following format:
-- |
-- | * Packages with 1 character names are placed in a directory named 1.
-- |
-- | * Packages with 2 character names are placed in a directory named 2.
-- |
-- | * Packages with 3 character names are placed in the directory 3/{first-character}
-- |   where {first-character} is the first character of the package name.
-- |
-- | * All other packages are stored in directories named {first-two}/{second-two}
-- |   where the top directory is the first two characters of the package name,
-- |   and the next subdirectory is the third and fourth characters of the
-- |   package name. For example, prelude would be stored in the 'pr/el'
-- |   directory.
-- |
-- | Format follows that used by Cargo in crates.io: https://github.com/rust-lang/crates.io-index
-- | As documented in the Cargo book: https://doc.rust-lang.org/cargo/reference/registries.html#index-format
packageEntryDirectory :: PackageName -> FilePath
packageEntryDirectory = PackageName.print >>> \name -> case String.length name of
  -- Package names are validated to be non-empty strings, so this case is not
  -- reachable unless the package name was unsafely coerced.
  0 -> unsafeCrashWith "Unexpected empty PackageName"
  1 -> "1"
  2 -> "2"
  3 -> Path.concat [ "3", String.take 1 name ]
  _ -> Path.concat [ String.take 2 name, String.take 2 (String.drop 2 name) ]

-- | Calculate the full file path for a given package name. For example, the
-- | 'prelude' package would be stored at 'pr/el/prelude'.
packageEntryFilePath :: PackageName -> FilePath
packageEntryFilePath name = Path.concat [ packageEntryDirectory name, PackageName.print name ]

-- | Parse a JSON Lines string listing several manifests in sorted order from
-- | lowest version to highest version.
parseEntry :: String -> Either String (NonEmptyArray Manifest)
parseEntry entry = do
  let split = String.split (String.Pattern "\n") <<< String.trim
  jsonArray <- traverse Argonaut.Parser.jsonParser (split entry)
  entries <- traverse (lmap CA.printJsonDecodeError <<< CA.decode Manifest.codec) jsonArray
  case NonEmptyArray.fromArray entries of
    Nothing -> Left "No entries exist."
    Just entries' -> pure entries'

-- | Print an array of manifests as a JSON Lines string in sorted order from
-- | lowest version to highest version.
printEntry :: NonEmptySet Manifest -> String
printEntry =
  Array.foldMap ((_ <> "\n") <<< Argonaut.stringify <<< CA.encode Manifest.codec)
    <<< Array.sortBy (comparing (_.version <<< un Manifest))
    <<< Array.fromFoldable

-- | Given the root of a manifest index on the file system and a package name,
-- | retrieve the manifests associated with that package, in sorted order from
-- | lowest to highest version.
readEntryFile :: forall m. MonadAff m => FilePath -> PackageName -> m (Either String (NonEmptyArray Manifest))
readEntryFile indexPath package = do
  let entryPath = Path.concat [ indexPath, packageEntryFilePath package ]
  liftAff (Error.try (FS.Aff.readTextFile UTF8 entryPath)) >>= case _ of
    Left error -> pure $ Left $ "Failed to read entry: " <> Exception.message error
    Right contents -> pure $ parseEntry contents

-- | Given the root of a manifest index on the file system, encode the provided
-- | list of manifests as a package entry in the JSON Lines format. This will
-- | fail if the manifests do not all share the same package name.
writeEntryFile :: forall m. MonadAff m => FilePath -> NonEmptySet Manifest -> m (Either String Unit)
writeEntryFile indexPath manifests = do
  let names = NonEmptySet.map (_.name <<< un Manifest) manifests
  case NonEmptySet.size names of
    1 -> do
      let Manifest { name } = NonEmptySet.min manifests
      let entryPath = Path.concat [ indexPath, packageEntryFilePath name ]
      unlessM (liftEffect (FS.Sync.exists entryPath)) do
        liftAff $ FS.Aff.mkdir' entryPath { recursive: true, mode: FS.Perms.mkPerms FS.Perms.all FS.Perms.all FS.Perms.all }
      let entry = printEntry manifests
      liftAff (Error.try (FS.Aff.writeTextFile UTF8 entryPath entry)) >>= case _ of
        Left error -> pure $ Left $ Exception.message error
        Right _ -> pure $ Right unit

    n -> pure $ Left $ Array.fold
      [ "Package entries can only contain one package, but "
      , Int.toStringAs Int.decimal n
      , " were provided: "
      , String.joinWith ", " $ map PackageName.print $ Array.fromFoldable names
      ]

-- | Given the root of a manifest index on the file system, insert the specified
-- | manifest into the package entry. This will create the package entry if it
-- | does not yet exist.
insertIntoEntryFile :: forall m. MonadAff m => FilePath -> Manifest -> m (Either String Unit)
insertIntoEntryFile indexPath manifest@(Manifest { name }) = do
  entry <- readEntryFile indexPath name

  let
    existing :: Maybe (NonEmptySet Manifest)
    existing = case entry of
      Left _ -> Nothing
      Right previous -> Just (NonEmptySet.fromFoldable1 previous)

    modified :: NonEmptySet Manifest
    modified = case existing of
      Nothing -> NonEmptySet.singleton manifest
      Just previous -> NonEmptySet.insert manifest previous

  case existing of
    Just previous | previous == modified -> pure (Right unit)
    _ -> writeEntryFile indexPath modified

-- | Given the root of a manifest index on the file system, remove the specified
-- | package version from the package entry.
removeFromEntryFile :: forall m. MonadAff m => FilePath -> PackageName -> Version -> m (Either String Unit)
removeFromEntryFile indexPath name version = do
  readEntryFile indexPath name >>= case _ of
    Left error ->
      pure $ Left error
    Right contents -> do
      let entryPath = Path.concat [ indexPath, packageEntryFilePath name ]
      case NonEmptySet.fromFoldable $ NonEmptyArray.filter (\(Manifest m) -> m.version /= version) contents of
        Nothing ->
          liftAff (Error.try (FS.Aff.unlink entryPath)) >>= case _ of
            Left error -> pure $ Left $ "Failed to delete entry:" <> Exception.message error
            Right _ -> pure $ Right unit
        Just modified ->
          writeEntryFile indexPath modified
