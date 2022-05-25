module Registry.Index
  ( RegistryIndex
  , readRegistryIndex
  , insertManifest
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Foreign.FastGlob (Include(..))
import Foreign.FastGlob as FastGlob
import Foreign.Node.FS as FS.Extra
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..))
import Registry.Version (Version)

type RegistryIndex = Map PackageName (Map Version Manifest)

-- | NOTE: Right now, this assumes that manifest files will parse
readRegistryIndex :: FilePath -> Aff RegistryIndex
readRegistryIndex directory = do
  packagePaths <- FastGlob.match' directory [ "**/*" ] { include: FilesOnly, ignore: [ "config.json" ] }

  let packages = Array.mapMaybe (hush <<< PackageName.parse <<< Path.basename) packagePaths.succeeded

  parsed <- for packages \package ->
    Tuple package <$> readPackage directory package

  let
    normalizePackage
      :: Tuple PackageName (Maybe (NonEmptyArray Manifest))
      -> Tuple PackageName (NonEmptyArray Manifest)
    normalizePackage (Tuple package mbManifests) = case mbManifests of
      Nothing -> unsafeCrashWith ("Package " <> PackageName.print package <> " failed to parse")
      Just manifests -> Tuple package manifests

    parsedPackages :: Array (Tuple PackageName (NonEmptyArray Manifest))
    parsedPackages = map normalizePackage parsed

    goManifest :: Manifest -> Tuple Version Manifest
    goManifest manifest@(Manifest { version }) = Tuple version manifest

    goPackage :: NonEmptyArray Manifest -> Map Version Manifest
    goPackage = map goManifest >>> Map.fromFoldable

  pure
    $ Map.fromFoldable
    $ map (map goPackage) parsedPackages

-- | Produce the directory containing this package in the registry index, using the following format:
-- |   * Packages with 1 character names are placed in a directory named 1.
-- |   * Packages with 2 character names are placed in a directory named 2.
-- |   * Packages with 3 character names are placed in the directory 3/{first-character} where {first-character} is the first character of the package name.
-- |   * All other packages are stored in directories named {first-two}/{second-two} where the top directory is the first two characters of the package name, and the next subdirectory is the third and fourth characters of the package name. For example, prelude would be stored in a file named pr/el/prelude.
-- |   * Each package file is a JSON Lines file where each line is a package manifest, stored in sorted order ascending by version.
-- |
-- | Format follows that used by Cargo in crates.io: https://github.com/rust-lang/crates.io-index
-- | As documented in the Cargo book: https://doc.rust-lang.org/cargo/reference/registries.html#index-format
getIndexDir :: PackageName -> FilePath
getIndexDir = PackageName.print >>> \packageName -> case String.length packageName of
  0 -> unsafeCrashWith "Invalid PackageName"
  1 -> "1/"
  2 -> "2/"
  3 -> "3/" <> String.take 1 packageName <> "/"
  _ -> String.take 2 packageName <> "/" <> String.take 2 (String.drop 2 packageName) <> "/"

getIndexPath :: PackageName -> FilePath
getIndexPath packageName = getIndexDir packageName <> PackageName.print packageName

-- | Collect all manifests for given PackageName
-- | This function must be run from the root of the registry index.
-- | This will return Nothing if:
-- |  the file doesn't exist, the file is empty, or if we can't decode the Manifests
readPackage :: FilePath -> PackageName -> Aff (Maybe (NonEmptyArray Manifest))
readPackage directory packageName = do
  let path = Path.concat [ directory, getIndexPath packageName ]

  contentsResult <- try do
    contents <- FS.readTextFile ASCII path
    pure $ hush $ traverse Json.parseJson $ String.split (Pattern "\n") contents

  pure case contentsResult of
    Left _ -> Nothing
    Right Nothing -> Nothing
    Right (Just arr) -> NEA.fromArray arr

insertManifest :: FilePath -> Manifest -> Aff Unit
insertManifest directory manifest@(Manifest { name, version }) = do
  let
    manifestPath = Path.concat [ directory, getIndexPath name ]
    manifestDirectory = Path.dirname manifestPath

  existingManifest <- readPackage directory name

  let
    modifiedManifests :: NonEmptyArray Manifest
    modifiedManifests = case existingManifest of
      Nothing -> NEA.singleton manifest
      Just manifests -> do
        case NEA.findIndex (un Manifest >>> _.version >>> eq version) manifests of
          Nothing ->
            NEA.cons manifest manifests
          Just ix ->
            fromMaybe manifests $ NEA.updateAt ix manifest manifests

    contents :: String
    contents =
      String.joinWith "\n"
        $ map (Json.encode >>> Json.stringify)
        $ Array.sortBy (comparing (un Manifest >>> _.version))
        $ Array.fromFoldable modifiedManifests

  FS.Extra.ensureDirectory manifestDirectory
  FS.writeTextFile ASCII manifestPath contents
