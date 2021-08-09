module Registry.Index where

import Registry.Prelude

import Control.Alternative (guard)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Foreign.SemVer (SemVer)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Glob.Basic (expandGlobsCwd)
import Partial.Unsafe (unsafeCrashWith)
import Registry.PackageName (PackageName(..))
import Registry.Schema (Manifest)

type RegistryIndex = Map PackageName (Map SemVer Manifest)

-- This function must be run from the root of the registry index.
-- NOTE: Right now, this assumes that manifest files will parse
readRegistryIndex :: Aff RegistryIndex
readRegistryIndex = do
  packagePaths <- Array.fromFoldable <$> expandGlobsCwd [ "*" ]
  let
    -- Exclude certain files that will always be in the root of the registry index.
    -- These files do not correspond to a package manifest file.
    exclude :: Array String
    exclude = [ "config.json" ]

    goPath path = do
      fileName <- Array.last $ String.split (Pattern "/") path
      guard (not (Array.elem fileName exclude))
      pure $ PackageName fileName

    packages = Array.mapMaybe goPath packagePaths

  parsed <- for packages \package -> Tuple package <$> readPackage package
  let
    parsedPackages :: Array (Tuple PackageName (NonEmptyArray Manifest))
    parsedPackages = Array.mapMaybe (\(Tuple package mbManifests) -> Tuple package <$> mbManifests) parsed

    goManifest :: Manifest -> Tuple SemVer Manifest
    goManifest manifest@{ version } = Tuple version manifest

    goPackage :: NonEmptyArray Manifest -> Map SemVer Manifest
    goPackage = map goManifest >>> Map.fromFoldable

  pure
    $ Map.fromFoldable
    $ map (map goPackage) parsedPackages

{-
- function from PackageName -> FilePath
  Following the Cargo structure: https://github.com/rust-lang/crates.io-index
  Documentation for the format in https://doc.rust-lang.org/cargo/reference/registries.html#index-format
  Either we can have a folder for every package and then a single JSON file for every version
  or have a JSONLines (.jsonl) file for every package and a line for every version in there

  Produce the directory containing this package in the registry index,
  according to the following format:

    * Packages with 1 character names are placed in a directory named 1.
    * Packages with 2 character names are placed in a directory named 2.
    * Packages with 3 character names are placed in the directory 3/{first-character} where {first-character} is the first character of the package name.
    * All other packages are stored in directories named {first-two}/{second-two} where the top directory is the first two characters of the package name, and the next subdirectory is the third and fourth characters of the package name. For example, prelude would be stored in a file named pr/el/prelude.
-}
getIndexDir :: PackageName -> FilePath
getIndexDir (PackageName packageName) = case String.length packageName of
  0 -> unsafeCrashWith "Invalid PackageName"
  1 -> "1/"
  2 -> "2/"
  3 -> "3/" <> String.take 1 packageName <> "/"
  _ -> String.take 2 packageName <> "/" <> String.take 2 (String.drop 2 packageName) <> "/"

getIndexPath :: PackageName -> FilePath
getIndexPath (PackageName packageName) = getIndexDir (PackageName packageName) <> packageName

-- Collect all manifests for given PackageName
-- This function must be run from the root of the registry index.
-- This will return Nothing if:
--   the file doesn't exist, the file is empty, or if we can't decode the Manifests
readPackage :: PackageName -> Aff (Maybe (NonEmptyArray Manifest))
readPackage packageName = do
  let
    path = getIndexPath packageName

  contentsResult <- try do
    contents <- readTextFile ASCII path
    pure $ hush do
      jsonLines <- traverse parseJson $ String.split (Pattern "\n") contents
      traverse decodeJson jsonLines

  pure case contentsResult of
    Left _ -> Nothing
    Right Nothing -> Nothing
    Right (Just arr) -> NEA.fromArray arr

-- This function must be run from the root of the registry index.
-- TODO: Should we bail on version collisions?
insertManifest :: Manifest -> Aff Unit
insertManifest manifest@{ name, version } = do
  let
    path = getIndexPath name

  existing <- readPackage name

  let
    modifiedManifests :: NonEmptyArray Manifest
    modifiedManifests = case existing of
      Nothing -> NEA.singleton manifest
      Just manifests -> do
        case NEA.findIndex (_.version >>> eq version) manifests of
          Nothing ->
            NEA.cons manifest manifests
          Just ix ->
            fromMaybe manifests $ NEA.updateAt ix manifest manifests

    contents :: String
    contents =
      String.joinWith "\n"
        $ map (encodeJson >>> stringify)
        $ Array.sortBy (comparing _.version)
        $ Array.fromFoldable modifiedManifests

  writeTextFile ASCII path contents
