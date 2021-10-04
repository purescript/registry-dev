module Test.Registry.Index where

import Registry.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Effect.Exception as Exception
import Foreign.Tmp as Tmp
import Node.FS.Stats as Stats
import Node.Glob.Basic as Glob
import Node.Path as Node.Path
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)
import Test.Support.Manifest as Support.Manifest

testRegistryIndex :: Aff Unit
testRegistryIndex = do
  tmp <- liftEffect Tmp.mkTmpDir
  initialIndex <- Index.readRegistryIndex tmp
  unless (initialIndex == emptyIndex) do
    throwError (Exception.error "Initial index is not empty.")
  _ <- testPipeline tmp initialIndex
  packagePaths <- Glob.expandGlobsWithStats tmp [ "**/*" ]
  let
    existingFiles = Map.keys $ Map.filter (not Stats.isDirectory) packagePaths
  unless (existingFiles == expectedFiles tmp) do
    throwError (Exception.error "Directory didn't match expected structure.")
  pure unit
  where
  testPipeline tmp =
    insertAndCheck tmp Support.Manifest.ab.name Support.Manifest.ab.v1a
      -- Version collision should overwrite existing entry
      >=> insertAndCheck tmp Support.Manifest.ab.name Support.Manifest.ab.v1b
      >=> insertAndCheck tmp Support.Manifest.ab.name Support.Manifest.ab.v2
      >=> insertAndCheck tmp Support.Manifest.abc.name Support.Manifest.abc.v1
      >=> insertAndCheck tmp Support.Manifest.abc.name Support.Manifest.abc.v2
      >=> insertAndCheck tmp Support.Manifest.abcd.name Support.Manifest.abcd.v1
      >=> insertAndCheck tmp Support.Manifest.abcd.name Support.Manifest.abcd.v2

emptyIndex :: RegistryIndex
emptyIndex = Map.empty

expectedFiles :: FilePath -> Set FilePath
expectedFiles tmp = Set.fromFoldable $ map (Node.Path.concat <<< Array.cons tmp)
  [ [ "2", "ab" ]
  , [ "3", "a", "abc" ]
  , [ "ab", "cd", "abcd" ]
  ]

insertAndCheck :: FilePath -> PackageName -> Manifest -> RegistryIndex -> Aff RegistryIndex
insertAndCheck registryDirectory packageName manifest existingRegistry = do
  -- Insert into registry index file & in memory
  -- read file
  -- ensure read index matches in memory index
  Index.insertManifest registryDirectory manifest
  registry <- Index.readRegistryIndex registryDirectory

  let
    -- Prefer later entries in the case of version collisions.
    updatedRegistry =
      Map.insertWith (flip Map.union) packageName (Map.singleton manifest.version manifest) existingRegistry

  if updatedRegistry /= registry then do
    let
      errorMessage = Array.intercalate " "
        [ "Failed to insert"
        , PackageName.print packageName
        , "version"
        , show manifest.version
        , "to registry index."
        ]

    throwError (Exception.error errorMessage)
  else
    pure updatedRegistry
