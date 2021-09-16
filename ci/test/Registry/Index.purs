module Test.Index where

import Registry.Prelude

import Data.Array as Array
import Data.Map as Map
import Effect.Exception as Exception
import Node.FS.Aff as FS
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)
import Test.Fixtures.Manifest as Fixtures

-- TODO
-- 1. Create a temporary directory to hold our registry index (pass in to writeIndex)
-- 2. Read index, should be empty
-- 3. Write each manifest to index
-- 4. Read index, should be `testIndex`
-- 5. Filesystem sanity check i.e. "two/c3", "wi/re/wire", "wi/th/with-index"

-- Corner cases to consider
-- 1. Version collision should overwrite
-- 2. two and three letters packages + 1 four+ letter

-- Make dummy packages

testRegistryIndex :: Aff Unit
testRegistryIndex = do
  FS.mkdir registryDirectory
  initialIndex <- Index.readRegistryIndex registryDirectory
  unless (initialIndex == emptyIndex) do
    error "Initial index is not empty."
  _ <- testPipeline initialIndex
  pure unit
  where
  testPipeline =
    insertAndCheck Fixtures.ab.name Fixtures.ab.v1a
      >=> insertAndCheck Fixtures.ab.name Fixtures.ab.v1b
      >=> insertAndCheck Fixtures.ab.name Fixtures.ab.v2
      >=> insertAndCheck Fixtures.abc.name Fixtures.abc.v1
      >=> insertAndCheck Fixtures.abc.name Fixtures.abc.v2
      >=> insertAndCheck Fixtures.abcd.name Fixtures.abcd.v1
      >=> insertAndCheck Fixtures.abcd.name Fixtures.abcd.v2

emptyIndex :: RegistryIndex
emptyIndex = Map.empty

registryDirectory :: FilePath
registryDirectory = "registry-index-test"

insertAndCheck :: PackageName -> Manifest -> RegistryIndex -> Aff RegistryIndex
insertAndCheck packageName manifest existingRegistry = do
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
