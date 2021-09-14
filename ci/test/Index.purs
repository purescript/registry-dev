module Test.Index where

import Registry.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe as Maybe
import Effect.Aff (bracket)
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Partial.Unsafe as Partial.Unsafe
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)
import Registry.Schema as Schema

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
    insertAndCheck ab.name ab.v1a
      >=> insertAndCheck ab.name ab.v1b
      >=> insertAndCheck ab.name ab.v2
      >=> insertAndCheck abc.name abc.v1
      >=> insertAndCheck abc.name abc.v2
      >=> insertAndCheck abcd.name abcd.v1
      >=> insertAndCheck abcd.name abcd.v2

emptyIndex :: RegistryIndex
emptyIndex = Map.empty

registryDirectory :: FilePath
registryDirectory = "registry-index-test"

type TestManifestEntry =
  { version :: SemVer
  , manifest :: Manifest
  }

insertAndCheck :: PackageName -> TestManifestEntry -> RegistryIndex -> Aff RegistryIndex
insertAndCheck packageName { version, manifest } existingRegistry = do
  -- Insert into registry index file & in memory
  -- read file
  -- ensure read index matches in memory index
  Index.insertManifest registryDirectory manifest
  let
    -- Prefer later entries in the case of version collisions.
    updatedRegistry =
      Map.insertWith (flip Map.union) packageName (Map.singleton version manifest) existingRegistry

  registry <- Index.readRegistryIndex registryDirectory

  unless (updatedRegistry == registry) do
    let
      errorMessage = Array.intercalate " "
        [ "Failed to insert"
        , PackageName.print packageName
        , "version"
        , show version
        , "to registry index."
        ]

    error errorMessage

  pure updatedRegistry

ab ::
  { name :: PackageName
  , v1a :: TestManifestEntry
  , v1b :: TestManifestEntry
  , v2 :: TestManifestEntry
  }
ab = { name, v1a, v1b, v2 }
  where
  name = unsafeFromJust $ hush $ PackageName.parse "ab"
  version1 = unsafeFromJust $ SemVer.parseSemVer "1.0.0"
  targets = Object.singleton "lib"
    { dependencies: Object.empty
    , sources: [ "src/**/*.purs" ]
    }
  version2 = unsafeFromJust $ SemVer.parseSemVer "2.0.0"
  license = unsafeFromJust $ hush $ SPDX.parse "MIT"
  repositoryWrong = Schema.GitHub
    { owner: "ab-wrong-user"
    , repo: "ab"
    , subdir: Nothing
    }
  repository = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  v1a =
    { version: version1
    , manifest: { name, version: version1, license, repository: repositoryWrong, targets }
    }
  v1b = do
    { version: version1
    , manifest: { name, version: version2, license, repository, targets }
    }
  v2 = do
    { version: version2
    , manifest: { name, version: version2, license, repository, targets }
    }

abc :: { name :: PackageName, v1 :: TestManifestEntry, v2 :: TestManifestEntry }
abc = { name, v1, v2}
  where
  name = unsafeFromJust $ hush $ PackageName.parse "abc"
  version1 = unsafeFromJust $ SemVer.parseSemVer "1.0.0"
  targets1 = Object.singleton "lib"
    { dependencies: Object.singleton "ab" (unsafeFromJust (SemVer.parseRange "^1.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  version2 = unsafeFromJust $ SemVer.parseSemVer "2.0.0"
  targets2 = Object.singleton "lib"
    { dependencies: Object.singleton "ab" (unsafeFromJust (SemVer.parseRange "^2.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  license = unsafeFromJust $ hush $ SPDX.parse "MIT"
  repository = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  v1 =
    { version: version1
    , manifest: { name, version: version1, license, repository, targets: targets1 }
    }
  v2 = do
    { version: version2
    , manifest: { name, version: version2, license, repository, targets: targets2 }
    }

abcd :: { name :: PackageName, v1 :: TestManifestEntry, v2 :: TestManifestEntry }
abcd = { name, v1, v2 }
  where
  name = unsafeFromJust $ hush $ PackageName.parse "abcd"
  version1 = unsafeFromJust $ SemVer.parseSemVer "1.0.0"
  targets1 = Object.singleton "lib"
    { dependencies: Object.singleton "abc" (unsafeFromJust (SemVer.parseRange "^1.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  version2 = unsafeFromJust $ SemVer.parseSemVer "2.0.0"
  targets2 = Object.singleton "lib"
    { dependencies: Object.singleton "abc" (unsafeFromJust (SemVer.parseRange "^2.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  license = unsafeFromJust $ hush $ SPDX.parse "MIT"
  repository = Schema.GitHub
    { owner: "abcd-user"
    , repo: "abcd"
    , subdir: Nothing
    }
  v1 =
    { version: version1
    , manifest: { name, version: version1, license, repository, targets: targets1 }
    }
  v2 = do
    { version: version2
    , manifest: { name, version: version2, license, repository, targets: targets2 }
    }

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = Partial.Unsafe.unsafePartial Maybe.fromJust
