module Test.Support.Manifest where

import Registry.Prelude

import Data.Map as Map
import Foreign.SPDX as SPDX
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..))
import Registry.Schema as Schema
import Registry.Version (ParseMode(..))
import Registry.Version as Version

ab ::
  { name :: PackageName
  , v1a :: Manifest
  , v1b :: Manifest
  , v2 :: Manifest
  }
ab = { name, v1a, v1b, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "ab"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  dependencies = Map.empty
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  license = unsafeFromRight $ SPDX.parse "MIT"
  locationWrong = Schema.GitHub
    { owner: "ab-wrong-user"
    , repo: "ab"
    , subdir: Nothing
    }
  location = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  description = Just "some description"
  v1a = Manifest { name, version: version1, license, location: locationWrong, dependencies, description, files: Nothing }
  v1b = Manifest { name, version: version1, license, location, dependencies, description, files: Nothing }
  v2 = Manifest { name, version: version2, license, location, dependencies, description, files: Nothing }

abc :: { name :: PackageName, v1 :: Manifest, v2 :: Manifest }
abc = { name, v1, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "abc"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  dependencies1 = Map.singleton (unsafeFromRight (PackageName.parse "ab")) (unsafeFromRight (Version.parseRange Strict ">=1.0.0 <2.0.0"))
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  dependencies2 = Map.singleton (unsafeFromRight (PackageName.parse "ab")) (unsafeFromRight (Version.parseRange Strict ">=2.0.0 <3.0.0"))
  license = unsafeFromRight $ SPDX.parse "MIT"
  location = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  description = Just "some description"
  v1 = Manifest { name, version: version1, license, location, dependencies: dependencies1, description, files: Nothing }
  v2 = Manifest { name, version: version2, license, location, dependencies: dependencies2, description, files: Nothing }

abcd :: { name :: PackageName, v1 :: Manifest, v2 :: Manifest }
abcd = { name, v1, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "abcd"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  dependencies1 = Map.singleton (unsafeFromRight (PackageName.parse "abc")) (unsafeFromRight (Version.parseRange Strict ">=1.0.0 <2.0.0"))
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  dependencies2 = Map.singleton (unsafeFromRight (PackageName.parse "abc")) (unsafeFromRight (Version.parseRange Strict ">=2.0.0 <3.0.0"))
  license = unsafeFromRight $ SPDX.parse "MIT"
  location = Schema.GitHub
    { owner: "abcd-user"
    , repo: "abcd"
    , subdir: Nothing
    }
  description = Just "some description"
  v1 = Manifest { name, version: version1, license, location, dependencies: dependencies1, description, files: Nothing }
  v2 = Manifest { name, version: version2, license, location, dependencies: dependencies2, description, files: Nothing }
