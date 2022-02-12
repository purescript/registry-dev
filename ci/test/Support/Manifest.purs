module Test.Support.Manifest where

import Registry.Prelude

import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Target(..))
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
  targets = Object.singleton "lib" $ Target
    { dependencies: Object.empty
    , sources: [ "src/**/*.purs" ]
    }
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  license = unsafeFromRight $ SPDX.parse "MIT"
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
  description = Just "some description"
  v1a = Manifest { name, owners: Nothing, version: version1, license, repository: repositoryWrong, targets, description }
  v1b = Manifest { name, owners: Nothing, version: version1, license, repository, targets, description }
  v2 = Manifest { name, owners: Nothing, version: version2, license, repository, targets, description }

abc :: { name :: PackageName, v1 :: Manifest, v2 :: Manifest }
abc = { name, v1, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "abc"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  targets1 = Object.singleton "lib" $ Target
    { dependencies: Object.singleton "ab" (unsafeFromRight (Version.parseRange Strict ">=1.0.0 <2.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  targets2 = Object.singleton "lib" $ Target
    { dependencies: Object.singleton "ab" (unsafeFromRight (Version.parseRange Strict ">=2.0.0 <3.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  license = unsafeFromRight $ SPDX.parse "MIT"
  repository = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  description = Just "some description"
  v1 = Manifest { name, owners: Nothing, version: version1, license, repository, targets: targets1, description }
  v2 = Manifest { name, owners: Nothing, version: version2, license, repository, targets: targets2, description }

abcd :: { name :: PackageName, v1 :: Manifest, v2 :: Manifest }
abcd = { name, v1, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "abcd"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  targets1 = Object.singleton "lib" $ Target
    { dependencies: Object.singleton "abc" (unsafeFromRight (Version.parseRange Strict ">=1.0.0 <2.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  targets2 = Object.singleton "lib" $ Target
    { dependencies: Object.singleton "abc" (unsafeFromRight (Version.parseRange Strict ">=2.0.0 <3.0.0"))
    , sources: [ "src/**/*.purs" ]
    }
  license = unsafeFromRight $ SPDX.parse "MIT"
  repository = Schema.GitHub
    { owner: "abcd-user"
    , repo: "abcd"
    , subdir: Nothing
    }
  description = Just "some description"
  v1 = Manifest { name, owners: Nothing, version: version1, license, repository, targets: targets1, description }
  v2 = Manifest { name, owners: Nothing, version: version2, license, repository, targets: targets2, description }
