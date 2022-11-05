module Registry.Constants where

import Data.Maybe (Maybe(..))
import Node.Path (FilePath)
import Registry.Location (GitHubData)

-- | The location of the registry GitHub repository
registry :: GitHubData
registry = { owner: "purescript", repo: "registry", subdir: Nothing }

-- | The location of the package sets within the registry GitHub repository
packageSets :: GitHubData
packageSets = registry { subdir = Just packageSetsDirectory }

-- | The file path to the package sets within the registry GitHub repository
packageSetsDirectory :: FilePath
packageSetsDirectory = "package-sets"

-- | The location of the package metadata within the registry GitHub repository
packageMetadata :: GitHubData
packageMetadata = registry { subdir = Just packageMetadataDirectory }

-- | The file path to the package metadata within the registry GitHub repository
packageMetadataDirectory :: FilePath
packageMetadataDirectory = "metadata"

-- | The location of the package index GitHub repository
packageIndex :: GitHubData
packageIndex = { owner: "purescript", repo: "registry-index", subdir: Nothing }

-- | The URL of the package storage backend
packageStorage :: String
packageStorage = "https://packages.registry.purescript.org"

-- | The URL of the package operation API
packageApi :: String
packageApi = "https://registry.purescript.org/api"
