module Registry.Constants where

import Node.Path (FilePath)

type GitHubRepo = { owner :: String, repo :: String }

-- | The location of the registry GitHub repository
registry :: GitHubRepo
registry = { owner: "purescript", repo: "registry" }

-- | The file path to the package sets within the registry GitHub repository
packageSetsDirectory :: FilePath
packageSetsDirectory = "package-sets"

-- | The file path to the package metadata within the registry GitHub repository
packageMetadataDirectory :: FilePath
packageMetadataDirectory = "metadata"

-- | The location of the package index GitHub repository
packageIndex :: GitHubRepo
packageIndex = { owner: "purescript", repo: "registry-index" }

-- | The URL of the package storage backend
packageStorageUrl :: String
packageStorageUrl = "https://packages.registry.purescript.org"

-- | The URL of the package operation API
packageApiUrl :: String
packageApiUrl = "https://registry.purescript.org/api"
