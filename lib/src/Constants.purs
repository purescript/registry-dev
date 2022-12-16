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
metadataDirectory :: FilePath
metadataDirectory = "metadata"

-- | The location of the package index GitHub repository
manifestIndex :: GitHubRepo
manifestIndex = { owner: "purescript", repo: "registry-index" }

-- | The URL of the package storage backend
storageUrl :: String
storageUrl = "https://packages.registry.purescript.org"

-- | The URL of the package operation API
apiUrl :: String
apiUrl = "https://registry.purescript.org/api"
