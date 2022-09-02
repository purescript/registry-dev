module Registry.Constants
  ( legacyPackageSetsRepo
  , metadataPath
  , packageSetsPath
  , registryDevRepo
  , registryIndexRepo
  , registryPackagesUrl
  , registryRepo
  ) where

import Affjax as Http
import Node.Path (FilePath)

type Repository = { owner :: String, repo :: String }

registryRepo :: Repository
registryRepo = { owner: "purescript", repo: "registry-preview" }

packageSetsPath :: FilePath
packageSetsPath = "package-sets"

metadataPath :: FilePath
metadataPath = "metadata"

registryDevRepo :: Repository
registryDevRepo = { owner: "purescript", repo: "registry" }

registryIndexRepo :: Repository
registryIndexRepo = { owner: "purescript", repo: "registry-index" }

legacyPackageSetsRepo :: Repository
legacyPackageSetsRepo = { owner: "purescript", repo: "package-sets" }

registryPackagesUrl :: Http.URL
registryPackagesUrl = "https://packages.registry.purescript.org"
