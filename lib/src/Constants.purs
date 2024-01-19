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

ignoredDirectories :: Array FilePath
ignoredDirectories =
  [ ".psci"
  , ".psci_modules"
  , ".spago"
  , "node_modules"
  , "bower_components"
  -- These files and directories are ignored by the NPM CLI and we are
  -- following their lead in ignoring them as well.
  , ".git"
  , "CVS"
  , ".svn"
  , ".hg"
  -- Additional VCS directories
  , "_darcs"
  , ".fossil"
  , ".jj"
  , ".pijul"
  ]

ignoredFiles :: Array FilePath
ignoredFiles =
  [ "package-lock.json"
  , "yarn.lock"
  , "pnpm-lock.yaml"
  ]

ignoredGlobs :: Array String
ignoredGlobs =
  [ "**/*.*.swp"
  , "**/._*"
  , "**/.DS_Store"
  ]

-- | We always include some files and directories when packaging a tarball, in
-- | addition to files users opt-in to with the 'files' key.
includedGlobs :: Array String
includedGlobs =
  [ "src/"
  , "purs.json"
  , "spago.dhall"
  , "packages.dhall"
  , "bower.json"
  , "package.json"
  , "spago.yaml"
  ]

-- | These files are always included and should be globbed in case-insensitive
-- | mode.
includedInsensitiveGlobs :: Array String
includedInsensitiveGlobs =
  [ "README*"
  , "LICENSE*"
  , "LICENCE*"
  ]
