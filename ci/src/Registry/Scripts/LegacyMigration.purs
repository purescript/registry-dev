module Registry.Scripts.LegacyMigration where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative (guard)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Map as Map
import Data.Time.Duration (Hours(..))
import Effect.AVar as AVar
import Effect.Aff as Aff
import Foreign.GitHub as GitHub
import Registry.API as API
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.Scripts.LegacyImport.Process (parBounded)
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Version (Version)

main :: Effect Unit
main = Aff.launchAff_ do
  API.checkIndexExists

  index <- Index.readRegistryIndex "./registry-index"
  octokit <- liftEffect GitHub.mkOctokit

  let
    packageSetsAddress = { owner: "purescript", repo: "package-sets" }
    repoCache = Array.fold [ "releases__", packageSetsAddress.owner, "__", packageSetsAddress.repo ]

  releases <- Process.withCache Process.jsonSerializer repoCache Nothing do
    log $ "Fetching releases for registry"
    lift $ try $ GitHub.getReleases octokit packageSetsAddress

  let
    packagesJson { name } =
      "https://raw.githubusercontent.com/purescript/package-sets/" <> name <> "/packages.json"

  packageSets <- do
    var <- AVar.new []
    Process.parBounded releases \_ release -> do
      Process.withCache Process.jsonSerializer Nothing do
        json <- map (lmap Aff.message) (Http.get ResponseFormat.json (packagesJson release))
        packagesJson <- Except.except (traverseKeys PackageName.parse =<< Json.decode json)
        var # Process.modifyAVar (flip Array.snoc packagesJson)
    AVar.read var

  let
    -- TODO: We probably need to check the dependencies for a manifest
    -- We have a list of dependencies, but how do we guarantee that these
    -- dependencies match the dependencies listed in the manifest
    -- for this package@version?
    -- They may not match - do we care?
    packageInRegistry (Tuple packageName { version }) =
      isJust (Map.lookup packageName index >>= Map.lookup version)

    validSets = packageSets # Array.mapMaybe \packageSet -> do
      guard (Array.all packageInRegistry (Map.toUnfoldable packagesJson))
      pure packageSet

  pure unit



    -- migrate to new format




type PackagesJson = Map PackageName PackageSetEntry

type PackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: Http.URL
  , version :: Version
  }
