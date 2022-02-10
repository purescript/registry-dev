module Registry.Scripts.LegacyMigration where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative (guard)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Map as Map
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Foreign.GitHub (Tag)
import Foreign.GitHub as GitHub
import Registry.API as API
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName(..))
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImport.Error as ImportError
import Registry.Scripts.LegacyImport.Process (parBounded)
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Version (Version)
import Text.Parsing.StringParser as Parser

main :: Effect Unit
main = Aff.launchAff_ do
  API.checkIndexExists

  index <- Index.readRegistryIndex "./registry-index"
  octokit <- liftEffect GitHub.mkOctokit

  let
    packageSetsAddress = { owner: "purescript", repo: "package-sets" }
    repoCache = Array.fold [ "releases__", packageSetsAddress.owner, "__", packageSetsAddress.repo ]
    packageSetsCache name = Array.fold [ "package-sets__", name ]

  eitherReleases <- Except.runExceptT $ Process.withCache Process.jsonSerializer repoCache Nothing do
    log $ "Fetching releases for registry"
    liftAff $ GitHub.getReleases octokit packageSetsAddress

  (releases :: Array Tag) <- case eitherReleases of
    Left err -> ?a err
    Right releases -> pure releases

  let
    packagesJson { name } =
      "https://raw.githubusercontent.com/purescript/package-sets/" <> name <> "/packages.json"

  packageSets <- do
    var <- AVar.new []
    Process.parBounded releases \_ release -> Except.runExceptT $ Except.withExceptT (ImportError.printImportErrorKey) do
      Process.withCache Process.jsonSerializer (packageSetsCache release.name) Nothing do
        let
          decodeError =
            ImportError.ResourceError
              <<< { resource: ImportError.APIResource ImportError.GitHubReleases, error: _ }
              <<< ImportError.DecodeError
          httpError =
            decodeError <<< Http.printError
        { body  } <- Except.ExceptT $ map (lmap httpError) (Http.get ResponseFormat.json (packagesJson release))
        packagesJson <- Except.except $ lmap ImportError.MalformedPackageName $ traverseKeys (lmap Parser.printParserError <<< PackageName.parse) =<< Json.decode body
        liftAff (var # Process.modifyAVar (flip Array.snoc packagesJson))
        pure packagesJson
    AVar.read var

  let
    -- try to go from RawPackageName -> PackageName, return Nothing if it doesn't parse
    packageInRegistry (Tuple packageName { version }) =
      isJust (Map.lookup packageName index >>= Map.lookup version)

    validSets = packageSets # Array.mapMaybe \packageSet -> do
      guard (Array.all packageInRegistry (Map.toUnfoldable packageSet))
      pure packageSet

  -- Compiler version under "metadata"

  pure unit



    -- migrate to new format




-- TODO: Use RawPackageName
type PackagesJson = Map PackageName PackageSetEntry

type PackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: Http.URL
  , version :: Version
  }
