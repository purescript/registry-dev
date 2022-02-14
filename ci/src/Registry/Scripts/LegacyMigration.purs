module Registry.Scripts.LegacyMigration where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Control.Alternative (guard)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Foreign.GitHub (Tag)
import Foreign.GitHub as GitHub
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.API as API
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImport.Error (ImportError, RequestError)
import Registry.Scripts.LegacyImport.Error as Error
import Registry.Scripts.LegacyImport.Error as ImportError
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Version (Version)
import Registry.Version as Version

-- question: where do we write the migrated package sets?

-- TODO: remove this?
releasesError :: RequestError -> ImportError
releasesError =
  ImportError.ResourceError
    <<< { resource: ImportError.APIResource ImportError.GitHubReleases, error: _ }

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
    Left err -> throwError (Aff.error (show (Error.printImportErrorKey err)))
    Right releases -> pure releases

  let
    packagesJson { name } =
      "https://raw.githubusercontent.com/purescript/package-sets/" <> name <> "/packages.json"

  (packageSets :: Array (Tuple Tag PackagesJson)) <- do
    var <- AVar.new []
    Process.parBounded releases \_ release -> Except.runExceptT $ Except.withExceptT (ImportError.printImportErrorKey) do
      Process.withCache Process.jsonSerializer (packageSetsCache release.name) Nothing do
        let
          httpError = releasesError <<< ImportError.DecodeError <<< Http.printError
        { body  } <- Except.ExceptT $ map (lmap httpError) (Http.get ResponseFormat.json (packagesJson release))
        packagesJson' :: PackagesJson <- Except.except $ lmap ImportError.MalformedPackageName $ Json.decode body
        liftAff (var # Process.modifyAVar (flip Array.snoc (Tuple release packagesJson')))
        pure packagesJson'
    AVar.read var

  let
    packageInRegistry (Tuple packageName { version }) = isJust do
      parsedPackageName <- hush (PackageName.parse packageName)
      manifests <- Map.lookup parsedPackageName index
      Map.lookup version manifests

    validSets :: Array (Tuple Tag (Map String PackageSetEntry))
    validSets = packageSets # Array.mapMaybe \(Tuple release packageSet) -> do
      guard (Array.all packageInRegistry (Map.toUnfoldable packageSet))
      pure $ Tuple release packageSet

  log $ "Got " <> show (Array.length packageSets) <> " package sets"
  log $ "Writing " <> show (Array.length validSets) <> " package sets"

  for_ validSets \(Tuple { name } packageSet) -> do
    FS.writeTextFile UTF8 (Path.concat [ "v1", "sets", name <> ".dhall" ]) (packageSetDhall packageSet)
    Json.writeJsonFile (Path.concat [ "v1", "sets", name <> ".json" ]) packageSet

packageSetDhall :: Map String PackageSetEntry -> String
packageSetDhall packageSet =
  String.joinWith "\n"
    [ "\\(externalPackage : Type) ->"
    , ""
    , "let Address = (../Address.dhall) externalPackage"
    , ""
    , "let compiler = " <> compilerVersion
    , ""
    , "let packages ="
    , String.joinWith "\n" packagesMap
    , "  }"
    , ""
    , "in { packages, compiler }"
    ]
  where
  compilerVersion = fromJust' (\_ -> unsafeCrashWith "Failed to find metadata package") do
    { version } <- Map.lookup "metadata" packageSet
    pure $ printVersion version

  printVersion version = "\"v" <> Version.printVersion version <> "\""

  packagesMap = Array.mapWithIndex printPackageVersion (Map.toUnfoldable packageSet)

  printPackageVersion ix (Tuple packageName { version }) = do
    String.joinWith " "
      [ if ix == 0 then "  {" else "  ,"
      , if packageName == "assert" then "`assert`" else packageName
      , "= Address.Registry"
      , printVersion version
      ]

type PackagesJson = Map String PackageSetEntry

type PackageSetEntry =
  { dependencies :: Array RawPackageName
  , repo :: Http.URL
  , version :: Version
  }
