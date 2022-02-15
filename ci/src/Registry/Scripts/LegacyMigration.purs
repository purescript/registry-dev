module Registry.Scripts.LegacyMigration where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
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
import Registry.Scripts.LegacyImport.Error as Error
import Registry.Scripts.LegacyImport.Error as ImportError
import Registry.Scripts.LegacyImport.Process as Process
import Registry.Version as Version

main :: Effect Unit
main = Aff.launchAff_ do
  API.checkIndexExists

  index <- Index.readRegistryIndex API.indexDir
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
          httpError =
            ImportError.ResourceError
              <<< { resource: ImportError.APIResource ImportError.GitHubReleases, error: _ }
              <<< ImportError.DecodeError
              <<< Http.printError
        { body  } <- Except.ExceptT $ map (lmap httpError) (Http.get ResponseFormat.json (packagesJson release))
        packagesJson' :: PackagesJson <- Except.except $ lmap ImportError.MalformedPackageName $ Json.decode body
        liftAff (var # Process.modifyAVar (flip Array.snoc (Tuple release packagesJson')))
        pure packagesJson'
    AVar.read var

  let
    packageInRegistry (Tuple packageName { version: RawVersion version }) = do
      let errorMessage msg = "  - " <> msg
      parsedPackageName <- lmap (\_ -> errorMessage "Failed to parse package name for " <> packageName) (PackageName.parse packageName)
      --traceM $ "Looking up manifests for " <> PackageName.print parsedPackageName
      manifests <- note (errorMessage "Failed to find package " <> packageName <> " in registry index") (Map.lookup parsedPackageName index)
      --traceM $ "Found manifests for " <> PackageName.print parsedPackageName
      parsedVersion <- lmap (\_ -> errorMessage "Failed to parse version " <> version <> " for package " <> packageName) (Version.parseVersion Version.Lenient version)
      note (errorMessage "Failed to find " <> packageName <> "@" <> Version.printVersion parsedVersion <> " in registry index") (Map.lookup parsedVersion manifests)

    processedSets :: Array (Either String (Tuple Tag (Map String PackageSetEntry)))
    processedSets = packageSets # map \(Tuple release packageSet) -> do
      let
        { fail } = partitionEithers (map packageInRegistry (Map.toUnfoldable packageSet))
      if Map.member "metadata" packageSet && Array.null fail then
        Right $ Tuple release packageSet
      else do
        let metadataError = if Map.member "metadata" packageSet then "" else "  - Package set doesn't contain metadata"
        Left $ fold
          [ "Migration for package set "
          , release.name
          , " failed with errors:\n"
          , String.joinWith "\n" fail
          , if not (Array.null fail) then "\n" else ""
          , metadataError
          , "\n\n"
          ]

  log $ "Got " <> show (Array.length packageSets) <> " legacy package sets"
  for_ processedSets case _ of
    Left err -> log err
    Right (Tuple { name } packageSet) -> do
      for_ (packageSetDhall packageSet) \dhall -> do
        FS.writeTextFile UTF8 (Path.concat [ "..", "v1", "sets", name <> ".dhall" ]) dhall
        Json.writeJsonFile (Path.concat [ "..", "v1", "sets", name <> ".json" ]) packageSet

packageSetDhall :: Map String PackageSetEntry -> Maybe String
packageSetDhall packageSet = do
  { version: RawVersion version } <- Map.lookup "metadata" packageSet
  pure $ String.joinWith "\n"
    [ "\\(externalPackage : Type) ->"
    , ""
    , "let Address = (../Address.dhall) externalPackage"
    , ""
    , "let compiler = " <> version
    , ""
    , "let packages ="
    , String.joinWith "\n" packagesMap
    , "  }"
    , ""
    , "in { packages, compiler }"
    ]
  where
  printVersion (RawVersion version) = "\"v" <> Version.printVersion (unsafeFromRight (Version.parseVersion Version.Lenient version)) <> "\""

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
  , version :: RawVersion
  }
