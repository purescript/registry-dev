module Registry.Scripts.PackageTransferrer where

import Registry.Prelude

import Affjax as Http
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Dotenv as Dotenv
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Node.Path as Path
import Node.Process as Node.Process
import Registry.API as API
import Registry.Cache as Cache
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM, readPackagesMetadata, throwWithComment)
import Registry.RegistryM as RegistryM
import Registry.Schema (Location(..), Metadata)
import Registry.Scripts.LegacyImporter as LegacyImporter
import Registry.Version (Version)
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile

  octokit <- liftEffect do
    token <- do
      result <- Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
      maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) result
    GitHub.mkOctokit token

  cache <- Cache.useCache

  let
    env :: RegistryM.Env
    env =
      { comment: \comment -> log ("[COMMENT] " <> comment)
      , closeIssue: log "Running locally, not closing issue..."
      , commitMetadataFile: API.pacchettiBottiPushToRegistryMetadata
      , commitIndexFile: \_ _ -> unsafeCrashWith "Should not push to registry index in transfer."
      , commitPackageSetFile: \_ _ -> unsafeCrashWith "Should not modify package set in transfer."
      , uploadPackage: \_ -> unsafeCrashWith "Should not upload anything in transfer."
      , deletePackage: \_ -> unsafeCrashWith "Should not delete anything in transfer."
      , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
      , cache
      , octokit
      , registry: Path.concat [ "..", "registry" ]
      , registryIndex: Path.concat [ "..", "registry-index" ]
      }

  RegistryM.runRegistryM env do
    API.fetchRegistry
    API.fillMetadataRef

    bowerPackages <- liftAff $ LegacyImporter.readLegacyRegistryFile "bower-packages.json"
    newPackages <- liftAff $ LegacyImporter.readLegacyRegistryFile "new-packages.json"

    bowerPackagesTransfers <- liftEffect $ Ref.new Map.empty
    newPackagesTransfers <- liftEffect $ Ref.new Map.empty

    bowerLocations <- forWithIndex bowerPackages \package location -> do
      let rawName = RawPackageName (stripPureScriptPrefix package)
      Except.runExceptT (LegacyImporter.validatePackage rawName location) >>= case _ of
        Left _ -> pure Nothing
        Right packageResult -> do
          packagesMetadata <- readPackagesMetadata
          case Map.lookup packageResult.name packagesMetadata of
            Nothing -> throwWithComment $ "No metadata exists for package " <> package
            Just metadata | Map.isEmpty metadata.published -> pure Nothing
            Just metadata -> do
              Except.runExceptT (findLatestLocations packageResult metadata) >>= case _ of
                Left err -> log err *> pure Nothing
                Right locations
                  | locations.metadataLocation == locations.tagLocation -> pure Nothing
                  | otherwise -> pure $ Just { package, location: locations.tagLocation }

    let toProcess = Map.catMaybes bowerLocations

    logShow toProcess

    -- find the most recent tag in the metadata, look it up in the tags
    -- if their locations are different, then it needs to be transferred
    -- run the 'transfer' operation
    -- write the change to the correct legacy file

    pure unit

type PackageLocations =
  { metadataLocation :: Location
  , tagLocation :: Location
  }

findLatestLocations :: LegacyImporter.PackageResult -> Metadata -> ExceptT String RegistryM PackageLocations
findLatestLocations package { location, published } = do
  let
    isMatchingTag :: Version -> GitHub.Tag -> Boolean
    isMatchingTag version tag = fromMaybe false do
      tagVersion <- hush $ Version.parseVersion Version.Lenient tag.name
      pure $ version == tagVersion

    matchMetadata :: Either String PackageLocations
    matchMetadata = do
      Tuple version _ <- note "No published versions" $ Array.last (Map.toUnfoldable published)
      matchingTag <- note "No versions match repo tags" $ Array.find (isMatchingTag version) package.tags
      tagUrl <- note ("Could not parse tag url " <> matchingTag.url) $ tagUrlToRepoUrl matchingTag.url
      let tagLocation = GitHub { owner: tagUrl.owner, repo: tagUrl.repo, subdir: Nothing }
      pure { metadataLocation: location, tagLocation }

  case matchMetadata of
    Left err -> throwError $ Array.fold
      [ PackageName.print package.name
      , " failed to match locations: "
      , err
      ]
    Right locations ->
      pure locations

-- Example tag url:
-- https://api.github.com/repos/octocat/Hello-World/commits/c5b97d5ae6c19d5c5df71a34c7fbeeda2479ccbc
tagUrlToRepoUrl :: Http.URL -> Maybe GitHub.Address
tagUrlToRepoUrl url = do
  noPrefix <- String.stripPrefix (String.Pattern "https://api.github.com/repos/") url
  let getOwnerRepoArray = Array.take 2 <<< String.split (String.Pattern "/")
  case getOwnerRepoArray noPrefix of
    [ owner, repo ] -> Just { owner, repo }
    _ -> Nothing
