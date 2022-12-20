module Registry.Scripts.PackageTransferrer where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Ref as Ref
import Registry.App.API (AppEffects, Source(..))
import Registry.App.API as API
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.Log (LOG, LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Types (RawPackageName(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (Tag)
import Registry.Foreign.Octokit as Octokit
import Registry.Operation (AuthenticatedPackageOperation(..), PackageOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImporter as LegacyImporter
import Run (Run)
import Run.Except as Run.Except

main :: Effect Unit
main = launchAff_ do
  _ <- Env.loadEnvFile ".env"
  env <- liftEffect Env.readEnvVars

  FS.Extra.ensureDirectory API.scratchDir

  token <- case env.pacchettibottiToken of
    Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_TOKEN not defined in the .env file."
    Just token -> pure token

  octokit <- liftEffect $ Octokit.newOctokit token

  transfer # Run.interpret
    ( Run.case_
        # Run.on ?a ?b Run.send
    )
    Run.runBaseAff'

transfer :: Run AppEffects Unit
transfer = do
  Log.info "Processing legacy registry..."
  { bower, new } <- Registry.readLegacyRegistry
  let packages = Map.union bower new
  Log.info "Reading latest locations for legacy registry packages..."
  locations <- latestLocations packages
  let needsTransfer = Map.catMaybes locations
  case Map.size needsTransfer of
    0 -> Log.info "No packages require transferring."
    n -> do
      Log.info $ Array.fold [ show n, " packages need transferring." ]
      _ <- transferAll packages needsTransfer
      Log.info "Completed transfers!"

transferAll :: Map String String -> Map String PackageLocations -> Run AppEffects (Map String String)
transferAll packages packageLocations = do
  packagesRef <- liftEffect (Ref.new packages)
  forWithIndex_ packageLocations \package locations -> do
    let newPackageLocation = locations.tagLocation
    transferPackage package newPackageLocation
    let url = locationToPackageUrl newPackageLocation
    liftEffect $ Ref.modify_ (Map.insert package url) packagesRef
  liftEffect $ Ref.read packagesRef

transferPackage :: String -> Location -> Run AppEffects Unit
transferPackage rawPackageName newLocation = do
  name <- case PackageName.parse (stripPureScriptPrefix rawPackageName) of
    Left _ -> Log.exit $ "Could not transfer " <> rawPackageName <> " because it is not a valid package name."
    Right value -> pure value

  let
    payload = { name, newLocation }
    rawPayload = stringifyJson Operation.transferCodec payload

  API.runOperation Importer $ Right $ Authenticated
    { email: Env.pacchettibottiEmail
    , payload: Transfer payload
    , rawPayload
    , signature: [] -- The API will re-sign using @pacchettibotti credentials.
    }

type PackageLocations =
  { metadataLocation :: Location
  , tagLocation :: Location
  }

latestLocations :: forall r. Map String String -> Run (REGISTRY + GITHUB + LOG + LOG_EXCEPT + r) (Map String (Maybe PackageLocations))
latestLocations packages = forWithIndex packages \package location -> do
  let rawName = RawPackageName (stripPureScriptPrefix package)
  Run.Except.runExceptAt LegacyImporter._exceptPackage (LegacyImporter.validatePackage rawName location) >>= case _ of
    Left _ -> pure Nothing
    Right packageResult | Array.null packageResult.tags -> pure Nothing
    Right packageResult -> do
      Registry.readMetadata packageResult.name >>= case _ of
        Nothing -> do
          Log.error $ "No metadata exists for package " <> package
          Log.exit $ "Cannot verify location of " <> PackageName.print packageResult.name <> " because it has no metadata."
        Just metadata -> case latestPackageLocations packageResult metadata of
          Left error -> do
            Log.warn $ "Could not verify location of " <> PackageName.print packageResult.name <> ": " <> error
            pure Nothing
          Right locations
            | locationsMatch locations.metadataLocation locations.tagLocation -> pure Nothing
            | otherwise -> pure $ Just locations
  where
  -- The eq instance for locations has case sensitivity, but GitHub doesn't care.
  locationsMatch :: Location -> Location -> Boolean
  locationsMatch (GitHub location1) (GitHub location2) =
    (String.toLower location1.repo == String.toLower location2.repo)
      && (String.toLower location1.owner == String.toLower location2.owner)
  locationsMatch _ _ =
    unsafeCrashWith "Only GitHub locations can be considered in legacy registries."

latestPackageLocations :: LegacyImporter.PackageResult -> Metadata -> Either String PackageLocations
latestPackageLocations package (Metadata { location, published }) = do
  let
    isMatchingTag :: Version -> Tag -> Boolean
    isMatchingTag version tag = fromMaybe false do
      tagVersion <- hush $ LenientVersion.parse tag.name
      pure $ version == LenientVersion.version tagVersion

  matchingTag <- do
    if Map.isEmpty published then do
      note "No repo tags exist" $ Array.head package.tags
    else do
      Tuple version _ <- note "No published versions" $ Array.last (Map.toUnfoldable published)
      note "No versions match repo tags" $ Array.find (isMatchingTag version) package.tags
  tagUrl <- note ("Could not parse tag url " <> matchingTag.url) $ LegacyImporter.tagUrlToRepoUrl matchingTag.url
  let tagLocation = GitHub { owner: tagUrl.owner, repo: tagUrl.repo, subdir: Nothing }
  pure { metadataLocation: location, tagLocation }

locationToPackageUrl :: Location -> String
locationToPackageUrl = case _ of
  GitHub { owner, repo } ->
    Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
  Git _ ->
    unsafeCrashWith "Git urls cannot be registered."
