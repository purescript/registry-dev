module Registry.API where

import Registry.Prelude

import Affjax as Http
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except as Except
import Data.Argonaut.Parser as Argonaut.Core
import Data.Argonaut.Parser as JsonParser
import Data.Array as Array
import Data.DateTime as DateTime
import Data.Foldable (traverse_)
import Data.Generic.Rep as Generic
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Interpolate (i)
import Data.JSDate as JSDate
import Data.Map as Map
import Data.MediaType.Common as MediaType
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String)
import Data.RFC3339String as RFC3339String
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Effect.Aff as Aff
import Effect.Exception (throw)
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.Dhall as Dhall
import Foreign.FastGlob as FastGlob
import Foreign.GitHub (IssueNumber)
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Node.Path as Path
import Node.Process as Env
import Node.Process as Node.Process
import Registry.Hash as Hash
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageUpload as Upload
import Registry.RegistryM (Env, RegistryM, closeIssue, comment, commitToTrunk, deletePackage, readPackagesMetadata, runRegistryM, throwWithComment, updatePackagesMetadata, uploadPackage)
import Registry.SSH as SSH
import Registry.Schema (AuthenticatedData(..), AuthenticatedOperation(..), BuildPlan(..), Location(..), Manifest(..), Metadata, Operation(..), UpdateData, addVersionToMetadata, isVersionInMetadata, mkNewMetadata, unpublishVersionInMetadata)
import Registry.Scripts.LegacyImport.Error (ImportError(..))
import Registry.Scripts.LegacyImport.Manifest as Manifest
import Registry.Types (RawPackageName(..), RawVersion(..))
import Registry.Version (ParseMode(..), Range, Version)
import Registry.Version as Version
import Sunde as Process
import Text.Parsing.StringParser as StringParser

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect do
    Env.lookupEnv "GITHUB_EVENT_PATH"
      >>= maybe (throw "GITHUB_EVENT_PATH not defined in the environment") pure
  octokit <- liftEffect GitHub.mkOctokit
  packagesMetadata <- mkMetadataRef
  checkIndexExists

  readOperation eventPath >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> runRegistryM (mkEnv octokit packagesMetadata issue) do
      comment $ Array.fold
        [ "The JSON input for this package update is malformed:"
        , newlines 2
        , "```" <> err <> "```"
        , newlines 2
        , "You can try again by commenting on this issue with a corrected payload."
        ]

    DecodedOperation issue op ->
      runRegistryM (mkEnv octokit packagesMetadata issue) (runOperation op)

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber Operation

derive instance Eq OperationDecoding
derive instance Generic.Generic OperationDecoding _

instance Show OperationDecoding where
  show = genericShow

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.readTextFile UTF8 eventPath

  GitHub.Event { issueNumber, body } <- case Json.parseJson fileContents of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  pure $ case JsonParser.jsonParser body of
    Left _err -> NotJson
    Right json -> case Json.decode json of
      Left err -> MalformedJson issueNumber err
      Right op -> DecodedOperation issueNumber op

-- TODO: test all the points where the pipeline could throw, to show that we are implementing
-- all the necessary checks
runOperation :: Operation -> RegistryM Unit
runOperation operation = case operation of
  -- TODO handle addToPackageSet, addToPursuit
  Addition { packageName, newRef, buildPlan, newPackageLocation } -> do
    -- check that we don't have a metadata file for that package
    metadataExists <- liftAff $ FS.exists $ metadataFile packageName
    let updateData = { packageName, buildPlan, updateRef: newRef }
    -- if the metadata file already exists then we steer this to be an Update instead
    if metadataExists then
      runOperation $ Update updateData
    else
      addOrUpdate updateData $ mkNewMetadata newPackageLocation

  Update { packageName, buildPlan, updateRef } -> do
    metadata <- readMetadata packageName { noMetadata: "No metadata found for your package. Did you mean to create an Addition?" }
    addOrUpdate { packageName, buildPlan, updateRef } metadata

  Authenticated auth@(AuthenticatedData { payload }) -> case payload of
    Unpublish { packageName, unpublishReason, unpublishVersion } -> do
      metadata <- readMetadata packageName { noMetadata: "No metadata found for your package. Only published packages can be unpublished." }

      let
        inPublished = Map.lookup unpublishVersion metadata.published
        inUnpublished = Map.lookup unpublishVersion metadata.unpublished

      publishedMetadata <- case inPublished, inUnpublished of
        Nothing, Nothing ->
          throwWithComment $ "Cannot unpublish " <> Version.printVersion unpublishVersion <> " because it is not a published version."
        Just published, Nothing ->
          -- We only pass through the case where the user is unpublishing a
          -- package that has been published and not yet unpublished.
          pure published
        Nothing, Just _ ->
          throwWithComment $ "Cannot unpublish " <> Version.printVersion unpublishVersion <> " because it has already been unpublished."
        Just _, Just _ ->
          throwWithComment $ String.joinWith "\n"
            [ "Cannot unpublish " <> Version.printVersion unpublishVersion <> "."
            , ""
            , "This version is listed both as published and unpublished. This is an internal error."
            , "cc @purescript/packaging"
            ]

      case metadata.owners of
        Nothing ->
          throwWithComment $ String.joinWith " "
            [ "Cannot verify package ownership because no owners are listed in the package metadata."
            , "Please publish a package version with your SSH public key in the owners field."
            , "You can then retry unpublishing this version by authenticating with your private key."
            ]
        Just owners ->
          liftAff (SSH.verifyPayload owners auth) >>= case _ of
            Left err ->
              throwWithComment $ String.joinWith "\n"
                [ "Failed to verify package ownership:"
                , "  " <> err
                ]
            Right _ -> do
              now <- liftEffect $ Now.nowDateTime

              publishedDate <- case PDT.fromRFC3339String publishedMetadata.publishedTime of
                Nothing ->
                  throwWithComment "Published time is an invalid RFC3339String\ncc @purescript/packaging"
                Just precise ->
                  pure $ PDT.toDateTimeLossy precise

              let hourLimit = 48
              let diff = DateTime.diff now publishedDate
              when (diff > Hours (Int.toNumber hourLimit)) do
                throwWithComment $ "Packages can only be unpublished within " <> show hourLimit <> " hours."

              deletePackage { name: packageName, version: unpublishVersion }

              let
                unpublishedMetadata =
                  { ref: publishedMetadata.ref
                  , reason: unpublishReason
                  , publishedTime: publishedMetadata.publishedTime
                  , unpublishedTime: RFC3339String.fromDateTime now
                  }

                updatedMetadata = unpublishVersionInMetadata unpublishVersion unpublishedMetadata metadata

              writeMetadata packageName updatedMetadata
                { commitFailed: \_ -> "Unpublish succeeded, but committing metadata failed.\ncc @purescript/packaging"
                , commitSucceeded: "Successfully unpublished!"
                }

    Transfer { packageName, newPackageLocation } -> do
      metadata <- readMetadata packageName
        { noMetadata: String.joinWith " "
            [ "No metadata found for your package."
            , "You can only transfer packages that have already been published."
            , "Did you mean to create an Addition?"
            ]
        }

      case metadata.owners of
        Nothing ->
          throwWithComment $ String.joinWith " "
            [ "Cannot verify package ownership because no owners are listed in the package metadata."
            , "Please publish a package version with your SSH public key in the owners field."
            , "You can then retry transferring this package by authenticating with your private key."
            ]
        Just owners ->
          liftAff (SSH.verifyPayload owners auth) >>= case _ of
            Left err ->
              throwWithComment $ String.joinWith "\n"
                [ "Failed to verify package ownership:"
                , "  " <> err
                ]
            Right _ -> do
              let updatedMetadata = metadata { location = newPackageLocation }
              writeMetadata packageName updatedMetadata
                { commitFailed: \_ -> "Transferred package location, but failed to commit metadata.\ncc @purescript/packaging"
                , commitSucceeded: "Successfully transferred your package!"
                }

metadataDir :: FilePath
metadataDir = "../metadata"

metadataFile :: PackageName -> FilePath
metadataFile packageName = Path.concat [ metadataDir, PackageName.print packageName <> ".json" ]

indexDir :: FilePath
indexDir = "../registry-index"

addOrUpdate :: UpdateData -> Metadata -> RegistryM Unit
addOrUpdate { updateRef, buildPlan, packageName } inputMetadata = do
  tmpDir <- liftEffect $ Tmp.mkTmpDir

  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  { packageDirectory, publishedTime } <- fetchPackageSource { tmpDir, ref: updateRef, location: inputMetadata.location }

  let absoluteFolderPath = Path.concat [ tmpDir, packageDirectory ]
  let manifestPath = Path.concat [ absoluteFolderPath, "purs.json" ]

  log $ "Package available in " <> absoluteFolderPath

  log "Verifying that the package contains a `src` directory"
  whenM (liftAff $ map (Array.null <<< _.succeeded) $ FastGlob.match absoluteFolderPath [ "src/**/*.purs" ]) do
    throwWithComment "This package has no .purs files in the src directory. All package sources must be in the src directory."

  -- If this is a legacy import, then we need to construct a `Manifest` for it.
  -- We also won't run the compiler verification.
  isLegacyImport <- liftAff $ map not $ FS.exists manifestPath
  when isLegacyImport do
    address <- case inputMetadata.location of
      Git _ -> throwWithComment "Legacy packages can only come from GitHub. Aborting."
      GitHub { owner, repo } -> pure { owner, repo }

    version <- case Version.parseVersion Lenient updateRef of
      Left _ -> throwWithComment $ "Not a valid registry version: " <> updateRef
      Right result -> pure result

    let
      liftError = map (lmap ManifestImportError)

      runManifest =
        Except.runExceptT <<< Except.mapExceptT (liftAff <<< map (lmap Json.printJson))

      gatherManifest :: ExceptT ImportError Aff Manifest
      gatherManifest = do
        manifestFields <- Manifest.constructManifestFields (RawPackageName $ show packageName) (RawVersion updateRef) address
        Except.mapExceptT liftError $ Manifest.toManifest packageName inputMetadata.location version manifestFields

    runManifest gatherManifest >>= case _ of
      Left err ->
        throwWithComment $ "Unable to produce a manifest for legacy package: " <> err
      Right manifest ->
        liftAff $ Json.writeJsonFile manifestPath manifest

  -- TODO: Verify the manifest against metadata.
  -- Try to read the manifest, typechecking it
  manifest@(Manifest manifestRecord) <- liftAff (try $ FS.readTextFile UTF8 manifestPath) >>= case _ of
    Left _err -> throwWithComment $ "Manifest not found at " <> manifestPath
    Right manifestStr -> do
      liftAff (Dhall.jsonToDhallManifest manifestStr) >>= case _ of
        Left err ->
          throwWithComment $ "Could not type-check Manifest file: " <> err
        Right _ -> case Json.parseJson manifestStr of
          Left err -> throwWithComment $ "Could not convert Manifest to JSON: " <> err
          Right res -> pure res

  let
    -- As soon as we have the manifest we need to update any fields that can
    -- change from version to version.
    metadata =
      inputMetadata { owners = manifestRecord.owners }

  runChecks { isLegacyImport, buildPlan, metadata, manifest }

  -- After we pass all the checks it's time to do side effects and register the package
  log "Packaging the tarball to upload..."
  -- We need the version number to upload the package
  let newVersion = manifestRecord.version
  let newDirname = PackageName.print packageName <> "-" <> Version.printVersion newVersion
  let packageSourceDir = Path.concat [ tmpDir, newDirname ]
  liftAff $ FS.Extra.ensureDirectory packageSourceDir
  -- We copy over all files that are always included (ie. src dir, purs.json file),
  -- and any files the user asked for via the 'files' key, and remove all files
  -- that should never be included (even if the user asked for them).
  copyPackageSourceFiles manifestRecord.files { source: absoluteFolderPath, destination: packageSourceDir }
  liftAff $ removeIgnoredTarballFiles packageSourceDir
  let tarballPath = packageSourceDir <> ".tar.gz"
  liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname, archiveName: tarballPath }
  log "Checking the tarball size..."
  FS.Stats.Stats { size: bytes } <- liftAff $ FS.stat tarballPath
  when (not isLegacyImport && bytes > warnPackageBytes) do
    if bytes > maxPackageBytes then
      throwWithComment $ "Package tarball is " <> show bytes <> " bytes, which exceeds the maximum size of " <> show maxPackageBytes <> " bytes.\ncc: @purescript/packaging"
    else
      log $ "WARNING: Package tarball is " <> show bytes <> ".\ncc: @purescript/packaging"
  log "Hashing the tarball..."
  hash <- liftAff $ Hash.sha256File tarballPath
  log $ "Hash: " <> show hash
  log "Uploading package to the storage backend..."
  let uploadPackageInfo = { name: packageName, version: newVersion }
  uploadPackage uploadPackageInfo tarballPath
  log $ "Adding the new version " <> Version.printVersion newVersion <> " to the package metadata file (hashes, etc)"
  log $ "Hash for ref " <> show updateRef <> " was " <> show hash
  let newMetadata = addVersionToMetadata newVersion { hash, ref: updateRef, publishedTime, bytes } metadata
  writeMetadata packageName newMetadata
    { commitFailed: \_ -> "Package uploaded, but committing metadata failed.\ncc: @purescript/packaging"
    , commitSucceeded: "Successfully uploaded package to the registry! 🎉 🚀"
    }

  closeIssue

  -- Optional steps below:
  -- TODO don't fail the pipeline if one of them fails

  -- Add to the registry index
  -- TODO: commit the registry-index
  liftAff $ Index.insertManifest indexDir manifest

  unless isLegacyImport do
    -- TODO: handle addToPackageSet: we'll try to add it to the latest set and build (see #156)
    pure unit

  unless isLegacyImport do
    log "Uploading to Pursuit"
    publishToPursuit { packageSourceDir, buildPlan }
    comment "Successfully uploaded package docs to Pursuit! 🎉 🚀"

runChecks :: { isLegacyImport :: Boolean, buildPlan :: BuildPlan, metadata :: Metadata, manifest :: Manifest } -> RegistryM Unit
runChecks { isLegacyImport, buildPlan, metadata, manifest } = do
  let Manifest manifestFields = manifest

  -- TODO: collect all errors and return them at once. Note: some of the checks
  -- are going to fail while parsing from JSON, so we should move them here if we
  -- want to handle everything together
  log "Running checks for the following manifest:"
  logShow manifestFields

  log "Ensuring the package is not the purescript-metadata package, which cannot be published."
  when (PackageName.print manifestFields.name == "metadata") do
    throwWithComment "The `metadata` package cannot be uploaded to the registry as it is a protected package."

  log "Check that version has not already been published"
  case Map.lookup manifestFields.version metadata.published of
    Nothing -> pure unit
    Just info -> throwWithComment $ String.joinWith "\n"
      [ "You tried to upload a version that already exists: " <> Version.printVersion manifestFields.version
      , "Its metadata is:"
      , "```"
      , show info
      , "```"
      ]

  log "Check that version has not been unpublished"
  case Map.lookup manifestFields.version metadata.unpublished of
    Nothing -> pure unit
    Just info -> throwWithComment $ String.joinWith "\n"
      [ "You tried to upload a version that has been unpublished: " <> Version.printVersion manifestFields.version
      , "Details:"
      , "```"
      , show info
      , "```"
      ]

  log "Check that all dependencies are contained in the registry"
  packages <- readPackagesMetadata

  let
    pkgNotInRegistry name = case Map.lookup name packages of
      Nothing -> Just name
      Just _p -> Nothing
    pkgsNotInRegistry =
      Array.mapMaybe pkgNotInRegistry $ Set.toUnfoldable $ Map.keys manifestFields.dependencies

  unless (Array.null pkgsNotInRegistry) do
    throwWithComment $ "Some dependencies of your package were not found in the Registry: " <> show pkgsNotInRegistry

  log "Check the submitted build plan matches the manifest"

  let unresolvedDependencies = getUnresolvedDependencies manifest buildPlan

  unless (isLegacyImport || Array.null unresolvedDependencies) do
    let
      { fail: missingPackages, success: incorrectVersions } = partitionEithers unresolvedDependencies

      printPackageRange (name /\ range) = Array.fold
        [ "`"
        , PackageName.print name
        , "` in range `"
        , Version.printRange range
        , "`"
        ]

      missingPackagesError = do
        guardA (not Array.null missingPackages)
        pure
          $ String.joinWith "\n  - "
          $ Array.cons "The build plan is missing dependencies that are listed in the manifest:"
          $ map printPackageRange missingPackages

      printPackageVersion (name /\ range /\ version) = Array.fold
        [ "`"
        , PackageName.print name
        , "@"
        , Version.printVersion version
        , "` does not satisfy range `"
        , Version.printRange range
        , "`"
        ]

      incorrectVersionsError = do
        guardA (not Array.null incorrectVersions)
        pure
          $ String.joinWith "\n  - "
          $ Array.cons "The build plan provides dependencies at versions outside the range listed in the manifest:"
          $ map printPackageVersion incorrectVersions

    throwWithComment $ String.joinWith "\n\n" $ Array.catMaybes
      [ Just "All dependencies from the manifest must be in the build plan at valid versions."
      , missingPackagesError
      , incorrectVersionsError
      ]

getUnresolvedDependencies :: Manifest -> BuildPlan -> Array (Either (PackageName /\ Range) (PackageName /\ Range /\ Version))
getUnresolvedDependencies (Manifest { dependencies }) (BuildPlan { resolutions }) =
  Array.mapMaybe (uncurry dependencyUnresolved) (Map.toUnfoldable dependencies)
  where
  dependencyUnresolved :: PackageName -> Range -> Maybe (Either (PackageName /\ Range) (PackageName /\ Range /\ Version))
  dependencyUnresolved dependencyName dependencyRange =
    case Map.lookup dependencyName resolutions of
      -- If the package is missing from the build plan then the plan is incorrect.
      Nothing -> Just $ Left $ dependencyName /\ dependencyRange
      -- If the package exists, but the version is not in the manifest range
      -- then the build plan is incorrect. Otherwise, this part of the build
      -- plan is correct.
      Just version
        | not (Version.rangeIncludes dependencyRange version) -> Just $ Right $ dependencyName /\ dependencyRange /\ version
        | otherwise -> Nothing

type PublishToPursuit =
  { packageSourceDir :: FilePath
  , buildPlan :: BuildPlan
  }

-- | Publishes a package to Pursuit.
-- |
-- | ASSUMPTIONS: This function should not be run on legacy packages or on
-- | packages where the `purescript-` prefix is still present.
publishToPursuit :: PublishToPursuit -> RegistryM Unit
publishToPursuit { packageSourceDir, buildPlan: buildPlan@(BuildPlan { compiler, resolutions }) } = do
  log "Fetching package dependencies"
  tmpDir <- liftEffect $ Tmp.mkTmpDir
  let dependenciesDir = tmpDir <> Path.sep <> "dependencies"
  liftAff $ FS.mkdir dependenciesDir

  -- We fetch every dependency at its resolved version, unpack the tarball, and
  -- store the resulting source code in a specified directory for dependencies.
  for_ (Map.toUnfoldable resolutions :: Array _) \(Tuple packageName version) -> do
    let
      -- This filename uses the format the directory name will have once
      -- unpacked, ie. package-name-major.minor.patch
      filename = PackageName.print packageName <> "-" <> Version.printVersion version <> ".tar.gz"
      filepath = dependenciesDir <> Path.sep <> filename
    wget ("packages.registry.purescript.org/" <> PackageName.print packageName <> "/" <> Version.printVersion version <> ".tar.gz") filepath
    liftEffect $ Tar.extract { cwd: dependenciesDir, filename: filepath }
    liftAff $ FS.unlink filepath

  log "Generating a resolutions file"
  let
    resolvedPaths = buildPlanToResolutions { buildPlan, dependenciesDir }
    resolutionsFilePath = tmpDir <> Path.sep <> "resolutions.json"

  liftAff $ Json.writeJsonFile resolutionsFilePath resolvedPaths

  -- NOTE: The compatibility version of purs publish appends 'purescript-' to the
  -- package name in the manifest file:
  -- https://github.com/purescript/purescript/blob/a846892d178d3c9c76c162ca39b9deb6fad4ec8e/src/Language/PureScript/Publish/Registry/Compat.hs#L19
  --
  -- The resulting documentation will all use purescript- prefixes in keeping
  -- with the format used by Pursuit in PureScript versions at least up to 0.16
  compilerOutput <- liftAff $ callCompiler
    { args: [ "publish", "--manifest", "purs.json", "--resolutions", resolutionsFilePath ]
    , version: Version.printVersion compiler
    , cwd: Just packageSourceDir
    }

  publishJson <- case compilerOutput of
    Left (UnknownError err) -> throwWithComment $ String.joinWith "\n" [ "Publishing failed for your package due to a compiler error:", "```", err, "```" ]
    Left MissingCompiler -> throwWithComment $ Array.fold [ "Publishing failed because the build plan compiler version ", Version.printVersion compiler, " is not supported. Please try again with a different compiler." ]
    Right publishResult -> do
      -- The output contains plenty of diagnostic lines, ie. "Compiling ..."
      -- but we only want the final JSON payload.
      let lines = String.split (String.Pattern "\n") publishResult
      case Array.last lines of
        Nothing -> throwWithComment $ Array.fold [ "Publishing failed because of an unexpected compiler error. cc @purescript/packaging" ]
        Just jsonString -> case Argonaut.Core.jsonParser jsonString of
          Left err ->
            throwWithComment $ String.joinWith "\n" [ "Failed to parse output of publishing. cc @purescript/packaging", "```" <> err <> "```" ]
          Right json ->
            pure json

  authToken <- liftEffect (Node.Process.lookupEnv "PACCHETTIBOTTI_PURSUIT_TOKEN") >>= case _ of
    Nothing -> do
      logShow =<< liftEffect Node.Process.getEnv
      throwWithComment "Publishing failed because there is no available auth token. cc: @purescript/packaging"
    Just token ->
      pure token

  log "Pushing to Pursuit"
  result <- liftAff $ Http.request
    { content: Just $ RequestBody.json publishJson
    , headers:
        [ RequestHeader.Accept MediaType.applicationJSON
        , RequestHeader.RequestHeader "Authorization" ("token " <> authToken)
        ]
    , method: Left Method.POST
    , username: Nothing
    , withCredentials: false
    , password: Nothing
    , responseFormat: ResponseFormat.string
    , timeout: Nothing
    , url: "https://pursuit.purescript.org/packages"
    }

  case result of
    Right { status } | status == StatusCode 201 ->
      pure unit
    Right { body, status: StatusCode status } ->
      throwWithComment $ String.joinWith "\n"
        [ "Expected a 201 response from Pursuit, but received " <> show status <> " instead (cc: @purescript/packaging)."
        , "Body:"
        , "```" <> body <> "```"
        ]
    Left err -> do
      let printedErr = Http.printError err
      throwWithComment $ String.joinWith "\n" [ "Received a failed response from Pursuit (cc: @purescript/packaging): ", "```" <> printedErr <> "```" ]

  pure unit

-- Resolutions format: https://github.com/purescript/purescript/pull/3565
--
-- Note: This interfaces with Pursuit, and therefore we must add purescript-
-- prefixes to all package names for compatibility with the Bower naming format.
buildPlanToResolutions :: { buildPlan :: BuildPlan, dependenciesDir :: FilePath } -> Map RawPackageName { version :: Version, path :: FilePath }
buildPlanToResolutions { buildPlan: BuildPlan { resolutions }, dependenciesDir } =
  Map.fromFoldable do
    Tuple name version <- (Map.toUnfoldable resolutions :: Array _)
    let
      bowerPackageName = RawPackageName ("purescript-" <> PackageName.print name)
      packagePath = Path.concat [ dependenciesDir, PackageName.print name <> "-" <> Version.printVersion version ]
    pure $ Tuple bowerPackageName { path: packagePath, version }

wget :: String -> FilePath -> RegistryM Unit
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = [ "-O", path, url ]
  result <- liftAff $ Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> pure unit
    _ -> throwWithComment $ "Error while fetching tarball: " <> result.stderr

mkEnv :: GitHub.Octokit -> MetadataRef -> IssueNumber -> Env
mkEnv octokit packagesMetadata issue =
  { comment: GitHub.createComment octokit issue
  , closeIssue: GitHub.closeIssue octokit issue
  , commitToTrunk: pushToMaster
  , uploadPackage: Upload.upload
  , deletePackage: Upload.delete
  , packagesMetadata
  }

type MetadataMap = Map PackageName Metadata
type MetadataRef = Ref MetadataMap

mkMetadataRef :: Aff MetadataRef
mkMetadataRef = do
  FS.Extra.ensureDirectory metadataDir
  packageList <- try (FS.readdir metadataDir) >>= case _ of
    Right list -> pure $ Array.mapMaybe (String.stripSuffix $ String.Pattern ".json") list
    Left err -> do
      error $ show err
      pure []
  packagesArray <- for packageList \rawPackageName -> do
    packageName <- case PackageName.parse rawPackageName of
      Right p -> pure p
      Left err -> do
        log $ "Encountered error while parsing package name! It was: " <> rawPackageName
        Aff.throwError $ Aff.error $ StringParser.printParserError err
    let metadataPath = metadataFile packageName
    metadata <- Json.readJsonFile metadataPath >>= case _ of
      Left err -> Aff.throwError $ Aff.error $ "Error parsing metadata file located at " <> metadataPath <> ": " <> err
      Right val -> pure val
    pure $ packageName /\ metadata
  liftEffect $ Ref.new $ Map.fromFoldable packagesArray

isPackageVersionInMetadata :: PackageName -> Version -> MetadataMap -> Boolean
isPackageVersionInMetadata packageName version metadataMap =
  case Map.lookup packageName metadataMap of
    Nothing -> false
    Just metadata -> isVersionInMetadata version metadata

checkIndexExists :: Aff Unit
checkIndexExists = do
  log "Checking if the registry-index is present..."
  whenM (not <$> FS.exists indexDir) do
    error "Didn't find the 'registry-index' repo, cloning..."
    Except.runExceptT (runGit [ "clone", "https://github.com/purescript/registry-index.git", indexDir ] Nothing) >>= case _ of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> log "Successfully cloned the 'registry-index' repo"

data PursPublishMethod = LegacyPursPublish | PursPublish

-- | A temporary flag that records whether we are using legacy purs publish
-- | (which requires all packages to be a Git repository) or new purs publish
-- | (which accepts any directory with package sources).
pursPublishMethod :: PursPublishMethod
pursPublishMethod = LegacyPursPublish

fetchPackageSource
  :: { tmpDir :: FilePath, ref :: String, location :: Location }
  -> RegistryM { packageDirectory :: FilePath, publishedTime :: RFC3339String }
fetchPackageSource { tmpDir, ref, location } = case location of
  Git _ -> do
    -- TODO: Support non-GitHub packages. Remember subdir when doing so. (See #15)
    throwWithComment "Packages are only allowed to come from GitHub for now. See #15"

  GitHub { owner, repo, subdir } -> do
    -- TODO: Support subdir. In the meantime, we verify subdir is not present. (See #16)
    when (isJust subdir) $ throwWithComment "`subdir` is not supported for now. See #16"

    case pursPublishMethod of
      LegacyPursPublish -> liftAff do
        log $ "Cloning repo at tag: " <> show { owner, repo, ref }
        cloneGitTag (i "https://github.com/" owner "/" repo) ref tmpDir
        log $ "Getting published time..."
        -- Cloning will result in the `repo` name as the directory name
        publishedTime <- gitGetRefTime ref (Path.concat [ tmpDir, repo ])
        pure { packageDirectory: repo, publishedTime }

      PursPublish -> do
        octokit <- liftEffect GitHub.mkOctokit
        commit <- liftAff $ GitHub.getRefCommit octokit { owner, repo } ref
        commitDate <- liftAff $ GitHub.getCommitDate octokit { owner, repo } commit
        let tarballName = ref <> ".tar.gz"
        let absoluteTarballPath = Path.concat [ tmpDir, tarballName ]
        let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
        log $ "Fetching tarball from GitHub: " <> archiveUrl
        wget archiveUrl absoluteTarballPath
        log $ "Tarball downloaded in " <> absoluteTarballPath
        liftEffect (Tar.getToplevelDir absoluteTarballPath) >>= case _ of
          Nothing ->
            throwWithComment "Could not find a toplevel dir in the tarball!"
          Just dir -> do
            log "Extracting the tarball..."
            liftEffect $ Tar.extract { cwd: tmpDir, filename: absoluteTarballPath }
            pure { packageDirectory: dir, publishedTime: commitDate }

-- | Clone a package from a Git location to the provided directory.
cloneGitTag :: Http.URL -> String -> FilePath -> Aff Unit
cloneGitTag url ref targetDir =
  Except.runExceptT (runGit [ "clone", url, "--branch", ref, "--single-branch", "-c", "advice.detachedHead=false" ] (Just targetDir)) >>= case _ of
    Left err -> Aff.throwError $ Aff.error err
    Right _ -> log "Successfully cloned package."

-- | Read the published time of the checked-out commit.
gitGetRefTime :: String -> FilePath -> Aff RFC3339String
gitGetRefTime ref repoDir = do
  result <- Except.runExceptT do
    timestamp <- runGit [ "log", "-1", "--date=iso8601-strict", "--format=%cd", ref ] (Just repoDir)
    jsDate <- liftEffect $ JSDate.parse timestamp
    dateTime <- Except.except $ note "Failed to convert JSDate to DateTime" $ JSDate.toDateTime jsDate
    pure $ PDT.toRFC3339String $ PDT.fromDateTime dateTime
  case result of
    Left err -> Aff.throwError $ Aff.error $ "Failed to get ref time: " <> err
    Right res -> pure res

pushToMaster :: PackageName -> FilePath -> Aff (Either String Unit)
pushToMaster packageName path = Except.runExceptT do
  _ <- runGit [ "config", "user.name", "PacchettiBotti" ] Nothing
  _ <- runGit [ "config", "user.email", "<pacchettibotti@ferrai.io>" ] Nothing
  _ <- runGit [ "add", path ] Nothing
  _ <- runGit [ "commit", "-m", "Update metadata for package " <> PackageName.print packageName ] Nothing
  _ <- runGit [ "push", "origin", "master" ] Nothing
  pure unit

runGit :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGit args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  case result.exit of
    NodeProcess.Normally 0 -> do
      info result.stdout
      info result.stderr
      pure $ Right $ String.trim result.stdout
    _ -> pure $ Left $ String.trim result.stderr

-- | The absolute maximum bytes allowed in a package
maxPackageBytes :: Number
maxPackageBytes = 2_000_000.0

-- | The number of bytes over which we flag a package for review
warnPackageBytes :: Number
warnPackageBytes = 200_000.0

-- | Copy files from the package source directory to the destination directory
-- | for the tarball. This will copy all always-included files as well as files
-- | provided by the user via the `files` key.
copyPackageSourceFiles :: Maybe (Array String) -> { source :: FilePath, destination :: FilePath } -> RegistryM Unit
copyPackageSourceFiles files { source, destination } = do
  userFiles <- case files of
    Nothing -> pure []
    Just globs -> do
      { succeeded, failed } <- liftAff $ FastGlob.match source globs

      unless (Array.null failed) do
        throwWithComment $ String.joinWith " "
          [ "Some paths matched by globs in the 'files' key are outside your package directory."
          , "Please ensure globs only match within your package directory, including symlinks."
          ]

      pure succeeded

  includedFiles <- liftAff $ FastGlob.match source includedGlobs
  includedInsensitiveFiles <- liftAff $ FastGlob.match' source includedInsensitiveGlobs { caseSensitive: false }

  let
    copyFiles = userFiles <> includedFiles.succeeded <> includedInsensitiveFiles.succeeded
    makePaths path = { from: Path.concat [ source, path ], to: Path.concat [ destination, path ] }

  liftAff $ traverse_ (makePaths >>> FS.Extra.copy) copyFiles

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
  ]

-- | These files are always included and should be globbed in case-insensitive
-- | mode.
includedInsensitiveGlobs :: Array String
includedInsensitiveGlobs =
  [ "README*"
  , "LICENSE*"
  , "LICENCE*"
  ]

-- | We always ignore some files and directories when packaging a tarball, such
-- | as common version control directories, even if a user has explicitly opted
-- | in to those files with the 'files' key.
-- |
-- | See also:
-- | https://docs.npmjs.com/cli/v8/configuring-npm/package-json#files
removeIgnoredTarballFiles :: FilePath -> Aff Unit
removeIgnoredTarballFiles path = do
  globMatches <- FastGlob.match' path ignoredGlobs { caseSensitive: false }
  for_ (ignoredDirectories <> ignoredFiles <> globMatches.succeeded) \match ->
    FS.Extra.remove (Path.concat [ path, match ])

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

readMetadata :: PackageName -> { noMetadata :: String } -> RegistryM Metadata
readMetadata packageName { noMetadata } = do
  let metadataFilePath = metadataFile packageName
  liftAff (FS.exists metadataFilePath) >>= case _ of
    false -> throwWithComment noMetadata
    _ -> pure unit

  readPackagesMetadata >>= \packages -> case Map.lookup packageName packages of
    Nothing -> throwWithComment "Couldn't read metadata file for your package.\ncc @purescript/packaging"
    Just m -> pure m

writeMetadata :: PackageName -> Metadata -> { commitFailed :: String -> String, commitSucceeded :: String } -> RegistryM Unit
writeMetadata packageName metadata { commitFailed, commitSucceeded } = do
  let metadataFilePath = metadataFile packageName
  liftAff $ Json.writeJsonFile metadataFilePath metadata
  updatePackagesMetadata packageName metadata
  commitToTrunk packageName metadataFilePath >>= case _ of
    Left err -> comment (commitFailed err)
    Right _ -> comment commitSucceeded

-- | Call a specific version of the PureScript compiler
callCompiler_ :: { version :: String, args :: Array String, cwd :: Maybe FilePath } -> Aff Unit
callCompiler_ = void <<< callCompiler

data CompilerFailure = UnknownError String | MissingCompiler

derive instance Eq CompilerFailure

-- | Call a specific version of the PureScript compiler
callCompiler :: { version :: String, args :: Array String, cwd :: Maybe FilePath } -> Aff (Either CompilerFailure String)
callCompiler { version, args, cwd } = do
  let
    -- Converts a string version 'v0.13.0' or '0.13.0' to the standard format for
    -- executables 'purs-0_13_0'
    compiler =
      append "purs-"
        $ String.replaceAll (String.Pattern ".") (String.Replacement "_")
        $ fromMaybe version
        $ String.stripPrefix (String.Pattern "v") version

  result <- Aff.try $ Process.spawn { cmd: compiler, stdin: Nothing, args } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  pure $ case result of
    Left exception -> case Aff.message exception of
      errorMessage
        | errorMessage == String.joinWith " " [ "spawn", compiler, "ENOENT" ] -> Left MissingCompiler
        | otherwise -> Left $ UnknownError errorMessage
    Right { exit: NodeProcess.Normally 0, stdout } ->
      Right $ String.trim stdout
    Right { stderr } ->
      Left $ UnknownError $ String.trim stderr
