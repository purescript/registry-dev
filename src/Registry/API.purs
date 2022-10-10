module Registry.API where

import Registry.Prelude

import Affjax.Node as Http
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Alternative (guard)
import Control.Monad.Except as Except
import Control.Monad.Reader (ask, asks)
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep as Generic
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Interpolate (i)
import Data.Map as Map
import Data.MediaType.Common as MediaType
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String)
import Data.RFC3339String as RFC3339String
import Data.Set as Set
import Data.String as String
import Data.String.Base64 as Base64
import Data.Time.Duration (Hours(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Effect.Aff as Aff
import Effect.Exception (throw)
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.Dhall as Dhall
import Foreign.FastGlob as FastGlob
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..), IssueNumber)
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Foreign.Purs (CompilerFailure(..))
import Foreign.Purs as Purs
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Foreign.Wget as Wget
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Node.Process as Env
import Node.Process as Node.Process
import Parsing as Parsing
import Registry.Cache (Cache)
import Registry.Cache as Cache
import Registry.Constants as Constants
import Registry.Hash as Hash
import Registry.Index as Index
import Registry.Json as Json
import Registry.Legacy.Manifest as Legacy.Manifest
import Registry.Legacy.PackageSet as Legacy.PackageSet
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.PackageUpload as Upload
import Registry.RegistryM (Env, RegistryM, closeIssue, comment, commitIndexFile, commitMetadataFile, commitPackageSetFile, deletePackage, readPackagesMetadata, runRegistryM, throwWithComment, updatePackagesMetadata, uploadPackage)
import Registry.SSH as SSH
import Registry.Schema (AuthenticatedData(..), AuthenticatedOperation(..), BuildPlan(..), Location(..), Manifest(..), Metadata, Operation(..), Owner(..), PackageSet(..), UpdateData, addVersionToMetadata, isVersionInMetadata, mkNewMetadata, unpublishVersionInMetadata)
import Registry.Solver as Solver
import Registry.Types (RawPackageName(..), RawVersion(..))
import Registry.Version (ParseMode(..), Range, Version)
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect do
    Env.lookupEnv "GITHUB_EVENT_PATH"
      >>= maybe (throw "GITHUB_EVENT_PATH not defined in the environment") pure

  githubToken <- liftEffect do
    Env.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken

  readOperation eventPath >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> do
      let
        comment = Array.fold
          [ "The JSON input for this package update is malformed:"
          , newlines 2
          , "```" <> err <> "```"
          , newlines 2
          , "You can try again by commenting on this issue with a corrected payload."
          ]

      Except.runExceptT (GitHub.createComment octokit issue comment) >>= case _ of
        Left githubError -> throwError $ Aff.error $ GitHub.printGitHubError githubError
        Right _ -> pure unit

    DecodedOperation issue username operation -> do
      cache <- Cache.useCache
      packagesMetadata <- liftEffect $ Ref.new Map.empty
      runRegistryM (mkEnv octokit cache packagesMetadata issue username) do
        comment $ case operation of
          Addition { packageName, newRef } ->
            "Adding package `" <> PackageName.print packageName <> "` at the ref `" <> newRef <> "`."
          Update { packageName, updateRef } ->
            "Updating package `" <> PackageName.print packageName <> "` at the ref `" <> updateRef <> "`."
          PackageSetUpdate _ ->
            "Processing package set update."
          Authenticated (AuthenticatedData { payload }) -> case payload of
            Unpublish { packageName, unpublishVersion } ->
              "Unpublishing `" <> PackageName.print packageName <> "` at version `" <> Version.printVersion unpublishVersion <> "`."
            Transfer { packageName } ->
              "Transferring `" <> PackageName.print packageName <> "`."

        fetchRegistry
        fetchRegistryIndex
        fillMetadataRef
        runOperation API operation

-- | Operations are exercised via the API and the legacy importer. If the
-- | importer is used then we don't compile or publish docs for the package.
-- | If the API is used for a 'legacy' package (ie. a Spago-based project), then
-- | we attempt to compile the package and warn if compilation fails. If the API
-- | is used for a normal package then failed compilation fails the pipeline.
data Source = API | Importer

derive instance Eq Source

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber String Operation

derive instance Eq OperationDecoding
derive instance Generic.Generic OperationDecoding _

instance Show OperationDecoding where
  show = genericShow

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.readTextFile UTF8 eventPath

  GitHub.Event { issueNumber, body, username } <- case Json.parseJson fileContents of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  case Argonaut.Parser.jsonParser (firstObject body) of
    Left err -> do
      log "Not JSON."
      logShow { err, body }
      pure NotJson
    Right json -> case Json.decode json of
      Left err -> do
        log "Malformed JSON."
        logShow { err, body }
        pure $ MalformedJson issueNumber err
      Right operation ->
        pure $ DecodedOperation issueNumber username operation

-- | Users may submit issues with contents wrapped in code fences, perhaps with
-- | a language specifier, trailing lines, and other issues. This rudimentary
-- | cleanup pass retrieves all contents within an opening { and closing }
-- | delimiter.
firstObject :: String -> String
firstObject input = fromMaybe input do
  before <- String.indexOf (String.Pattern "{") input
  let start = String.drop before input
  after <- String.lastIndexOf (String.Pattern "}") start
  pure (String.take (after + 1) start)

-- TODO: test all the points where the pipeline could throw, to show that we are implementing
-- all the necessary checks
runOperation :: Source -> Operation -> RegistryM Unit
runOperation source operation = case operation of
  Addition { packageName, newRef, buildPlan, newPackageLocation } -> do
    packagesMetadata <- readPackagesMetadata
    -- If we already have a metadata file for this package, then it is already
    -- registered and an addition is not a valid operation.
    case Map.lookup packageName packagesMetadata of
      -- If the user is trying to add the package from the same location it was
      -- registered, then we convert their operation to an update under the
      -- assumption they are trying to publish a new version.
      Just metadata | metadata.location == newPackageLocation ->
        runOperation source $ Update { packageName, buildPlan, updateRef: newRef }
      -- Otherwise, if they attempted to re-register the package under a new
      -- location, then they either did not know the package already existed or
      -- they are attempting a transfer.
      Just _ -> throwWithComment $ String.joinWith " "
        [ "Cannot register"
        , PackageName.print packageName
        , "because it has already been registered.\nIf you are attempting to"
        , "register your package, please choose a different package name."
        , "If you are attempting to transfer this package to a new location,"
        , "please submit a transfer instead."
        ]
      -- If this is a brand-new package, then we can allow them to register it
      -- so long as they aren't publishing an existing location under a new name
      Nothing | not (locationIsUnique newPackageLocation packagesMetadata) -> throwWithComment $ String.joinWith " "
        [ "Cannot register"
        , PackageName.print packageName
        , "at the location"
        , show newPackageLocation
        , "because that location is already in use to publish another package."
        ]
      Nothing ->
        addOrUpdate source { packageName, buildPlan, updateRef: newRef } (mkNewMetadata newPackageLocation)

  Update { packageName, buildPlan, updateRef } -> do
    metadata <- readMetadata packageName { noMetadata: "No metadata found for your package. Did you mean to create an Addition?" }
    addOrUpdate source { packageName, buildPlan, updateRef } metadata

  PackageSetUpdate { compiler, packages } -> do
    { octokit, cache, username, registryIndex: registryIndexPath } <- ask

    latestPackageSet <- PackageSet.readLatestPackageSet
    let prevCompiler = (un PackageSet latestPackageSet).compiler
    let prevPackages = (un PackageSet latestPackageSet).packages

    let didChangeCompiler = maybe false (not <<< eq prevCompiler) compiler
    let didRemovePackages = any isNothing packages

    -- Changing the compiler version or removing packages are both restricted
    -- to only the packaging team. We throw here if this is an authenticated
    -- operation and we can't verify they are a member of the packaging team.
    when (didChangeCompiler || didRemovePackages) do
      -- We always throw if we couldn't verify the user who opened or commented
      -- is a member of the packaging team.
      liftAff (Except.runExceptT (GitHub.listTeamMembers octokit cache packagingTeam)) >>= case _ of
        Left githubError -> throwWithComment $ Array.fold
          [ "This package set update changes the compiler version or removes a "
          , "package from the package set. Only members of the "
          , "@purescript/packaging team can take these actions, but we were "
          , "unable to authenticate your account:\n"
          , GitHub.printGitHubError githubError
          ]
        Right members -> do
          unless (Array.elem username (map _.username members)) do
            throwWithComment $ String.joinWith " "
              [ "This package set update changes the compiler version or"
              , "removes a package from the package set. Only members of the"
              , "@purescript/packaging team can take these actions, but your"
              , "username is not a member of the packaging team."
              ]

    -- The compiler version cannot be downgraded.
    for_ compiler \version -> when (version < prevCompiler) do
      throwWithComment $ String.joinWith " "
        [ "You are downgrading the compiler used in the package set from"
        , "the current version (" <> Version.printVersion prevCompiler <> ")"
        , "to the lower version (" <> Version.printVersion version <> ")."
        , "The package set compiler version cannot be downgraded."
        ]

    -- Package versions cannot be downgraded.
    let
      downgradedPackages = Array.catMaybes do
        Tuple packageName packageVersion <- Map.toUnfoldable packages
        pure do
          newVersion <- packageVersion
          prevVersion <- Map.lookup packageName prevPackages
          -- We want to fail if the existing version is greater than the
          -- new proposed version.
          guard (prevVersion > newVersion)
          pure (Tuple packageName { old: prevVersion, new: newVersion })

    when (not (Array.null downgradedPackages)) do
      let
        formatPackage (Tuple name { old, new }) = Array.fold
          [ "  - "
          , PackageName.print name
          , " from "
          , Version.printVersion old
          , " to "
          , Version.printVersion new
          ]

      throwWithComment $ Array.fold
        [ "You are attempting to downgrade one or more package versions from "
        , "their version in the previous set. Affected packages:\n\n"
        , String.joinWith "\n" $ map formatPackage downgradedPackages
        ]

    -- With these conditions met, we can attempt to process the batch with the
    -- new packages and/or compiler version. Note: if the compiler is updated to
    -- a version that isn't supported by the registry then an 'unsupported
    -- compiler' error will be thrown.
    registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath
    PackageSet.validatePackageSet registryIndex latestPackageSet

    let candidates = PackageSet.validatePackageSetCandidates registryIndex latestPackageSet packages

    unless (Map.isEmpty candidates.rejected) do
      throwWithComment $ String.joinWith "\n"
        [ "One or more packages in the suggested batch cannot be processed.\n"
        , PackageSet.printRejections candidates.rejected
        ]

    if Map.isEmpty candidates.accepted then do
      throwWithComment "No packages in the suggested batch can be processed; all failed validation checks."
    else do
      PackageSet.processBatchAtomic registryIndex latestPackageSet compiler candidates.accepted >>= case _ of
        Just { fail, packageSet, success } | Map.isEmpty fail -> do
          newPath <- PackageSet.getPackageSetPath (un PackageSet packageSet).version
          liftAff $ Json.writeJsonFile newPath packageSet
          let commitMessage = PackageSet.commitMessage latestPackageSet success (un PackageSet packageSet).version
          commitPackageSetFile (un PackageSet packageSet).version commitMessage >>= case _ of
            Left err -> throwWithComment $ "Failed to commit package set file (cc: @purescript/packaging): " <> err
            Right _ -> do
              comment "Built and released a new package set! Now mirroring to the package-sets repo..."
              metadata <- readPackagesMetadata
              case Legacy.PackageSet.fromPackageSet registryIndex metadata packageSet of
                Left err -> throwWithComment $ "Failed to convert to legacy package set (cc: @purescript/packaging): " <> err
                Right legacyPackageSet -> do
                  Legacy.PackageSet.mirrorLegacySet legacyPackageSet
                  comment "Mirrored a new legacy package set."
                  closeIssue
        _ -> do
          throwWithComment "The package set produced from this suggested update does not compile."

  Authenticated submittedAuth@(AuthenticatedData { payload }) -> case payload of
    Unpublish { packageName, unpublishReason, unpublishVersion } -> do
      username <- asks _.username
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

      Tuple auth maybeOwners <- acceptTrustees username submittedAuth metadata.owners

      case maybeOwners of
        Nothing ->
          throwWithComment $ String.joinWith " "
            [ "Cannot verify package ownership because no owners are listed in the package metadata."
            , "Please publish a package version with your SSH public key in the owners field."
            , "You can then retry unpublishing this version by authenticating with your private key."
            ]
        Just owners -> liftAff (SSH.verifyPayload owners auth) >>= case _ of
          Left err -> throwWithComment $ String.joinWith "\n"
            [ "Failed to verify package ownership:"
            , err
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

            writeMetadata packageName updatedMetadata >>= case _ of
              Left err -> throwWithComment $ String.joinWith "\n"
                [ "Unpublish succeeded, but committing metadata failed."
                , err
                , "cc @purescript/packaging"
                ]
              Right _ -> pure unit

            writeDeleteIndex packageName unpublishVersion >>= case _ of
              Left err -> throwWithComment $ String.joinWith "\n"
                [ "Unpublish succeeded, but committing to the registry index failed."
                , err
                , "cc: @purescript/packaging"
                ]
              Right _ -> pure unit

            comment "Successfully unpublished!"

      closeIssue

    Transfer { packageName, newPackageLocation } -> do
      username <- asks _.username
      metadata <- readMetadata packageName
        { noMetadata: String.joinWith " "
            [ "No metadata found for your package."
            , "You can only transfer packages that have already been published."
            , "Did you mean to create an Addition?"
            ]
        }

      packagesMetadata <- readPackagesMetadata
      unless (locationIsUnique newPackageLocation packagesMetadata) do
        throwWithComment $ String.joinWith " "
          [ "Cannot transfer package to"
          , show newPackageLocation
          , "because another package is already registered at that location."
          ]

      Tuple auth maybeOwners <- acceptTrustees username submittedAuth metadata.owners

      case maybeOwners of
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
              writeMetadata packageName updatedMetadata >>= case _ of
                Left err -> throwWithComment $ String.joinWith "\n"
                  [ "Transferred package location, but failed to commit metadata."
                  , err
                  , "cc: @purescript/packaging"
                  ]
                Right _ -> do
                  comment "Successfully transferred your package!"
                  registryPath <- asks _.registry
                  liftAff (Except.runExceptT (syncLegacyRegistry registryPath packageName newPackageLocation)) >>= case _ of
                    Left err ->
                      throwWithComment $ "Failed to synchronize with legacy registry (cc: @purescript/packaging): " <> err
                    Right _ ->
                      pure unit

      closeIssue

registryMetadataPath :: FilePath -> FilePath
registryMetadataPath registryPath = Path.concat [ registryPath, Constants.metadataPath ]

registryPackageSetsPath :: FilePath -> FilePath
registryPackageSetsPath registryPath = Path.concat [ registryPath, Constants.packageSetsPath ]

metadataFile :: FilePath -> PackageName -> FilePath
metadataFile registryPath packageName = Path.concat [ registryPath, Constants.metadataPath, PackageName.print packageName <> ".json" ]

addOrUpdate :: Source -> UpdateData -> Metadata -> RegistryM Unit
addOrUpdate source { updateRef, buildPlan: providedBuildPlan, packageName } inputMetadata = do
  tmpDir <- liftEffect $ Tmp.mkTmpDir

  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  { packageDirectory, publishedTime } <- fetchPackageSource { tmpDir, ref: updateRef, location: inputMetadata.location }

  let manifestPath = Path.concat [ packageDirectory, "purs.json" ]

  log $ "Package available in " <> packageDirectory

  log "Verifying that the package contains a `src` directory"
  whenM (liftAff $ map (Array.null <<< _.succeeded) $ FastGlob.match packageDirectory [ "src/**/*.purs" ]) do
    throwWithComment "This package has no .purs files in the src directory. All package sources must be in the src directory."

  -- If this is a legacy import, then we need to construct a `Manifest` for it.
  isLegacyImport <- liftEffect $ map not $ FS.Sync.exists manifestPath
  when isLegacyImport do
    address <- case inputMetadata.location of
      Git _ -> throwWithComment "Legacy packages can only come from GitHub. Aborting."
      GitHub { owner, repo } -> pure { owner, repo }

    version <- case Version.parseVersion Lenient updateRef of
      Left _ -> throwWithComment $ "Not a valid registry version: " <> updateRef
      Right result -> pure result

    legacyPackageSets <- Legacy.Manifest.fetchLegacyPackageSets

    let
      packageSetDeps = do
        versions <- Map.lookup packageName legacyPackageSets
        deps <- Map.lookup (RawVersion updateRef) versions
        pure deps

    Except.runExceptT (Legacy.Manifest.fetchLegacyManifest packageSetDeps address (RawVersion updateRef)) >>= case _ of
      Left manifestError -> do
        let formatError { error, reason } = reason <> " " <> Legacy.Manifest.printLegacyManifestError error
        throwWithComment $ String.joinWith "\n"
          [ "There were problems with the legacy manifest file:"
          , formatError manifestError
          ]
      Right legacyManifest -> do
        let manifest = Legacy.Manifest.toManifest packageName version inputMetadata.location legacyManifest
        liftAff $ Json.writeJsonFile manifestPath manifest

  -- TODO: Verify the manifest against metadata.
  -- We trust the manifest for updating the owners field in the metadata, and
  -- trust the metadata for everything else. No field should be different.
  manifest@(Manifest manifestRecord) <- liftAff (try $ FS.readTextFile UTF8 manifestPath) >>= case _ of
    Left _err -> throwWithComment $ "Manifest not found at " <> manifestPath
    Right manifestStr -> liftAff (Dhall.jsonToDhallManifest manifestStr) >>= case _ of
      Left err -> throwWithComment $ "Could not typecheck manifest: " <> err
      Right _ -> case Json.parseJson manifestStr of
        Left err -> throwWithComment $ "Could not parse manifest as JSON: " <> err
        Right res -> pure res

  let
    -- As soon as we have the manifest we need to update any fields that can
    -- change from version to version. As per the spec, that's only the 'owners'
    -- field.
    metadata = inputMetadata { owners = manifestRecord.owners }

  -- Then, we can run verification checks on the manifest and either verify the
  -- provided build plan or produce a new one.
  verifyManifest { metadata, manifest }
  eitherBuildPlan <- verifyBuildPlan { source, buildPlan: providedBuildPlan, manifest }

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
  copyPackageSourceFiles manifestRecord.files { source: packageDirectory, destination: packageSourceDir }
  liftAff $ removeIgnoredTarballFiles packageSourceDir
  let tarballPath = packageSourceDir <> ".tar.gz"
  liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname }
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

  -- Now that we have the package source contents we can verify we can compile
  -- the package. We skip failures when the package is a legacy package.
  compilationResult <- case eitherBuildPlan of
    -- We do not throw an exception if we're bulk-uploading legacy packages
    Left error | source == Importer || (source == API && isLegacyImport) ->
      pure (Left error)
    Left error ->
      throwWithComment error
    Right buildPlan ->
      compilePackage { packageSourceDir: packageDirectory, buildPlan }

  case compilationResult of
    Left error
      | source == Importer -> do
          log error
          log "Failed to compile, but continuing because the API source was the importer."
      | source == API && isLegacyImport -> do
          log error
          log "Failed to compile, but continuing because this is a legacy package."
      | otherwise ->
          throwWithComment error
    Right _ ->
      pure unit

  log "Uploading package to the storage backend..."
  let uploadPackageInfo = { name: packageName, version: newVersion }
  uploadPackage uploadPackageInfo tarballPath
  log $ "Adding the new version " <> Version.printVersion newVersion <> " to the package metadata file (hashes, etc)"
  log $ "Hash for ref " <> show updateRef <> " was " <> show hash
  let newMetadata = addVersionToMetadata newVersion { hash, ref: updateRef, publishedTime, bytes } metadata
  writeMetadata packageName newMetadata >>= case _ of
    Left err -> throwWithComment $ String.joinWith "\n"
      [ "Package uploaded, but committing metadata failed."
      , err
      , "cc: @purescript/packaging"
      ]
    Right _ ->
      comment "Successfully uploaded package to the registry! 🎉 🚀"

  closeIssue

  registryPath <- asks _.registry
  liftAff (Except.runExceptT (syncLegacyRegistry registryPath packageName newMetadata.location)) >>= case _ of
    Left err -> throwWithComment $ "Failed to synchronize with legacy registry (cc: @purescript/packaging): " <> err
    Right _ -> pure unit

  -- After a package has been uploaded we add it to the registry index, we
  -- upload its documentation to Pursuit, and we can now process it for package
  -- sets when the next batch goes out.

  -- We write to the registry index if possible. If this fails, the packaging
  -- team should manually insert the entry.
  writeInsertIndex manifest >>= case _ of
    Left err -> comment $ String.joinWith "\n"
      [ "Package uploaded, but committing to the registry failed."
      , err
      , "cc: @purescript/packaging"
      ]
    Right _ -> pure unit

  when (source == API) $ case compilationResult of
    Left error ->
      comment $ Array.fold [ "Skipping Pursuit publishing because this package failed to compile:\n\n", error ]
    Right dependenciesDir -> do
      log "Uploading to Pursuit"
      for_ eitherBuildPlan \buildPlan -> do
        publishToPursuit { packageSourceDir: packageDirectory, buildPlan, dependenciesDir } >>= case _ of
          Left error ->
            comment $ "Pursuit publishing failed: " <> error
          Right message ->
            comment message

verifyManifest :: { metadata :: Metadata, manifest :: Manifest } -> RegistryM Unit
verifyManifest { metadata, manifest } = do
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

-- | Verify the build plan for the package. If the user provided a build plan,
-- | we ensure that the provided versions are within the ranges listed in the
-- | manifest. If not, we solve their manifest to produce a build plan.
verifyBuildPlan :: { source :: Source, buildPlan :: BuildPlan, manifest :: Manifest } -> RegistryM (Either String BuildPlan)
verifyBuildPlan { source, buildPlan: buildPlan@(BuildPlan plan), manifest } = Except.runExceptT do
  log "Check the submitted build plan matches the manifest"
  -- We don't verify packages provided via the mass import.
  case source of
    Importer -> pure buildPlan
    API -> case plan.resolutions of
      Nothing -> do
        indexPath <- asks _.registryIndex
        registryIndex <- liftAff $ Index.readRegistryIndex indexPath
        let getDependencies = _.dependencies <<< un Manifest
        case Solver.solve (map (map getDependencies) registryIndex) (getDependencies manifest) of
          Left errors -> do
            throwError $ String.joinWith "\n"
              [ "Could not produce valid dependencies for manifest."
              , ""
              , errors # foldMapWithIndex \index error -> String.joinWith "\n"
                  [ "[Error " <> show (index + 1) <> "]"
                  , Solver.printSolverError error
                  ]
              ]
          Right solution ->
            pure $ BuildPlan $ plan { resolutions = Just solution }
      Just resolutions -> do
        Except.ExceptT $ validateResolutions manifest resolutions
        pure buildPlan

validateResolutions :: Manifest -> Map PackageName Version -> RegistryM (Either String Unit)
validateResolutions manifest resolutions = Except.runExceptT do
  let unresolvedDependencies = getUnresolvedDependencies manifest resolutions
  unless (Array.null unresolvedDependencies) do
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

    throwError $ String.joinWith "\n\n" $ Array.catMaybes
      [ Just "All dependencies from the manifest must be in the build plan at valid versions."
      , missingPackagesError
      , incorrectVersionsError
      ]

-- | Verifies that all dependencies in the manifest are present in the build
-- | plan, and the version listed in the build plan is within the range provided
-- | in the manifest. Note: this only checks dependencies listed in the manifest
-- | and will ignore transitive dependecies.
getUnresolvedDependencies :: Manifest -> Map PackageName Version -> Array (Either (PackageName /\ Range) (PackageName /\ Range /\ Version))
getUnresolvedDependencies (Manifest { dependencies }) resolutions =
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

type CompilePackage =
  { packageSourceDir :: FilePath
  , buildPlan :: BuildPlan
  }

compilePackage :: CompilePackage -> RegistryM (Either String FilePath)
compilePackage { packageSourceDir, buildPlan: BuildPlan plan } = do
  tmp <- liftEffect $ Tmp.mkTmpDir
  let dependenciesDir = Path.concat [ tmp, ".registry" ]
  liftAff $ FS.Extra.ensureDirectory dependenciesDir
  case plan.resolutions of
    Nothing -> do
      log "Compiling..."
      compilerOutput <- liftAff $ Purs.callCompiler
        { command: Purs.Compile { globs: [ "src/**/*.purs" ] }
        , version: Version.printVersion plan.compiler
        , cwd: Just packageSourceDir
        }
      pure (handleCompiler dependenciesDir compilerOutput)

    Just resolved -> do
      let (packages :: Array _) = Map.toUnfoldable resolved
      for_ packages (uncurry (installPackage dependenciesDir))
      log "Compiling..."
      compilerOutput <- liftAff $ Purs.callCompiler
        { command: Purs.Compile { globs: [ "src/**/*.purs", Path.concat [ dependenciesDir, "*/src/**/*.purs" ] ] }
        , version: Version.printVersion plan.compiler
        , cwd: Just packageSourceDir
        }
      pure (handleCompiler dependenciesDir compilerOutput)
  where
  -- We fetch every dependency at its resolved version, unpack the tarball, and
  -- store the resulting source code in a specified directory for dependencies.
  installPackage dir packageName version = do
    let
      -- This filename uses the format the directory name will have once
      -- unpacked, ie. package-name-major.minor.patch
      filename = PackageName.print packageName <> "-" <> Version.printVersion version <> ".tar.gz"
      filepath = Path.concat [ dir, filename ]

    liftAff (withBackoff' (Wget.wget (Constants.registryPackagesUrl <> "/" <> PackageName.print packageName <> "/" <> Version.printVersion version <> ".tar.gz") filepath)) >>= case _ of
      Nothing -> throwWithComment "Could not fetch tarball."
      Just (Left err) -> throwWithComment $ "Error while fetching tarball: " <> err
      Just (Right _) -> pure unit

    liftEffect $ Tar.extract { cwd: dir, archive: filename }
    liftAff $ FS.unlink filepath
    log $ "Installed " <> PackageName.print packageName <> "@" <> Version.printVersion version

  handleCompiler tmp = case _ of
    Right _ ->
      Right tmp
    Left MissingCompiler -> Left $ Array.fold
      [ "Compilation failed because the build plan compiler version "
      , Version.printVersion plan.compiler
      , " is not supported. Please try again with a different compiler."
      ]
    Left (CompilationError errs) -> Left $ String.joinWith "\n"
      [ "Compilation failed because the build plan does not compile with version " <> Version.printVersion plan.compiler <> " of the compiler:"
      , "```"
      , Purs.printCompilerErrors errs
      , "```"
      ]
    Left (UnknownError err) -> Left $ String.joinWith "\n"
      [ "Compilation failed for your package due to a compiler error:"
      , "```"
      , err
      , "```"
      ]

type PublishToPursuit =
  { packageSourceDir :: FilePath
  , dependenciesDir :: FilePath
  , buildPlan :: BuildPlan
  }

-- | Publishes a package to Pursuit.
-- |
-- | ASSUMPTIONS: This function should not be run on legacy packages or on
-- | packages where the `purescript-` prefix is still present.
publishToPursuit :: PublishToPursuit -> RegistryM (Either String String)
publishToPursuit { packageSourceDir, dependenciesDir, buildPlan: buildPlan@(BuildPlan { compiler }) } = Except.runExceptT do
  log "Generating a resolutions file"
  tmp <- liftEffect Tmp.mkTmpDir

  let
    resolvedPaths = buildPlanToResolutions { buildPlan, dependenciesDir }
    resolutionsFilePath = Path.concat [ tmp, "resolutions.json" ]

  liftAff $ Json.writeJsonFile resolutionsFilePath resolvedPaths

  -- NOTE: The compatibility version of purs publish appends 'purescript-' to the
  -- package name in the manifest file:
  -- https://github.com/purescript/purescript/blob/a846892d178d3c9c76c162ca39b9deb6fad4ec8e/src/Language/PureScript/Publish/Registry/Compat.hs#L19
  --
  -- The resulting documentation will all use purescript- prefixes in keeping
  -- with the format used by Pursuit in PureScript versions at least up to 0.16
  compilerOutput <- liftAff $ Purs.callCompiler
    { command: Purs.Publish { resolutions: resolutionsFilePath }
    , version: Version.printVersion compiler
    , cwd: Just packageSourceDir
    }

  publishJson <- case compilerOutput of
    Left MissingCompiler -> throwError $ Array.fold
      [ "Publishing failed because the build plan compiler version "
      , Version.printVersion compiler
      , " is not supported. Please try again with a different compiler."
      ]
    Left (CompilationError errs) -> throwError $ String.joinWith "\n"
      [ "Publishing failed because the build plan does not compile with version " <> Version.printVersion compiler <> " of the compiler:"
      , "```"
      , Purs.printCompilerErrors errs
      , "```"
      ]
    Left (UnknownError err) -> throwError $ String.joinWith "\n"
      [ "Publishing failed for your package due to an unknown compiler error:"
      , "```"
      , err
      , "```"
      ]
    Right publishResult -> do
      -- The output contains plenty of diagnostic lines, ie. "Compiling ..."
      -- but we only want the final JSON payload.
      let lines = String.split (String.Pattern "\n") publishResult
      case Array.last lines of
        Nothing -> throwError "Publishing failed because of an unexpected compiler error. cc @purescript/packaging"
        Just jsonString -> case Argonaut.Parser.jsonParser jsonString of
          Left err -> throwError $ String.joinWith "\n"
            [ "Failed to parse output of publishing. cc @purescript/packaging"
            , "```" <> err <> "```"
            ]
          Right json ->
            pure json

  authToken <- liftEffect (Node.Process.lookupEnv "PACCHETTIBOTTI_TOKEN") >>= case _ of
    Nothing -> do
      logShow =<< liftEffect Node.Process.getEnv
      throwError "Publishing failed because there is no available auth token. cc: @purescript/packaging"
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
      pure $ "Successfully uploaded package docs to Pursuit! 🎉 🚀"
    Right { body, status: StatusCode status } ->
      throwError $ String.joinWith "\n"
        [ "Expected a 201 response from Pursuit, but received " <> show status <> " instead (cc: @purescript/packaging)."
        , "Body:"
        , "```" <> body <> "```"
        ]
    Left err -> do
      let printedErr = Http.printError err
      throwError $ String.joinWith "\n" [ "Received a failed response from Pursuit (cc: @purescript/packaging): ", "```" <> printedErr <> "```" ]

-- Resolutions format: https://github.com/purescript/purescript/pull/3565
--
-- Note: This interfaces with Pursuit, and therefore we must add purescript-
-- prefixes to all package names for compatibility with the Bower naming format.
buildPlanToResolutions :: { buildPlan :: BuildPlan, dependenciesDir :: FilePath } -> Map RawPackageName { version :: Version, path :: FilePath }
buildPlanToResolutions { buildPlan: BuildPlan { resolutions }, dependenciesDir } =
  Map.fromFoldable do
    Tuple name version <- (Map.toUnfoldable (fromMaybe Map.empty resolutions) :: Array _)
    let
      bowerPackageName = RawPackageName ("purescript-" <> PackageName.print name)
      packagePath = Path.concat [ dependenciesDir, PackageName.print name <> "-" <> Version.printVersion version ]
    pure $ Tuple bowerPackageName { path: packagePath, version }

mkEnv :: GitHub.Octokit -> Cache -> Ref (Map PackageName Metadata) -> IssueNumber -> String -> Env
mkEnv octokit cache metadataRef issue username =
  { comment: \comment -> Except.runExceptT (GitHub.createComment octokit issue comment) >>= case _ of
      Left _ -> throwError $ Aff.error "Unable to create comment!"
      Right _ -> pure unit
  , closeIssue: Except.runExceptT (GitHub.closeIssue octokit issue) >>= case _ of
      Left _ -> throwError $ Aff.error "Unable to close issue!"
      Right _ -> pure unit
  , commitMetadataFile: pacchettiBottiPushToRegistryMetadata
  , commitIndexFile: pacchettiBottiPushToRegistryIndex
  , commitPackageSetFile: pacchettiBottiPushToRegistryPackageSets
  , uploadPackage: Upload.upload
  , deletePackage: Upload.delete
  , packagesMetadata: metadataRef
  , cache
  , octokit
  , username
  , registry: Path.concat [ scratchDir, "registry" ]
  , registryIndex: Path.concat [ scratchDir, "registry-index" ]
  }

fillMetadataRef :: RegistryM Unit
fillMetadataRef = do
  registryDir <- asks _.registry
  let metadataDir = registryMetadataPath registryDir
  packages <- liftAff do
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
          Aff.throwError $ Aff.error $ Parsing.parseErrorMessage err
      let metadataPath = metadataFile registryDir packageName
      metadata <- Json.readJsonFile metadataPath >>= case _ of
        Left err -> Aff.throwError $ Aff.error $ "Error parsing metadata file located at " <> metadataPath <> ": " <> err
        Right val -> pure val
      pure $ packageName /\ metadata
    pure $ Map.fromFoldable packagesArray
  metadataRef <- asks _.packagesMetadata
  liftEffect $ Ref.write packages metadataRef

isPackageVersionInMetadata :: PackageName -> Version -> Map PackageName Metadata -> Boolean
isPackageVersionInMetadata packageName version metadata =
  case Map.lookup packageName metadata of
    Nothing -> false
    Just packageMetadata -> isVersionInMetadata version packageMetadata

packageNameIsUnique :: PackageName -> Map PackageName Metadata -> Boolean
packageNameIsUnique name = isNothing <<< Map.lookup name

locationIsUnique :: Location -> Map PackageName Metadata -> Boolean
locationIsUnique location = Map.isEmpty <<< Map.filter (eq location <<< _.location)

-- | Fetch the latest from the given repository. Will perform a fresh clone if
-- | a checkout of the repository does not exist at the given path, and will
-- | pull otherwise.
fetchRepo :: GitHub.Address -> FilePath -> Aff Unit
fetchRepo address path = liftEffect (FS.Sync.exists path) >>= case _ of
  true -> do
    log $ "Found the " <> address.repo <> " repo locally, pulling..."
    result <- Except.runExceptT do
      branch <- Git.runGitSilent [ "rev-parse", "--abbrev-ref", "HEAD" ] (Just path)
      unless (branch == "main" || branch == "master") do
        throwError $ Array.fold
          [ "Cannot fetch using a branch other than 'main' or 'master'. Got: "
          , branch
          ]
      Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just path)
    case result of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> pure unit
  _ -> do
    log $ "Didn't find the " <> address.repo <> " repo, cloning..."
    Except.runExceptT (Git.runGit [ "clone", "https://github.com/" <> address.owner <> "/" <> address.repo <> ".git", path ] Nothing) >>= case _ of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> pure unit

fetchRegistryIndex :: RegistryM Unit
fetchRegistryIndex = do
  registryIndexPath <- asks _.registryIndex
  log "Fetching the most recent registry index..."
  liftAff $ fetchRepo Constants.registryIndexRepo registryIndexPath

fetchRegistry :: RegistryM Unit
fetchRegistry = do
  registryPath <- asks _.registry
  log "Fetching the most recent registry ..."
  liftAff $ fetchRepo Constants.registryRepo registryPath

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
        Git.cloneGitTag (i "https://github.com/" owner "/" repo) ref tmpDir
        log $ "Getting published time..."
        -- Cloning will result in the `repo` name as the directory name
        publishedTime <- Except.runExceptT (Git.gitGetRefTime ref (Path.concat [ tmpDir, repo ])) >>= case _ of
          Left error -> Aff.throwError $ Aff.error $ "Failed to get published time: " <> error
          Right value -> pure value
        pure { packageDirectory: Path.concat [ tmpDir, repo ], publishedTime }

      PursPublish -> do
        { octokit, cache } <- ask
        commitDate <- do
          result <- liftAff $ Except.runExceptT do
            commit <- GitHub.getRefCommit octokit cache { owner, repo } ref
            GitHub.getCommitDate octokit cache { owner, repo } commit
          case result of
            Left githubError -> throwWithComment $ "Unable to get published time for commit:\n" <> GitHub.printGitHubError githubError
            Right a -> pure a
        let tarballName = ref <> ".tar.gz"
        let absoluteTarballPath = Path.concat [ tmpDir, tarballName ]
        let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
        log $ "Fetching tarball from GitHub: " <> archiveUrl
        liftAff (Wget.wget archiveUrl absoluteTarballPath) >>= case _ of
          Left err -> throwWithComment $ "Error while fetching tarball: " <> err
          Right _ -> pure unit
        log $ "Tarball downloaded in " <> absoluteTarballPath
        liftEffect (Tar.getToplevelDir absoluteTarballPath) >>= case _ of
          Nothing ->
            throwWithComment "Could not find a toplevel dir in the tarball!"
          Just dir -> do
            log "Extracting the tarball..."
            liftEffect $ Tar.extract { cwd: tmpDir, archive: tarballName }
            pure { packageDirectory: dir, publishedTime: commitDate }

pacchettiBottiPushToRegistryIndex :: PackageName -> FilePath -> Aff (Either String Unit)
pacchettiBottiPushToRegistryIndex packageName registryIndexDir = Except.runExceptT do
  GitHubToken token <- Git.configurePacchettiBotti (Just registryIndexDir)
  Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryIndexDir)
  Git.runGit_ [ "add", Index.getIndexPath packageName ] (Just registryIndexDir)
  Git.runGit_ [ "commit", "-m", "Update manifests for package " <> PackageName.print packageName ] (Just registryIndexDir)
  let upstreamRepo = Constants.registryIndexRepo.owner <> "/" <> Constants.registryIndexRepo.repo
  let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
  void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryIndexDir)

pacchettiBottiPushToRegistryMetadata :: PackageName -> FilePath -> Aff (Either String Unit)
pacchettiBottiPushToRegistryMetadata packageName registryDir = Except.runExceptT do
  GitHubToken token <- Git.configurePacchettiBotti (Just registryDir)
  Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryDir)
  Git.runGit_ [ "add", Path.concat [ "metadata", PackageName.print packageName <> ".json" ] ] (Just registryDir)
  Git.runGit_ [ "commit", "-m", "Update metadata for package " <> PackageName.print packageName ] (Just registryDir)
  let upstreamRepo = Constants.registryRepo.owner <> "/" <> Constants.registryRepo.repo
  let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
  void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryDir)

pacchettiBottiPushToRegistryPackageSets :: Version -> String -> FilePath -> Aff (Either String Unit)
pacchettiBottiPushToRegistryPackageSets version commitMessage registryDir = Except.runExceptT do
  GitHubToken token <- Git.configurePacchettiBotti (Just registryDir)
  Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryDir)
  Git.runGit_ [ "add", Path.concat [ Constants.packageSetsPath, Version.printVersion version <> ".json" ] ] (Just registryDir)
  Git.runGit_ [ "commit", "-m", commitMessage ] (Just registryDir)
  let upstreamRepo = Constants.registryRepo.owner <> "/" <> Constants.registryRepo.repo
  let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
  void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryDir)

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
    makePaths path = { from: Path.concat [ source, path ], to: Path.concat [ destination, path ], preserveTimestamps: true }

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
  registryDir <- asks _.registry
  let metadataFilePath = metadataFile registryDir packageName
  liftEffect (FS.Sync.exists metadataFilePath) >>= case _ of
    false -> throwWithComment noMetadata
    _ -> pure unit

  readPackagesMetadata >>= \packages -> case Map.lookup packageName packages of
    Nothing -> throwWithComment "Couldn't read metadata file for your package.\ncc @purescript/packaging"
    Just m -> pure m

writeMetadata :: PackageName -> Metadata -> RegistryM (Either String Unit)
writeMetadata packageName metadata = do
  registryDir <- asks _.registry
  liftAff $ Json.writeJsonFile (metadataFile registryDir packageName) metadata
  updatePackagesMetadata packageName metadata
  commitMetadataFile packageName

writeInsertIndex :: Manifest -> RegistryM (Either String Unit)
writeInsertIndex manifest@(Manifest { name }) = do
  registryIndexDir <- asks _.registryIndex
  liftAff $ Index.insertManifest registryIndexDir manifest
  commitIndexFile name

writeDeleteIndex :: PackageName -> Version -> RegistryM (Either String Unit)
writeDeleteIndex name version = do
  registryIndexDir <- asks _.registryIndex
  liftAff $ Index.deleteManifest registryIndexDir name version
  commitIndexFile name

-- | Re-sign a payload as pacchettibotti if the authenticated operation was
-- | submitted by a registry trustee.
--
-- @pacchettibotti is considered an 'owner' of all packages for authenticated
-- operations. Registry trustees can ask pacchettibotti to perform an action on
-- behalf of a package by submitting a payload with the @pacchettibotti email
-- address. If the payload was submitted by a trustee (ie. a member of the
-- packaging team) then pacchettibotti will re-sign it and add itself as an
-- owner before continuing with the authenticated operation.
acceptTrustees
  :: String
  -> AuthenticatedData
  -> Maybe (NonEmptyArray Owner)
  -> RegistryM (Tuple AuthenticatedData (Maybe (NonEmptyArray Owner)))
acceptTrustees username authData@(AuthenticatedData authenticated) maybeOwners = do
  { octokit, cache } <- ask
  if authenticated.email /= Git.pacchettiBottiEmail then
    pure (Tuple authData maybeOwners)
  else do
    liftAff (Except.runExceptT (GitHub.listTeamMembers octokit cache packagingTeam)) >>= case _ of
      Left githubError -> throwWithComment $ Array.fold
        [ "This authenticated operation was opened using the pacchettibotti "
        , "email address, but we were unable to authenticate that you are a "
        , "member of the @purescript/packaging team:\n\n"
        , GitHub.printGitHubError githubError
        ]
      Right members -> do
        unless (Array.elem username (map _.username members)) do
          throwWithComment $ Array.fold
            [ "This authenticated operation was opened using the pacchettibotti "
            , "email address, but your username is not a member of the "
            , "@purescript/packaging team."
            ]

        { publicKey, privateKey } <- readPacchettiBottiKeys

        signature <- liftAff (SSH.signPayload { publicKey, privateKey, rawPayload: authenticated.rawPayload }) >>= case _ of
          Left _ -> throwWithComment "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature

        let
          newAuth = AuthenticatedData (authenticated { signature = signature })

          pacchettiBottiOwner = Owner
            { email: Git.pacchettiBottiEmail
            , keytype: pacchettiBottiKeyType
            , public: publicKey
            }

          ownersWithPacchettiBotti = case maybeOwners of
            Nothing -> NonEmptyArray.singleton pacchettiBottiOwner
            Just owners -> NonEmptyArray.cons pacchettiBottiOwner owners

        pure (Tuple newAuth (Just ownersWithPacchettiBotti))

-- | Read the PacchettiBotti SSH keypair from the environment.
--
-- PacchettiBotti's keys are stored in base64-encoded strings in the
-- environment. To regenerate SSH keys for pacchettibotti:
--
-- 1. Generate the keypair
-- $ ssh-keygen -t ed25519 -C "pacchettibotti@purescript.org"
--
-- 2. Encode the keypair (run this for both public and private):
-- $ cat id_ed25519 | base64 | tr -d \\n
-- $ cat id_ed25519.pub | base64 | tr -d \\n
--
-- 3. Store the results in 1Password and in GitHub secrets storage.
readPacchettiBottiKeys :: RegistryM { publicKey :: String, privateKey :: String }
readPacchettiBottiKeys = do
  publicKey <- liftEffect (Node.Process.lookupEnv "PACCHETTIBOTTI_ED25519_PUB") >>= case _ of
    Nothing -> throwWithComment "PACCHETTIBOTTI_ED25519_PUB not defined in the environment."
    Just b64Key -> case Base64.decode b64Key of
      Left b64Error -> throwWithComment $ "Failed to decode base64-encoded public key: " <> Aff.message b64Error
      Right decoded -> case verifyPublicKey (String.trim decoded) of
        Left error -> throwWithComment $ "Public key is malformed: " <> error
        Right key -> pure key

  privateKey <- liftEffect (Node.Process.lookupEnv "PACCHETTIBOTTI_ED25519") >>= case _ of
    Nothing -> throwWithComment "PACCHETTIBOTTI_ED25519 not defined in the environment."
    Just b64Key -> case Base64.decode b64Key of
      Left _ -> throwWithComment $ "Failed to decode base64-encoded private key."
      Right key -> pure (String.trim key)

  pure { publicKey, privateKey }
  where
  verifyPublicKey :: String -> Either String String
  verifyPublicKey decodedKey = do
    let split = String.split (String.Pattern " ") decodedKey

    keyFields <- note "Key must be of the form 'keytype key email'" do
      keyType <- Array.index split 0
      key <- Array.index split 1
      email <- Array.index split 2
      pure { keyType, key, email }

    if keyFields.keyType /= pacchettiBottiKeyType then
      Left $ Array.fold [ "Key type must be ", pacchettiBottiKeyType, " but received ", keyFields.keyType, " instead." ]
    else if keyFields.email /= Git.pacchettiBottiEmail then
      Left $ Array.fold [ "Email must be ", Git.pacchettiBottiEmail, " but received: ", keyFields.email, " instead." ]
    else
      pure keyFields.key

packagingTeam :: GitHub.Team
packagingTeam = { org: "purescript", team: "packaging" }

pacchettiBottiKeyType :: String
pacchettiBottiKeyType = "ssh-ed25519"

-- | An ignored directory suitable for storing results when running the API or
-- | scripts.
scratchDir :: String
scratchDir = "scratch"

data LegacyRegistryFile = BowerPackages | NewPackages

derive instance Eq LegacyRegistryFile

instance Show LegacyRegistryFile where
  show = case _ of
    BowerPackages -> "BowerPackages"
    NewPackages -> "NewPackages"

legacyRegistryFilePath :: FilePath -> LegacyRegistryFile -> FilePath
legacyRegistryFilePath registryPath file = Path.concat
  [ registryPath
  , case file of
      BowerPackages -> "bower-packages.json"
      NewPackages -> "new-packages.json"
  ]

-- | A helper function that syncs API operations to the new-packages.json or
-- | bower-packages.json files, namely registrations and transfers.
syncLegacyRegistry :: FilePath -> PackageName -> Location -> ExceptT String Aff Unit
syncLegacyRegistry registry package location = do
  packageUrl <- case location of
    GitHub { owner, repo } -> pure $ GitHub.PackageURL $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
    _ -> throwError "Packages must come from GitHub."

  let newPackagesPath = legacyRegistryFilePath registry NewPackages
  let bowerPackagesPath = legacyRegistryFilePath registry BowerPackages

  newPackages <- Except.ExceptT $ Json.readJsonFile newPackagesPath
  bowerPackages <- Except.ExceptT $ Json.readJsonFile bowerPackagesPath

  let
    rawPackageName = "purescript-" <> PackageName.print package

    -- Here we determine which, if any, legacy registry file should be updated with this package.
    -- If the package is new (ie. not listed in either registry file) then we insert it into the
    -- new-packages.json file. If not (ie. we found it in one of the registry files), and the location
    -- of the package in the registry file is different from its one in the registry metadata, then we
    -- update the package in that registry file. If the package exists at the proper location already
    -- then we do nothing.
    targetFile = case Map.lookup rawPackageName newPackages, Map.lookup rawPackageName bowerPackages of
      Nothing, Nothing -> Just NewPackages
      Just url, _
        | url /= packageUrl -> Just NewPackages
        | otherwise -> Nothing
      _, Just url
        | url /= packageUrl -> Just BowerPackages
        | otherwise -> Nothing

  for_ targetFile \target -> do
    let sourcePackages = if target == NewPackages then newPackages else bowerPackages
    let sourceFile = if target == NewPackages then newPackagesPath else bowerPackagesPath
    let packages = Map.insert rawPackageName packageUrl sourcePackages
    liftAff $ Json.writeJsonFile sourceFile packages
    -- A simple cehck to verify that a change was indeed written to disk, before
    -- we attempt to commit and push.
    Git.runGitSilent [ "diff", "--stat" ] (Just registry) >>= case _ of
      files | String.contains (String.Pattern sourceFile) files -> do
        GitHubToken token <- Git.configurePacchettiBotti (Just registry)
        Git.runGit_ [ "pull" ] (Just registry)
        Git.runGit_ [ "add", sourceFile ] (Just registry)
        let message = Array.fold [ "Mirror registry API operation to ", sourceFile ]
        Git.runGit_ [ "commit", "-m", message ] (Just registry)
        let upstreamRepo = Constants.registryRepo.owner <> "/" <> Constants.registryRepo.repo
        let origin = "https://pacchettibotti:" <> token <> "@github.com" <> upstreamRepo <> ".git"
        void $ Git.runGitSilent [ "push", origin, "main" ] Nothing
      _ ->
        log "No changes to commit."
