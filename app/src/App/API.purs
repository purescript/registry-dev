module Registry.App.API where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Ansi.Codes (GraphicsParam)
import Control.Alternative as Alternative
import Control.Monad.Except as Except
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HTTP.Method (Method(..))
import Data.HTTP.Method as Method
import Data.Map as Map
import Data.MediaType.Common as MediaType
import Data.Newtype (unwrap)
import Data.Number.Format as Number.Format
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Dodo (Doc)
import Effect.Aff as Aff
import Node.Buffer as Buffer
import Node.ChildProcess as ChildProcess
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.Auth as Auth
import Registry.App.CLI.Git as Git
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.Cache (CACHE)
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Notify (NOTIFY)
import Registry.App.Effect.Notify as Notify
import Registry.App.Effect.PackageSets (Change(..), PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), rawPackageNameMapCodec)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (GitHubToken(..), Team)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tar as Foreign.Tar
import Registry.Foreign.Tmp as Tmp
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PackageOperation(..), PackageSetOperation(..), PublishData)
import Registry.Operation.Validation as Operation.Validation
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except as Run.Except
import Sunde as Sunde

-- | Operations are exercised via the API and the legacy importer. If the
-- | importer is used then we don't compile or publish docs for the package.
-- | If the API is used for a 'legacy' package (ie. a Spago-based project), then
-- | we attempt to compile the package and warn if compilation fails. If the API
-- | is used for a normal package then failed compilation fails the pipeline.
data Source = API | Importer

derive instance Eq Source

type AppEffects = (REGISTRY + PACKAGE_SETS + STORAGE + GITHUB + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + NOTIFY + CACHE + LOG + LOG_EXCEPT + AFF + EFFECT ())

-- TODO: This has been converted to use effects, but it's still essentially
-- hardcoded to a GitHub event (such as referring to usernames), and needs to
-- be reworked in the future to support the HTTP API.
runOperation :: Source -> Either PackageSetOperation PackageOperation -> Run AppEffects Unit
runOperation source operation = case operation of
  Right (Publish fields@{ name, location }) -> do
    allMetadata <- Registry.readAllMetadata
    case Map.lookup name allMetadata of
      Just metadata ->
        case location of
          -- The user can add a new version of their package if it comes from
          -- the same location listed in the metadata OR if they do not provide
          -- a location.
          Nothing ->
            publish source fields metadata
          Just packageLocation | (un Metadata metadata).location == packageLocation ->
            publish source fields metadata
          -- Otherwise, if they attempted to re-register the package under a new
          -- location, then they either did not know the package already existed or
          -- they are attempting a transfer.
          Just _ -> do
            Log.error $ PackageName.print name <> " is already registered, aborting."
            Log.exit $ String.joinWith " "
              [ "Cannot register"
              , PackageName.print name
              , "because it has already been registered.\nIf you are attempting to"
              , "register your package, please choose a different package name."
              , "If you are attempting to transfer this package to a new location,"
              , "please submit a transfer instead."
              ]

      -- If this is a brand-new package, then we can allow them to register it
      -- so long as they aren't publishing an existing location under a new name
      Nothing -> case location of
        Nothing -> do
          Log.error "No location field provided, aborting."
          Log.exit $ String.joinWith " "
            [ "Cannot register"
            , PackageName.print name
            , "because no 'location' field was provided."
            ]
        Just packageLocation | not (Operation.Validation.locationIsUnique packageLocation allMetadata) -> do
          Log.error "Location already in use, aborting."
          Log.exit $ String.joinWith " "
            [ "Cannot register"
            , PackageName.print name
            , "at the location"
            , stringifyJson Location.codec packageLocation
            , "because that location is already in use to publish another package."
            ]
        Just packageLocation ->
          publish source fields (mkNewMetadata packageLocation)

  Left (PackageSetUpdate { compiler, packages }) -> do
    { username } <- Env.askGitHubEvent

    latestPackageSet <- Registry.readLatestPackageSet >>= case _ of
      Nothing -> Log.exit "There is no latest package set."
      Just set -> pure set

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
      GitHub.listTeamMembers packagingTeam >>= case _ of
        Left githubError -> Log.exit $ Array.fold
          [ "This package set update changes the compiler version or removes a "
          , "package from the package set. Only members of the "
          , "@purescript/packaging team can take these actions, but we were "
          , "unable to authenticate your account:\n"
          , Octokit.printGitHubError githubError
          ]
        Right members -> do
          unless (Array.elem username members) do
            Log.exit $ String.joinWith " "
              [ "This package set update changes the compiler version or"
              , "removes a package from the package set. Only members of the"
              , "@purescript/packaging team can take these actions, but your"
              , "username is not a member of the packaging team."
              ]

    -- The compiler version cannot be downgraded.
    for_ compiler \version -> when (version < prevCompiler) do
      Log.exit $ String.joinWith " "
        [ "You are downgrading the compiler used in the package set from"
        , "the current version (" <> Version.print prevCompiler <> ")"
        , "to the lower version (" <> Version.print version <> ")."
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
          Alternative.guard (prevVersion > newVersion)
          pure (Tuple packageName { old: prevVersion, new: newVersion })

    when (not (Array.null downgradedPackages)) do
      let
        formatPackage (Tuple name { old, new }) = Array.fold
          [ "  - "
          , PackageName.print name
          , " from "
          , Version.print old
          , " to "
          , Version.print new
          ]

      Log.exit $ Array.fold
        [ "You are attempting to downgrade one or more package versions from "
        , "their version in the previous set. Affected packages:\n\n"
        , String.joinWith "\n" $ map formatPackage downgradedPackages
        ]

    -- With these conditions met, we can attempt to process the batch with the
    -- new packages and/or compiler version. Note: if the compiler is updated to
    -- a version that isn't supported by the registry then an 'unsupported
    -- compiler' error will be thrown.
    manifestIndex <- Registry.readAllManifests

    -- TODO: The candidates really ought to be a ChangeSet, and the validation
    -- function should probably be in Run and only fetch the manifests it needs
    -- on-demand instead of requiring the entire index.
    PackageSets.validatePackageSet latestPackageSet
    let candidates = PackageSets.validatePackageSetCandidates manifestIndex latestPackageSet packages

    unless (Map.isEmpty candidates.rejected) do
      Log.exit $ String.joinWith "\n"
        [ "One or more packages in the suggested batch cannot be processed.\n"
        , PackageSets.printRejections candidates.rejected
        ]

    if Map.isEmpty candidates.accepted then do
      Log.exit "No packages in the suggested batch can be processed; all failed validation checks."
    else do
      let changeSet = candidates.accepted <#> maybe Remove Update
      Notify.notify "Attempting to build package set update."
      PackageSets.upgradeAtomic latestPackageSet (fromMaybe (un PackageSet latestPackageSet).compiler compiler) changeSet >>= case _ of
        Just packageSet -> do
          let commitMessage = PackageSets.commitMessage latestPackageSet changeSet (un PackageSet packageSet).version
          Registry.writePackageSet packageSet commitMessage
          Notify.notify "Built and released a new package set! Now mirroring to the package-sets repo..."
          Registry.mirrorPackageSet packageSet
          Notify.notify "Mirrored a new legacy package set."
        _ -> do
          Log.exit "The package set produced from this suggested update does not compile."

  Right (Authenticated submittedAuth@{ payload }) -> case payload of
    Unpublish { name, version, reason } -> do
      Metadata metadata <- Registry.readMetadata name >>= case _ of
        Nothing -> Log.exit "No metadata found for your package. Only published packages can be unpublished."
        Just content -> pure content

      now <- Run.liftEffect nowUTC
      publishedMetadata <- case Operation.Validation.validateUnpublish now version (Metadata metadata) of
        Left Operation.Validation.NotPublished ->
          Log.exit $ "Cannot unpublish " <> Version.print version <> " because it is not a published version."
        Left Operation.Validation.AlreadyUnpublished ->
          Log.exit $ "Cannot unpublish " <> Version.print version <> " because it has already been unpublished."
        Left Operation.Validation.InternalError ->
          Log.exit $ String.joinWith "\n"
            [ "Cannot unpublish " <> Version.print version <> "."
            , ""
            , "This version is listed both as published and unpublished. This is an internal error."
            , "cc @purescript/packaging"
            ]
        Left (Operation.Validation.PastTimeLimit { difference, limit }) ->
          Log.exit $ "Packages can only be unpublished within " <> Number.Format.toString (unwrap limit) <> " hours. Package was published " <> Number.Format.toString (unwrap difference) <> " hours ago."
        Right published ->
          pure published

      Tuple auth maybeOwners <- acceptTrustees submittedAuth metadata.owners

      case maybeOwners of
        Nothing ->
          Log.exit $ String.joinWith " "
            [ "Cannot verify package ownership because no owners are listed in the package metadata."
            , "Please publish a package version with your SSH public key in the owners field."
            , "You can then retry unpublishing this version by authenticating with your private key."
            ]
        Just owners -> Run.liftAff (Auth.verifyPayload owners auth) >>= case _ of
          Left err -> Log.exit $ String.joinWith "\n"
            [ "Failed to verify package ownership:"
            , err
            ]
          Right _ -> do
            let unpublishedMetadata = { reason, publishedTime: publishedMetadata.publishedTime, unpublishedTime: now }
            let updatedMetadata = unpublishVersionInMetadata version unpublishedMetadata (Metadata metadata)
            Storage.deleteTarball name version
            Registry.writeMetadata name updatedMetadata
            Registry.deleteManifest name version
            Notify.notify $ "Unpublished package " <> formatPackageVersion name version

    Transfer { name, newLocation } -> do
      Metadata metadata <- Registry.readMetadata name >>= case _ of
        Nothing -> Log.exit $ String.joinWith " "
          [ "No metadata found for your package."
          , "You can only transfer packages that have already been published."
          , "Did you mean to publish your package?"
          ]
        Just content -> pure content

      allMetadata <- Registry.readAllMetadata

      let
        isUniqueLocation = Operation.Validation.locationIsUnique newLocation allMetadata
        notUniqueError = String.joinWith " "
          [ "Cannot transfer"
          , PackageName.print name
          , " to "
          , stringifyJson Location.codec newLocation
          , "because another package is already registered at that location."
          ]

      case source of
        Importer | not isUniqueLocation ->
          Log.warn notUniqueError
        API | not isUniqueLocation ->
          Log.exit notUniqueError
        _ -> do
          Tuple auth maybeOwners <- acceptTrustees submittedAuth metadata.owners

          case maybeOwners of
            Nothing ->
              Log.exit $ String.joinWith " "
                [ "Cannot verify package ownership because no owners are listed in the package metadata."
                , "Please publish a package version with your SSH public key in the owners field."
                , "You can then retry transferring this package by authenticating with your private key."
                ]
            Just owners ->
              Run.liftAff (Auth.verifyPayload owners auth) >>= case _ of
                Left err ->
                  Log.exit $ String.joinWith "\n"
                    [ "Failed to verify package ownership:"
                    , "  " <> err
                    ]
                Right _ -> do
                  let updatedMetadata = metadata { location = newLocation }
                  Registry.writeMetadata name (Metadata updatedMetadata)
                  Notify.notify "Successfully transferred your package!"
                  Registry.mirrorLegacyRegistry name newLocation
                  Notify.notify "Mirrored location change to the legacy registry."

jsonToDhallManifest :: String -> Aff (Either String String)
jsonToDhallManifest jsonStr = do
  let cmd = "json-to-dhall"
  let stdin = Just jsonStr
  let args = [ "--records-loose", "--unions-strict", "." <> Path.sep <> Path.concat [ "types", "v1", "Manifest.dhall" ] ]
  result <- Sunde.spawn { cmd, stdin, args } ChildProcess.defaultSpawnOptions
  pure $ case result.exit of
    ChildProcess.Normally 0 -> Right jsonStr
    _ -> Left result.stderr

publish
  :: forall r
   . Source
  -> PublishData
  -> Metadata
  -> Run (REGISTRY + STORAGE + GITHUB + PACCHETTIBOTTI_ENV + CACHE + NOTIFY + LOG + LOG_EXCEPT + AFF + EFFECT + r) Unit
publish source publishData@{ name, ref, compiler, resolutions } (Metadata inputMetadata) = do
  tmpDir <- liftEffect $ Tmp.mkTmpDir

  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  { packageDirectory, publishedTime } <- fetchPackageSource { tmpDir, ref, location: inputMetadata.location }

  let manifestPath = Path.concat [ packageDirectory, "purs.json" ]

  Log.debug $ "Package available in " <> packageDirectory

  Log.debug "Verifying that the package contains a `src` directory"
  whenM (liftAff $ Operation.Validation.containsPursFile (Path.concat [ packageDirectory, "src" ])) do
    Log.exit "This package has no .purs files in the src directory. All package sources must be in the `src` directory, with any additional sources indicated by the `files` key in your manifest."

  -- If this is a legacy import, then we need to construct a `Manifest` for it.
  isLegacyImport <- liftEffect $ map not $ FS.Sync.exists manifestPath
  when isLegacyImport do
    Notify.notify $ "Package source does not have a purs.json file. Creating one from your bower.json and/or spago.dhall files..."
    address <- case inputMetadata.location of
      Git _ -> Log.exit "Legacy packages can only come from GitHub. Aborting."
      GitHub { owner, repo } -> pure { owner, repo }

    version <- case LenientVersion.parse ref of
      Left _ -> Log.exit $ "Not a valid registry version: " <> ref
      Right result -> pure $ LenientVersion.version result

    Legacy.Manifest.fetchLegacyManifest name address (RawVersion ref) >>= case _ of
      Left manifestError -> do
        let formatError { error, reason } = reason <> " " <> Legacy.Manifest.printLegacyManifestError error
        Log.exit $ String.joinWith "\n"
          [ "There were problems with the legacy manifest file:"
          , formatError manifestError
          ]
      Right legacyManifest -> do
        let manifest = Legacy.Manifest.toManifest name version inputMetadata.location legacyManifest
        liftAff $ writeJsonFile Manifest.codec manifestPath manifest

  -- Try to read the manifest, typechecking it
  manifest@(Manifest manifestFields) <- liftAff (try $ FS.Aff.readTextFile UTF8 manifestPath) >>= case _ of
    Left _err -> Log.exit $ "Manifest not found at " <> manifestPath
    Right manifestStr -> liftAff (jsonToDhallManifest manifestStr) >>= case _ of
      Left err -> Log.exit $ "Could not typecheck manifest: " <> err
      Right _ -> case parseJson Manifest.codec manifestStr of
        Left err -> Log.exit $ "Could not parse manifest as JSON: " <> CA.printJsonDecodeError err
        Right res -> pure res

  Notify.notify "Verifying package..."

  -- We trust the manifest for any changes to the 'owners' field, but for all
  -- other fields we trust the registry metadata.
  let metadata = inputMetadata { owners = manifestFields.owners }
  when (not isLegacyImport && not (Operation.Validation.nameMatches manifest publishData)) do
    Log.exit $ Array.fold
      [ "The manifest file specifies a package name ("
      , PackageName.print manifestFields.name
      , ") that differs from the package name submitted to the API ("
      , PackageName.print name
      , "). The manifest and API request must match."
      ]

  unless (Operation.Validation.locationMatches manifest (Metadata metadata)) do
    Log.exit $ Array.fold
      [ "The manifest file specifies a location ("
      , stringifyJson Location.codec manifestFields.location
      , ") that differs from the location in the registry metadata ("
      , stringifyJson Location.codec metadata.location
      , "). If you would like to change the location of your package you should "
      , "submit a Transfer operation."
      ]

  -- Then, we can run verification checks on the manifest and either verify the
  -- provided build plan or produce a new one.
  verifyManifest { metadata: Metadata metadata, manifest }
  verifiedResolutions <- verifyResolutions { resolutions, manifest }

  -- After we pass all the checks it's time to do side effects and register the package
  Log.info "Packaging the tarball to upload..."
  -- We need the version number to upload the package
  let newVersion = manifestFields.version
  let newDirname = PackageName.print name <> "-" <> Version.print newVersion
  let packageSourceDir = Path.concat [ tmpDir, newDirname ]
  Run.liftAff $ FS.Extra.ensureDirectory packageSourceDir
  -- We copy over all files that are always included (ie. src dir, purs.json file),
  -- and any files the user asked for via the 'files' key, and remove all files
  -- that should never be included (even if the user asked for them).
  copyPackageSourceFiles manifestFields.files { source: packageDirectory, destination: packageSourceDir }
  Run.liftAff $ removeIgnoredTarballFiles packageSourceDir
  let tarballPath = packageSourceDir <> ".tar.gz"
  Run.liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname }
  Log.debug "Checking the tarball size..."
  FS.Stats.Stats { size: bytes } <- liftAff $ FS.Aff.stat tarballPath
  for_ (Operation.Validation.validateTarballSize bytes) case _ of
    Operation.Validation.ExceedsMaximum maxPackageBytes ->
      Log.exit $ "Package tarball is " <> show bytes <> " bytes, which exceeds the maximum size of " <> show maxPackageBytes <> " bytes.\ncc: @purescript/packaging"
    Operation.Validation.WarnPackageSize maxWarnBytes ->
      Notify.notify $ "WARNING: Package tarball is " <> show bytes <> "bytes, which exceeds the warning threshold of " <> show maxWarnBytes <> " bytes.\ncc: @purescript/packaging"

  Log.debug "Hashing the tarball..."
  hash <- Run.liftAff $ Sha256.hashFile tarballPath
  Log.debug $ "Hash: " <> Sha256.print hash

  -- Now that we have the package source contents we can verify we can compile
  -- the package. We skip failures when the package is a legacy package.
  compilationResult <- compilePackage { packageSourceDir: packageDirectory, compiler, resolutions: verifiedResolutions }

  case compilationResult of
    Left error
      -- We allow bulk-uploaded legacy packages to fail compilation because we
      -- do not necessarily know what compiler to use with them.
      | source == Importer -> do
          Log.debug error
          Log.warn "Failed to compile, but continuing because this package is coming from the importer"
      | otherwise ->
          Log.exit error
    Right _ ->
      pure unit

  Notify.notify "Uploading package to the storage backend..."
  Storage.uploadTarball name newVersion tarballPath
  Log.debug $ "Adding the new version " <> Version.print newVersion <> " to the package metadata file."
  let newMetadata = addVersionToMetadata newVersion { hash, ref, publishedTime, bytes } (Metadata metadata)
  Registry.writeMetadata name newMetadata
  Notify.notify "Successfully uploaded package to the registry! 🎉 🚀"

  -- After a package has been uploaded we add it to the registry index, we
  -- upload its documentation to Pursuit, and we can now process it for package
  -- sets when the next batch goes out.

  -- We write to the registry index if possible. If this fails, the packaging
  -- team should manually insert the entry.
  Registry.writeManifest manifest

  when (source == API) $ case compilationResult of
    Left error ->
      Notify.notify $ Array.fold [ "Skipping Pursuit publishing because this package failed to compile:\n", error ]
    Right dependenciesDir -> do
      Log.debug "Uploading to Pursuit"
      publishToPursuit { packageSourceDir: packageDirectory, compiler, resolutions: verifiedResolutions, dependenciesDir } >>= case _ of
        Left message -> Notify.notify message
        Right _ -> Notify.notify "Successfully published docs to pursuit!"

  Registry.mirrorLegacyRegistry name (un Metadata newMetadata).location
  Notify.notify "Mirrored location change to the legacy registry."

verifyManifest :: forall r. { metadata :: Metadata, manifest :: Manifest } -> Run (REGISTRY + LOG + LOG_EXCEPT + r) Unit
verifyManifest { metadata, manifest } = do
  let Manifest manifestFields = manifest

  -- TODO: collect all errors and return them at once. Note: some of the checks
  -- are going to fail while parsing from JSON, so we should move them here if we
  -- want to handle everything together
  Log.debug $ "Running checks for the following manifest:\n" <> printJson Manifest.codec manifest

  Log.debug "Ensuring the package is not the purescript-metadata package, which cannot be published."
  when (Operation.Validation.isMetadataPackage manifest) do
    Log.exit "The `metadata` package cannot be uploaded to the registry as it is a protected package."

  Log.debug "Check that version has not already been published"
  for_ (Operation.Validation.isNotPublished manifest metadata) \info -> do
    Log.exit $ String.joinWith "\n"
      [ "You tried to upload a version that already exists: " <> Version.print manifestFields.version
      , "Its metadata is:"
      , "```json"
      , printJson Metadata.publishedMetadataCodec info
      , "```"
      ]

  Log.debug "Check that version has not been unpublished"
  for_ (Operation.Validation.isNotUnpublished manifest metadata) \info -> do
    Log.exit $ String.joinWith "\n"
      [ "You tried to upload a version that has been unpublished: " <> Version.print manifestFields.version
      , ""
      , "```json"
      , printJson Metadata.unpublishedMetadataCodec info
      , "```"
      ]

  Log.debug "Check that all dependencies are contained in the registry"
  allMetadata <- Registry.readAllMetadata

  let
    pkgNotInRegistry name = case Map.lookup name allMetadata of
      Nothing -> Just name
      Just _p -> Nothing
    pkgsNotInRegistry =
      Array.mapMaybe pkgNotInRegistry $ Set.toUnfoldable $ Map.keys manifestFields.dependencies

  unless (Array.null pkgsNotInRegistry) do
    Log.exit $ "Some dependencies of your package were not found in the Registry: " <> String.joinWith ", " (map PackageName.print pkgsNotInRegistry)

-- | Verify the build plan for the package. If the user provided a build plan,
-- | we ensure that the provided versions are within the ranges listed in the
-- | manifest. If not, we solve their manifest to produce a build plan.
verifyResolutions
  :: forall r
   . { resolutions :: Maybe (Map PackageName Version), manifest :: Manifest }
  -> Run (REGISTRY + LOG + LOG_EXCEPT + r) (Map PackageName Version)
verifyResolutions { resolutions, manifest } = do
  Log.debug "Check the submitted build plan matches the manifest"
  -- We don't verify packages provided via the mass import.
  manifestIndex <- Registry.readAllManifests
  case resolutions of
    Nothing -> case Operation.Validation.validateDependenciesSolve manifest manifestIndex of
      Left errors -> do
        let
          printedError = String.joinWith "\n"
            [ "Could not produce valid dependencies for manifest."
            , ""
            , errors # foldMapWithIndex \index error -> String.joinWith "\n"
                [ "[Error " <> show (index + 1) <> "]"
                , Solver.printSolverError error
                ]
            ]
        Log.exit printedError
      Right solved -> pure solved
    Just provided -> do
      validateResolutions manifest provided
      pure provided

validateResolutions :: forall r. Manifest -> Map PackageName Version -> Run (LOG_EXCEPT + r) Unit
validateResolutions manifest resolutions = do
  let unresolvedDependencies = Operation.Validation.getUnresolvedDependencies manifest resolutions
  unless (Array.null unresolvedDependencies) do
    let
      { fail: missingPackages, success: incorrectVersions } = partitionEithers unresolvedDependencies

      printPackageRange (name /\ range) = Array.fold
        [ "`"
        , PackageName.print name
        , "` in range `"
        , Range.print range
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
        , Version.print version
        , "` does not satisfy range `"
        , Range.print range
        , "`"
        ]

      incorrectVersionsError = do
        guardA (not Array.null incorrectVersions)
        pure
          $ String.joinWith "\n  - "
          $ Array.cons "The build plan provides dependencies at versions outside the range listed in the manifest:"
          $ map printPackageVersion incorrectVersions

    Log.exit $ String.joinWith "\n\n" $ Array.catMaybes
      [ Just "All dependencies from the manifest must be in the build plan at valid versions."
      , missingPackagesError
      , incorrectVersionsError
      ]

type CompilePackage =
  { packageSourceDir :: FilePath
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

compilePackage
  :: forall r
   . CompilePackage
  -> Run (STORAGE + LOG + AFF + EFFECT + r) (Either String FilePath)
compilePackage { packageSourceDir, compiler, resolutions } = do
  tmp <- Run.liftEffect $ Tmp.mkTmpDir
  let dependenciesDir = Path.concat [ tmp, ".registry" ]
  Run.liftAff $ FS.Extra.ensureDirectory dependenciesDir
  let
    globs =
      if Map.isEmpty resolutions then
        [ "src/**/*.purs" ]
      else
        [ "src/**/*.purs"
        , Path.concat [ dependenciesDir, "*/src/**/*.purs" ]
        ]
  forWithIndex_ resolutions (installPackage dependenciesDir)
  Log.debug "Compiling..."
  compilerOutput <- Run.liftAff $ Purs.callCompiler
    { command: Purs.Compile { globs }
    , version: Version.print compiler
    , cwd: Just packageSourceDir
    }
  pure (handleCompiler dependenciesDir compilerOutput)
  where
  -- We fetch every dependency at its resolved version, unpack the tarball, and
  -- store the resulting source code in a specified directory for dependencies.
  installPackage dir name version = do
    let
      -- This filename uses the format the directory name will have once
      -- unpacked, ie. package-name-major.minor.patch
      filename = PackageName.print name <> "-" <> Version.print version <> ".tar.gz"
      filepath = Path.concat [ dir, filename ]

    Storage.downloadTarball name version filepath

    Run.liftEffect $ Tar.extract { cwd: dir, archive: filename }
    Run.liftAff $ FS.Aff.unlink filepath
    Log.debug $ "Installed " <> formatPackageVersion name version

  handleCompiler tmp = case _ of
    Right _ ->
      Right tmp
    Left MissingCompiler -> Left $ Array.fold
      [ "Compilation failed because the build plan compiler version "
      , Version.print compiler
      , " is not supported. Please try again with a different compiler."
      ]
    Left (CompilationError errs) -> Left $ String.joinWith "\n"
      [ "Compilation failed because the build plan does not compile with version " <> Version.print compiler <> " of the compiler:"
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
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

-- | Publishes a package to Pursuit.
-- |
-- | ASSUMPTIONS: This function should not be run on legacy packages or on
-- | packages where the `purescript-` prefix is still present.
publishToPursuit
  :: forall r
   . PublishToPursuit
  -> Run (PACCHETTIBOTTI_ENV + LOG + NOTIFY + AFF + EFFECT + r) (Either (Doc GraphicsParam) Unit)
publishToPursuit { packageSourceDir, dependenciesDir, compiler, resolutions } = Run.Except.runExceptAt Log._logExcept do
  { token } <- Env.askPacchettiBotti

  Log.debug "Generating a resolutions file"
  tmp <- Run.liftEffect Tmp.mkTmpDir

  let
    resolvedPaths = formatPursuitResolutions { resolutions, dependenciesDir }
    resolutionsFilePath = Path.concat [ tmp, "resolutions.json" ]

  Run.liftAff $ writeJsonFile pursuitResolutionsCodec resolutionsFilePath resolvedPaths

  -- NOTE: The compatibility version of purs publish appends 'purescript-' to the
  -- package name in the manifest file:
  -- https://github.com/purescript/purescript/blob/a846892d178d3c9c76c162ca39b9deb6fad4ec8e/src/Language/PureScript/Publish/Registry/Compat.hs#L19
  --
  -- The resulting documentation will all use purescript- prefixes in keeping
  -- with the format used by Pursuit in PureScript versions at least up to 0.16
  compilerOutput <- Run.liftAff $ Purs.callCompiler
    { command: Purs.Publish { resolutions: resolutionsFilePath }
    , version: Version.print compiler
    , cwd: Just packageSourceDir
    }

  publishJson <- case compilerOutput of
    Left MissingCompiler -> Log.exit $ Array.fold
      [ "Publishing failed because the build plan compiler version "
      , Version.print compiler
      , " is not supported. Please try again with a different compiler."
      ]
    Left (CompilationError errs) -> Log.exit $ String.joinWith "\n"
      [ "Publishing failed because the build plan does not compile with version " <> Version.print compiler <> " of the compiler:"
      , "```"
      , Purs.printCompilerErrors errs
      , "```"
      ]
    Left (UnknownError err) -> Log.exit $ String.joinWith "\n"
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
        Nothing -> Log.exit "Publishing failed because of an unexpected compiler error. cc @purescript/packaging"
        Just jsonString -> case Argonaut.Parser.jsonParser jsonString of
          Left err -> Log.exit $ String.joinWith "\n"
            [ "Failed to parse output of publishing. cc @purescript/packaging"
            , "```"
            , err
            , "```"
            ]
          Right json ->
            pure json

  Log.debug "Pushing to Pursuit..."
  result <- Run.liftAff $ Affjax.Node.request
    { content: Just $ RequestBody.json publishJson
    , headers:
        [ RequestHeader.Accept MediaType.applicationJSON
        , RequestHeader.RequestHeader "Authorization" ("token " <> un GitHubToken token)
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
      Notify.notify "Successfully uploaded package docs to Pursuit! 🎉 🚀"
    Right { body, status: StatusCode status } ->
      Log.exit $ String.joinWith "\n"
        [ "Expected a 201 response from Pursuit, but received " <> show status <> " instead (cc: @purescript/packaging)."
        , ""
        , "```"
        , body
        , "```"
        ]
    Left err -> do
      let printedErr = Affjax.Node.printError err
      Log.exit $ String.joinWith "\n"
        [ "Received a failed response from Pursuit (cc: @purescript/packaging): "
        , "```"
        , printedErr
        , "```"
        ]

type PursuitResolutions = Map RawPackageName { version :: Version, path :: FilePath }

pursuitResolutionsCodec :: JsonCodec PursuitResolutions
pursuitResolutionsCodec = rawPackageNameMapCodec $ CA.Record.object "Resolution" { version: Version.codec, path: CA.string }

-- Resolutions format: https://github.com/purescript/purescript/pull/3565
--
-- Note: This interfaces with Pursuit, and therefore we must add purescript-
-- prefixes to all package names for compatibility with the Bower naming format.
formatPursuitResolutions :: { resolutions :: Map PackageName Version, dependenciesDir :: FilePath } -> PursuitResolutions
formatPursuitResolutions { resolutions, dependenciesDir } =
  Map.fromFoldable do
    Tuple name version <- Map.toUnfoldable resolutions
    let
      bowerPackageName = RawPackageName ("purescript-" <> PackageName.print name)
      packagePath = Path.concat [ dependenciesDir, PackageName.print name <> "-" <> Version.print version ]
    [ Tuple bowerPackageName { path: packagePath, version } ]

isPackageVersionInMetadata :: PackageName -> Version -> Map PackageName Metadata -> Boolean
isPackageVersionInMetadata packageName version metadata =
  case Map.lookup packageName metadata of
    Nothing -> false
    Just packageMetadata -> isVersionInMetadata version packageMetadata

packageNameIsUnique :: PackageName -> Map PackageName Metadata -> Boolean
packageNameIsUnique name = isNothing <<< Map.lookup name

data PursPublishMethod = LegacyPursPublish | PursPublish

-- | A temporary flag that records whether we are using legacy purs publish
-- | (which requires all packages to be a Git repository) or new purs publish
-- | (which accepts any directory with package sources).
pursPublishMethod :: PursPublishMethod
pursPublishMethod = LegacyPursPublish

fetchPackageSource
  :: forall r
   . { tmpDir :: FilePath, ref :: String, location :: Location }
  -> Run (GITHUB + LOG + LOG_EXCEPT + AFF + EFFECT + r) { packageDirectory :: FilePath, publishedTime :: DateTime }
fetchPackageSource { tmpDir, ref, location } = case location of
  Git _ -> do
    -- TODO: Support non-GitHub packages. Remember subdir when doing so. (See #15)
    Log.exit "Packages are only allowed to come from GitHub for now. See #15"

  GitHub { owner, repo, subdir } -> do
    -- TODO: Support subdir. In the meantime, we verify subdir is not present. (See #16)
    when (isJust subdir) $ Log.exit "`subdir` is not supported for now. See #16"

    case pursPublishMethod of
      LegacyPursPublish -> do
        Log.debug $ "Using legacy Git clone to fetch package source at tag: " <> show { owner, repo, ref }
        Run.liftAff (Aff.attempt (Git.cloneGitTag (Array.fold [ "https://github.com/", owner, "/", repo ]) ref tmpDir)) >>= case _ of
          Left error -> do
            Log.error $ "Failed to clone git tag: " <> Aff.message error
            Log.exit $ "Failed to clone repository " <> owner <> "/" <> repo <> " at ref " <> ref
          Right _ -> Log.debug "Cloned package source."
        Log.debug $ "Getting published time..."
        -- Cloning will result in the `repo` name as the directory name
        publishedTime <- Run.liftAff (Except.runExceptT (Git.gitGetRefTime ref (Path.concat [ tmpDir, repo ]))) >>= case _ of
          Left error -> do
            Log.error $ "Failed to get published time: " <> error
            Log.exit $ "Cloned repository " <> owner <> "/" <> repo <> " at ref " <> ref <> ", but could not read the published time from the ref."
          Right value -> pure value
        pure { packageDirectory: Path.concat [ tmpDir, repo ], publishedTime }

      PursPublish -> do
        Log.debug $ "Using GitHub API to fetch package source at tag " <> show { owner, repo, ref }
        commitDate <- do
          let destination = owner <> "/" <> repo
          commit <- GitHub.getRefCommit { owner, repo } (RawVersion ref) >>= case _ of
            Left githubError -> do
              Log.error $ "Failed to fetch " <> destination <> " at ref " <> ref <> ": " <> Octokit.printGitHubError githubError
              Log.exit $ "Failed to fetch commit data associated with " <> destination <> " at ref " <> ref
            Right result -> pure result
          GitHub.getCommitDate { owner, repo } (RawVersion commit) >>= case _ of
            Left githubError -> do
              Log.error $ "Failed to fetch " <> destination <> " at commit " <> commit <> ": " <> Octokit.printGitHubError githubError
              Log.exit $ "Unable to get published time for commit " <> commit <> " associated with the given ref " <> ref
            Right a -> pure a

        let tarballName = ref <> ".tar.gz"
        let absoluteTarballPath = Path.concat [ tmpDir, tarballName ]
        let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
        Log.debug $ "Fetching tarball from GitHub: " <> archiveUrl

        response <- Run.liftAff $ withBackoff' $ Affjax.Node.request $ Affjax.Node.defaultRequest
          { method = Left GET
          , responseFormat = ResponseFormat.arrayBuffer
          , url = archiveUrl
          }

        case response of
          Nothing -> Log.exit $ "Could not download " <> archiveUrl
          Just (Left error) -> do
            Log.error $ "Failed to download " <> archiveUrl <> " because of an HTTP error: " <> Affjax.Node.printError error
            Log.exit $ "Could not download " <> archiveUrl
          Just (Right { status, body }) | status /= StatusCode 200 -> do
            buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
            bodyString <- Run.liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
            Log.error $ "Failed to download " <> archiveUrl <> " because of a non-200 status code (" <> show status <> ") with body " <> bodyString
            Log.exit $ "Could not download " <> archiveUrl
          Just (Right { body }) -> do
            Log.debug $ "Successfully downloaded " <> archiveUrl <> " into a buffer."
            buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
            Run.liftAff (Aff.attempt (FS.Aff.writeFile absoluteTarballPath buffer)) >>= case _ of
              Left error -> do
                Log.error $ "Downloaded " <> archiveUrl <> " but failed to write it to the file at path " <> absoluteTarballPath <> ":\n" <> Aff.message error
                Log.exit $ "Could not download " <> archiveUrl <> " due to an internal error."
              Right _ ->
                Log.debug $ "Tarball downloaded to " <> absoluteTarballPath

        Log.debug "Verifying tarball..."
        Run.liftEffect (Foreign.Tar.getToplevelDir absoluteTarballPath) >>= case _ of
          Nothing ->
            Log.exit "Downloaded tarball from GitHub has no top-level directory."
          Just dir -> do
            Log.debug "Extracting the tarball..."
            Run.liftEffect $ Tar.extract { cwd: tmpDir, archive: tarballName }
            pure { packageDirectory: dir, publishedTime: commitDate }

-- | Copy files from the package source directory to the destination directory
-- | for the tarball. This will copy all always-included files as well as files
-- | provided by the user via the `files` key.
copyPackageSourceFiles
  :: forall r
   . Maybe (NonEmptyArray NonEmptyString)
  -> { source :: FilePath, destination :: FilePath }
  -> Run (LOG_EXCEPT + AFF + EFFECT + r) Unit
copyPackageSourceFiles files { source, destination } = do
  userFiles <- case files of
    Nothing -> pure []
    Just nonEmptyGlobs -> do
      let globs = map NonEmptyString.toString $ NonEmptyArray.toArray nonEmptyGlobs
      { succeeded, failed } <- liftAff $ FastGlob.match source globs

      unless (Array.null failed) do
        Log.exit $ String.joinWith " "
          [ "Some paths matched by globs in the 'files' key are outside your package directory."
          , "Please ensure globs only match within your package directory, including symlinks."
          ]

      pure succeeded

  includedFiles <- Run.liftAff $ FastGlob.match source includedGlobs
  includedInsensitiveFiles <- Run.liftAff $ FastGlob.match' source includedInsensitiveGlobs { caseSensitive: false }

  let
    copyFiles = userFiles <> includedFiles.succeeded <> includedInsensitiveFiles.succeeded
    makePaths path = { from: Path.concat [ source, path ], to: Path.concat [ destination, path ], preserveTimestamps: true }

  Run.liftAff $ traverse_ (makePaths >>> FS.Extra.copy) copyFiles

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
  :: forall r
   . AuthenticatedData
  -> Maybe (NonEmptyArray Owner)
  -> Run (GITHUB + LOG_EXCEPT + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + AFF + r) (Tuple AuthenticatedData (Maybe (NonEmptyArray Owner)))
acceptTrustees authenticated maybeOwners = do
  { username } <- Env.askGitHubEvent
  { publicKey, privateKey } <- Env.askPacchettiBotti

  if authenticated.email /= pacchettiBottiEmail then
    pure (Tuple authenticated maybeOwners)
  else do
    GitHub.listTeamMembers packagingTeam >>= case _ of
      Left githubError -> Log.exit $ Array.fold
        [ "This authenticated operation was opened using the pacchettibotti "
        , "email address, but we were unable to authenticate that you are a "
        , "member of the @purescript/packaging team:\n\n"
        , Octokit.printGitHubError githubError
        ]
      Right members -> do
        unless (Array.elem username members) do
          Log.exit $ Array.fold
            [ "This authenticated operation was opened using the pacchettibotti "
            , "email address, but your username is not a member of the "
            , "@purescript/packaging team."
            ]

        signature <- Run.liftAff (Auth.signPayload { publicKey, privateKey, rawPayload: authenticated.rawPayload }) >>= case _ of
          Left _ -> Log.exit "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature

        let
          newAuthenticated = authenticated { signature = signature }

          pacchettiBottiOwner = Owner
            { email: Env.pacchettibottiEmail
            , keytype: Env.pacchettibottiKeyType
            , public: publicKey
            }

          ownersWithPacchettiBotti = case maybeOwners of
            Nothing -> NonEmptyArray.singleton pacchettiBottiOwner
            Just owners -> NonEmptyArray.cons pacchettiBottiOwner owners

        pure (Tuple newAuthenticated (Just ownersWithPacchettiBotti))

packagingTeam :: Team
packagingTeam = { org: "purescript", team: "packaging" }

-- | An ignored directory suitable for storing results when running the API or
-- | scripts.
scratchDir :: FilePath
scratchDir = "scratch"

cacheDir :: FilePath
cacheDir = ".cache"

mkNewMetadata :: Location -> Metadata
mkNewMetadata location = Metadata
  { location
  , owners: Nothing
  , published: Map.empty
  , unpublished: Map.empty
  }

addVersionToMetadata :: Version -> PublishedMetadata -> Metadata -> Metadata
addVersionToMetadata version versionMeta (Metadata metadata) = do
  let published = Map.insert version versionMeta metadata.published
  Metadata (metadata { published = published })

unpublishVersionInMetadata :: Version -> UnpublishedMetadata -> Metadata -> Metadata
unpublishVersionInMetadata version versionMeta (Metadata metadata) = do
  let published = Map.delete version metadata.published
  let unpublished = Map.insert version versionMeta metadata.unpublished
  Metadata (metadata { published = published, unpublished = unpublished })

isVersionInMetadata :: Version -> Metadata -> Boolean
isVersionInMetadata version (Metadata metadata) = versionPublished || versionUnpublished
  where
  versionPublished = isJust $ Map.lookup version metadata.published
  versionUnpublished = isJust $ Map.lookup version metadata.unpublished
