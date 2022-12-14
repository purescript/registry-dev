module Registry.App.API where

import Registry.App.Prelude

import Affjax.Node as Http
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Alternative as Alternative
import Control.Monad.Except as Except
import Control.Monad.Reader (ask, asks)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.DateTime (DateTime)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HTTP.Method as Method
import Data.Map as Map
import Data.MediaType.Common as MediaType
import Data.Newtype (unwrap)
import Data.Number.Format as Number.Format
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Data.String.Base64 as Base64
import Data.String.NonEmpty as NonEmptyString
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception (throw)
import Effect.Ref as Ref
import Foreign.FastGlob as FastGlob
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..), IssueNumber, PackageURL(..))
import Foreign.GitHub as GitHub
import Foreign.JsonRepair as JsonRepair
import Foreign.Node.FS as FS.Extra
import Foreign.Object as Object
import Foreign.Purs (CompilerFailure(..))
import Foreign.Purs as Purs
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Foreign.Wget as Wget
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Node.Process as Node.Process
import Registry.App.Auth as Auth
import Registry.App.Cache (Cache)
import Registry.App.Cache as Cache
import Registry.App.Json as Json
import Registry.App.LenientVersion as LenientVersion
import Registry.App.PackageIndex as PackageIndex
import Registry.App.PackageSets as App.PackageSets
import Registry.App.PackageStorage as PackageStorage
import Registry.App.RegistryM (Env, RegistryM, closeIssue, comment, commitMetadataFile, commitPackageSetFile, deletePackage, readPackagesMetadata, runRegistryM, throwWithComment, updatePackagesMetadata, uploadPackage)
import Registry.Constants (GitHubRepo)
import Registry.Constants as Constants
import Registry.Legacy.Manifest as Legacy.Manifest
import Registry.Legacy.PackageSet as Legacy.PackageSet
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PackageOperation(..), PackageSetOperation(..), PublishData)
import Registry.Operation as Operation
import Registry.Operation.Validation as Operation.Validation
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver as Solver
import Registry.Version as Version
import Sunde as Process

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect do
    Node.Process.lookupEnv "GITHUB_EVENT_PATH"
      >>= maybe (throw "GITHUB_EVENT_PATH not defined in the environment") pure

  githubToken <- liftEffect do
    Node.Process.lookupEnv "GITHUB_TOKEN"
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
      FS.Extra.ensureDirectory scratchDir
      cache <- Cache.useCache cacheDir
      packagesMetadata <- liftEffect $ Ref.new Map.empty
      runRegistryM (mkEnv octokit cache packagesMetadata issue username) do
        comment $ case operation of
          Left packageSetOperation -> case packageSetOperation of
            PackageSetUpdate _ ->
              "Processing package set update."
          Right packageOperation -> case packageOperation of
            Publish { name, ref } ->
              "Publishing package `" <> PackageName.print name <> "` at the ref `" <> ref <> "`."
            Authenticated { payload } -> case payload of
              Unpublish { name, version } ->
                "Unpublishing `" <> PackageName.print name <> "` at version `" <> Version.print version <> "`."
              Transfer { name } ->
                "Transferring `" <> PackageName.print name <> "`."

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
  | DecodedOperation IssueNumber String (Either PackageSetOperation PackageOperation)

derive instance Eq OperationDecoding

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.Aff.readTextFile UTF8 eventPath

  GitHub.Event { issueNumber, body, username } <- case Json.jsonParser fileContents >>= GitHub.decodeEvent of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  let
    -- TODO: Right now we parse all operations from GitHub issues, but we should
    -- in the future only parse out package set operations. The others should be
    -- handled via a HTTP API.
    decodeOperation :: Json -> Either CA.JsonDecodeError (Either PackageSetOperation PackageOperation)
    decodeOperation json = do
      object <- CA.decode CA.jobject json
      let keys = Object.keys object
      let hasKeys = all (flip Array.elem keys)
      if hasKeys [ "packages" ] then
        map (Left <<< PackageSetUpdate) (CA.decode Operation.packageSetUpdateCodec json)
      else if hasKeys [ "name", "ref", "compiler" ] then
        map (Right <<< Publish) (CA.decode Operation.publishCodec json)
      else if hasKeys [ "payload", "signature", "email" ] then
        map (Right <<< Authenticated) (CA.decode Operation.authenticatedCodec json)
      else
        Left $ CA.TypeMismatch "Operation: Expected a valid registry operation, but provided object did not match any operation decoder."

  case Argonaut.Parser.jsonParser (JsonRepair.tryRepair (firstObject body)) of
    Left err -> do
      log "Not JSON."
      logShow { err, body }
      pure NotJson
    Right json -> case decodeOperation json of
      Left jsonError -> do
        let printedError = CA.printJsonDecodeError jsonError
        log $ "Malformed JSON:\n" <> printedError
        log $ "Received body:\n" <> body
        pure $ MalformedJson issueNumber printedError
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
runOperation :: Source -> Either PackageSetOperation PackageOperation -> RegistryM Unit
runOperation source operation = case operation of
  Right (Publish fields@{ name, location }) -> do
    packagesMetadata <- readPackagesMetadata
    case Map.lookup name packagesMetadata of
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
          Just _ -> throwWithComment $ String.joinWith " "
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
        Nothing -> throwWithComment $ String.joinWith " "
          [ "Cannot register"
          , PackageName.print name
          , "because no 'location' field was provided."
          ]
        Just packageLocation | not (Operation.Validation.locationIsUnique packageLocation packagesMetadata) -> throwWithComment $ String.joinWith " "
          [ "Cannot register"
          , PackageName.print name
          , "at the location"
          , Json.stringifyJson Location.codec packageLocation
          , "because that location is already in use to publish another package."
          ]
        Just packageLocation ->
          publish source fields (mkNewMetadata packageLocation)

  Left (PackageSetUpdate { compiler, packages }) -> do
    { octokit, cache, username } <- ask

    latestPackageSet <- App.PackageSets.readLatestPackageSet
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

      throwWithComment $ Array.fold
        [ "You are attempting to downgrade one or more package versions from "
        , "their version in the previous set. Affected packages:\n\n"
        , String.joinWith "\n" $ map formatPackage downgradedPackages
        ]

    -- With these conditions met, we can attempt to process the batch with the
    -- new packages and/or compiler version. Note: if the compiler is updated to
    -- a version that isn't supported by the registry then an 'unsupported
    -- compiler' error will be thrown.
    registryIndex <- PackageIndex.readManifestIndexFromDisk
    App.PackageSets.validatePackageSet registryIndex latestPackageSet

    let candidates = App.PackageSets.validatePackageSetCandidates registryIndex latestPackageSet packages

    unless (Map.isEmpty candidates.rejected) do
      throwWithComment $ String.joinWith "\n"
        [ "One or more packages in the suggested batch cannot be processed.\n"
        , App.PackageSets.printRejections candidates.rejected
        ]

    if Map.isEmpty candidates.accepted then do
      throwWithComment "No packages in the suggested batch can be processed; all failed validation checks."
    else do
      workDir <- liftEffect Tmp.mkTmpDir
      App.PackageSets.processBatchAtomic workDir registryIndex latestPackageSet compiler candidates.accepted >>= case _ of
        Just { fail, packageSet, success } | Map.isEmpty fail -> do
          newPath <- App.PackageSets.getPackageSetPath (un PackageSet packageSet).version
          liftAff $ Json.writeJsonFile PackageSet.codec newPath packageSet
          let commitMessage = App.PackageSets.commitMessage latestPackageSet success (un PackageSet packageSet).version
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

  Right (Authenticated submittedAuth@{ payload }) -> case payload of
    Unpublish { name, version, reason } -> do
      username <- asks _.username
      Metadata metadata <- readMetadata name { noMetadata: "No metadata found for your package. Only published packages can be unpublished." }
      now <- liftEffect nowUTC
      publishedMetadata <- case Operation.Validation.validateUnpublish now version (Metadata metadata) of
        Left Operation.Validation.NotPublished ->
          throwWithComment $ "Cannot unpublish " <> Version.print version <> " because it is not a published version."
        Left Operation.Validation.AlreadyUnpublished ->
          throwWithComment $ "Cannot unpublish " <> Version.print version <> " because it has already been unpublished."
        Left Operation.Validation.InternalError ->
          throwWithComment $ String.joinWith "\n"
            [ "Cannot unpublish " <> Version.print version <> "."
            , ""
            , "This version is listed both as published and unpublished. This is an internal error."
            , "cc @purescript/packaging"
            ]
        Left (Operation.Validation.PastTimeLimit { difference, limit }) ->
          throwWithComment $ "Packages can only be unpublished within " <> Number.Format.toString (unwrap limit) <> " hours. Package was published " <> Number.Format.toString (unwrap difference) <> " hours ago."
        Right published ->
          pure published

      Tuple auth maybeOwners <- acceptTrustees username submittedAuth metadata.owners

      case maybeOwners of
        Nothing ->
          throwWithComment $ String.joinWith " "
            [ "Cannot verify package ownership because no owners are listed in the package metadata."
            , "Please publish a package version with your SSH public key in the owners field."
            , "You can then retry unpublishing this version by authenticating with your private key."
            ]
        Just owners -> liftAff (Auth.verifyPayload owners auth) >>= case _ of
          Left err -> throwWithComment $ String.joinWith "\n"
            [ "Failed to verify package ownership:"
            , err
            ]
          Right _ -> do
            deletePackage { name, version }

            let
              unpublishedMetadata =
                { reason
                , publishedTime: publishedMetadata.publishedTime
                , unpublishedTime: now
                }

              updatedMetadata = unpublishVersionInMetadata version unpublishedMetadata (Metadata metadata)

            writeMetadata name updatedMetadata >>= case _ of
              Left err -> throwWithComment $ String.joinWith "\n"
                [ "Unpublish succeeded, but committing metadata failed."
                , err
                , "cc @purescript/packaging"
                ]
              Right _ -> pure unit

            PackageIndex.writeDeleteIndex name version >>= case _ of
              Left err -> throwWithComment $ String.joinWith "\n"
                [ "Unpublish succeeded, but committing to the registry index failed."
                , err
                , "cc: @purescript/packaging"
                ]
              Right _ -> pure unit

            comment "Successfully unpublished!"

      closeIssue

    Transfer { name, newLocation } -> do
      username <- asks _.username
      Metadata metadata <- readMetadata name
        { noMetadata: String.joinWith " "
            [ "No metadata found for your package."
            , "You can only transfer packages that have already been published."
            , "Did you mean to create an Addition?"
            ]
        }

      packagesMetadata <- readPackagesMetadata
      let
        isUniqueLocation = Operation.Validation.locationIsUnique newLocation packagesMetadata
        notUniqueError = String.joinWith " "
          [ "Cannot transfer"
          , PackageName.print name
          , " to "
          , Json.stringifyJson Location.codec newLocation
          , "because another package is already registered at that location."
          ]

      case source of
        Importer | not isUniqueLocation ->
          comment notUniqueError
        API | not isUniqueLocation ->
          throwWithComment notUniqueError
        _ -> do
          Tuple auth maybeOwners <- acceptTrustees username submittedAuth metadata.owners

          case maybeOwners of
            Nothing ->
              throwWithComment $ String.joinWith " "
                [ "Cannot verify package ownership because no owners are listed in the package metadata."
                , "Please publish a package version with your SSH public key in the owners field."
                , "You can then retry transferring this package by authenticating with your private key."
                ]
            Just owners ->
              liftAff (Auth.verifyPayload owners auth) >>= case _ of
                Left err ->
                  throwWithComment $ String.joinWith "\n"
                    [ "Failed to verify package ownership:"
                    , "  " <> err
                    ]
                Right _ -> do
                  let updatedMetadata = metadata { location = newLocation }
                  writeMetadata name (Metadata updatedMetadata) >>= case _ of
                    Left err -> throwWithComment $ String.joinWith "\n"
                      [ "Transferred package location, but failed to commit metadata."
                      , err
                      , "cc: @purescript/packaging"
                      ]
                    Right _ -> do
                      comment "Successfully transferred your package!"
                      syncLegacyRegistry name newLocation

          closeIssue

registryMetadataPath :: FilePath -> FilePath
registryMetadataPath registryPath = Path.concat [ registryPath, Constants.packageMetadataDirectory ]

registryPackageSetsPath :: FilePath -> FilePath
registryPackageSetsPath registryPath = Path.concat [ registryPath, Constants.packageSetsDirectory ]

metadataFile :: FilePath -> PackageName -> FilePath
metadataFile registryPath packageName = Path.concat [ registryPath, Constants.packageMetadataDirectory, PackageName.print packageName <> ".json" ]

jsonToDhallManifest :: String -> Aff (Either String String)
jsonToDhallManifest jsonStr = do
  let cmd = "json-to-dhall"
  let stdin = Just jsonStr
  let args = [ "--records-loose", "--unions-strict", "." <> Path.sep <> Path.concat [ "types", "v1", "Manifest.dhall" ] ]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> Right jsonStr
    _ -> Left result.stderr

publish :: Source -> PublishData -> Metadata -> RegistryM Unit
publish source publishData@{ name, ref, compiler, resolutions } (Metadata inputMetadata) = do
  tmpDir <- liftEffect $ Tmp.mkTmpDir

  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  { packageDirectory, publishedTime } <- fetchPackageSource { tmpDir, ref, location: inputMetadata.location }

  let manifestPath = Path.concat [ packageDirectory, "purs.json" ]

  log $ "Package available in " <> packageDirectory

  log "Verifying that the package contains a `src` directory"
  whenM (liftAff $ Operation.Validation.containsPursFile (Path.concat [ packageDirectory, "src" ])) do
    throwWithComment "This package has no .purs files in the src directory. All package sources must be in the `src` directory, with any additional sources indicated by the `files` key in your manifest."

  -- If this is a legacy import, then we need to construct a `Manifest` for it.
  isLegacyImport <- liftEffect $ map not $ FS.Sync.exists manifestPath
  when isLegacyImport do
    address <- case inputMetadata.location of
      Git _ -> throwWithComment "Legacy packages can only come from GitHub. Aborting."
      GitHub { owner, repo } -> pure { owner, repo }

    version <- case LenientVersion.parse ref of
      Left _ -> throwWithComment $ "Not a valid registry version: " <> ref
      Right result -> pure $ LenientVersion.version result

    legacyPackageSets <- Legacy.Manifest.fetchLegacyPackageSets

    let
      packageSetDeps = do
        versions <- Map.lookup name legacyPackageSets
        deps <- Map.lookup (RawVersion ref) versions
        pure deps

    Except.runExceptT (Legacy.Manifest.fetchLegacyManifest packageSetDeps address (RawVersion ref)) >>= case _ of
      Left manifestError -> do
        let formatError { error, reason } = reason <> " " <> Legacy.Manifest.printLegacyManifestError error
        throwWithComment $ String.joinWith "\n"
          [ "There were problems with the legacy manifest file:"
          , formatError manifestError
          ]
      Right legacyManifest -> do
        let manifest = Legacy.Manifest.toManifest name version inputMetadata.location legacyManifest
        liftAff $ Json.writeJsonFile Manifest.codec manifestPath manifest

  -- Try to read the manifest, typechecking it
  manifest@(Manifest manifestFields) <- liftAff (try $ FS.Aff.readTextFile UTF8 manifestPath) >>= case _ of
    Left _err -> throwWithComment $ "Manifest not found at " <> manifestPath
    Right manifestStr -> liftAff (jsonToDhallManifest manifestStr) >>= case _ of
      Left err -> throwWithComment $ "Could not typecheck manifest: " <> err
      Right _ -> case Json.parseJson Manifest.codec manifestStr of
        Left err -> throwWithComment $ "Could not parse manifest as JSON: " <> err
        Right res -> pure res

  -- We trust the manifest for any changes to the 'owners' field, but for all
  -- other fields we trust the registry metadata.
  let metadata = inputMetadata { owners = manifestFields.owners }
  when (not isLegacyImport && not (Operation.Validation.nameMatches manifest publishData)) do
    throwWithComment $ Array.fold
      [ "The manifest file specifies a package name ("
      , PackageName.print manifestFields.name
      , ") that differs from the package name submitted to the API ("
      , PackageName.print name
      , "). The manifest and API request must match."
      ]

  unless (Operation.Validation.locationMatches manifest (Metadata metadata)) do
    throwWithComment $ Array.fold
      [ "The manifest file specifies a location ("
      , Json.stringifyJson Location.codec manifestFields.location
      , ") that differs from the location in the registry metadata ("
      , Json.stringifyJson Location.codec metadata.location
      , "). If you would like to change the location of your package you should "
      , "submit a Transfer operation."
      ]

  -- Then, we can run verification checks on the manifest and either verify the
  -- provided build plan or produce a new one.
  verifyManifest { metadata: Metadata metadata, manifest }
  verifiedResolutions <- verifyResolutions { resolutions, manifest } >>= case _ of
    Left error ->
      throwWithComment error
    Right result ->
      pure result

  -- After we pass all the checks it's time to do side effects and register the package
  log "Packaging the tarball to upload..."
  -- We need the version number to upload the package
  let newVersion = manifestFields.version
  let newDirname = PackageName.print name <> "-" <> Version.print newVersion
  let packageSourceDir = Path.concat [ tmpDir, newDirname ]
  liftAff $ FS.Extra.ensureDirectory packageSourceDir
  -- We copy over all files that are always included (ie. src dir, purs.json file),
  -- and any files the user asked for via the 'files' key, and remove all files
  -- that should never be included (even if the user asked for them).
  copyPackageSourceFiles manifestFields.files { source: packageDirectory, destination: packageSourceDir }
  liftAff $ removeIgnoredTarballFiles packageSourceDir
  let tarballPath = packageSourceDir <> ".tar.gz"
  liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname }
  log "Checking the tarball size..."
  FS.Stats.Stats { size: bytes } <- liftAff $ FS.Aff.stat tarballPath
  for_ (Operation.Validation.validateTarballSize bytes) case _ of
    Operation.Validation.ExceedsMaximum maxPackageBytes ->
      throwWithComment $ "Package tarball is " <> show bytes <> " bytes, which exceeds the maximum size of " <> show maxPackageBytes <> " bytes.\ncc: @purescript/packaging"
    Operation.Validation.WarnPackageSize maxWarnBytes ->
      comment $ "WARNING: Package tarball is " <> show bytes <> "bytes, which exceeds the warning threshold of " <> show maxWarnBytes <> " bytes.\ncc: @purescript/packaging"
  log "Hashing the tarball..."
  hash <- liftAff $ Sha256.hashFile tarballPath
  log $ "Hash: " <> Sha256.print hash

  -- Now that we have the package source contents we can verify we can compile
  -- the package. We skip failures when the package is a legacy package.
  compilationResult <- compilePackage { packageSourceDir: packageDirectory, compiler, resolutions: verifiedResolutions }

  case compilationResult of
    Left error
      -- We allow bulk-uploaded legacy packages to fail compilation because we
      -- do not necessarily know what compiler to use with them.
      | source == Importer -> do
          log error
          log "Failed to compile, but continuing because the API source was the importer."
      | otherwise ->
          throwWithComment error
    Right _ ->
      pure unit

  log "Uploading package to the storage backend..."
  let uploadPackageInfo = { name, version: newVersion }
  uploadPackage uploadPackageInfo tarballPath
  log $ "Adding the new version " <> Version.print newVersion <> " to the package metadata file (hashes, etc)"
  log $ "Hash for ref " <> ref <> " was " <> Sha256.print hash
  let newMetadata = addVersionToMetadata newVersion { hash, ref, publishedTime, bytes } (Metadata metadata)
  writeMetadata name newMetadata >>= case _ of
    Left err -> throwWithComment $ String.joinWith "\n"
      [ "Package uploaded, but committing metadata failed."
      , err
      , "cc: @purescript/packaging"
      ]
    Right _ ->
      comment "Successfully uploaded package to the registry! 🎉 🚀"

  closeIssue

  -- After a package has been uploaded we add it to the registry index, we
  -- upload its documentation to Pursuit, and we can now process it for package
  -- sets when the next batch goes out.

  -- We write to the registry index if possible. If this fails, the packaging
  -- team should manually insert the entry.
  PackageIndex.writeInsertIndex manifest >>= case _ of
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
      publishToPursuit { packageSourceDir: packageDirectory, compiler, resolutions: verifiedResolutions, dependenciesDir } >>= case _ of
        Left error ->
          comment $ "Pursuit publishing failed: " <> error
        Right message ->
          comment message

  syncLegacyRegistry name (un Metadata newMetadata).location

verifyManifest :: { metadata :: Metadata, manifest :: Manifest } -> RegistryM Unit
verifyManifest { metadata, manifest } = do
  let Manifest manifestFields = manifest

  -- TODO: collect all errors and return them at once. Note: some of the checks
  -- are going to fail while parsing from JSON, so we should move them here if we
  -- want to handle everything together
  log "Running checks for the following manifest:"
  log $ Argonaut.stringifyWithIndent 2 $ CA.encode Manifest.codec manifest

  log "Ensuring the package is not the purescript-metadata package, which cannot be published."
  when (Operation.Validation.isMetadataPackage manifest) do
    throwWithComment "The `metadata` package cannot be uploaded to the registry as it is a protected package."

  log "Check that version has not already been published"
  for_ (Operation.Validation.isNotPublished manifest metadata) \info -> do
    throwWithComment $ String.joinWith "\n"
      [ "You tried to upload a version that already exists: " <> Version.print manifestFields.version
      , "Its metadata is:"
      , "```"
      , Argonaut.stringifyWithIndent 2 $ Codec.encode Metadata.publishedMetadataCodec info
      , "```"
      ]

  log "Check that version has not been unpublished"
  for_ (Operation.Validation.isNotUnpublished manifest metadata) \info -> do
    throwWithComment $ String.joinWith "\n"
      [ "You tried to upload a version that has been unpublished: " <> Version.print manifestFields.version
      , "Details:"
      , "```"
      , Argonaut.stringifyWithIndent 2 $ Codec.encode Metadata.unpublishedMetadataCodec info
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
    throwWithComment $ "Some dependencies of your package were not found in the Registry: " <> String.joinWith ", " (map PackageName.print pkgsNotInRegistry)

-- | Verify the build plan for the package. If the user provided a build plan,
-- | we ensure that the provided versions are within the ranges listed in the
-- | manifest. If not, we solve their manifest to produce a build plan.
verifyResolutions :: { resolutions :: Maybe (Map PackageName Version), manifest :: Manifest } -> RegistryM (Either String (Map PackageName Version))
verifyResolutions { resolutions, manifest } = Except.runExceptT do
  log "Check the submitted build plan matches the manifest"
  -- We don't verify packages provided via the mass import.
  registryIndex <- lift PackageIndex.readManifestIndexFromDisk
  case resolutions of
    Nothing -> case Operation.Validation.validateDependenciesSolve manifest registryIndex of
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
        throwError printedError
      Right solved -> pure solved
    Just provided -> do
      Except.ExceptT $ validateResolutions manifest provided
      pure provided

validateResolutions :: Manifest -> Map PackageName Version -> RegistryM (Either String Unit)
validateResolutions manifest resolutions = Except.runExceptT do
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

    throwError $ String.joinWith "\n\n" $ Array.catMaybes
      [ Just "All dependencies from the manifest must be in the build plan at valid versions."
      , missingPackagesError
      , incorrectVersionsError
      ]

type CompilePackage =
  { packageSourceDir :: FilePath
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

compilePackage :: CompilePackage -> RegistryM (Either String FilePath)
compilePackage { packageSourceDir, compiler, resolutions } = do
  tmp <- liftEffect $ Tmp.mkTmpDir
  let dependenciesDir = Path.concat [ tmp, ".registry" ]
  liftAff $ FS.Extra.ensureDirectory dependenciesDir
  let
    globs =
      if Map.isEmpty resolutions then
        [ "src/**/*.purs" ]
      else
        [ "src/**/*.purs"
        , Path.concat [ dependenciesDir, "*/src/**/*.purs" ]
        ]
  forWithIndex_ resolutions (installPackage dependenciesDir)
  log "Compiling..."
  compilerOutput <- liftAff $ Purs.callCompiler
    { command: Purs.Compile { globs }
    , version: Version.print compiler
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
      filename = PackageName.print packageName <> "-" <> Version.print version <> ".tar.gz"
      filepath = Path.concat [ dir, filename ]

    liftAff (withBackoff' (Wget.wget (Constants.packageStorageUrl <> "/" <> PackageName.print packageName <> "/" <> Version.print version <> ".tar.gz") filepath)) >>= case _ of
      Nothing -> throwWithComment "Could not fetch tarball."
      Just (Left err) -> throwWithComment $ "Error while fetching tarball: " <> err
      Just (Right _) -> pure unit

    liftEffect $ Tar.extract { cwd: dir, archive: filename }
    liftAff $ FS.Aff.unlink filepath
    log $ "Installed " <> PackageName.print packageName <> "@" <> Version.print version

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
publishToPursuit :: PublishToPursuit -> RegistryM (Either String String)
publishToPursuit { packageSourceDir, dependenciesDir, compiler, resolutions } = Except.runExceptT do
  log "Generating a resolutions file"
  tmp <- liftEffect Tmp.mkTmpDir

  let
    resolvedPaths = formatPursuitResolutions { resolutions, dependenciesDir }
    resolutionsFilePath = Path.concat [ tmp, "resolutions.json" ]

  liftAff $ Json.writeJsonFile pursuitResolutionsCodec resolutionsFilePath resolvedPaths

  -- NOTE: The compatibility version of purs publish appends 'purescript-' to the
  -- package name in the manifest file:
  -- https://github.com/purescript/purescript/blob/a846892d178d3c9c76c162ca39b9deb6fad4ec8e/src/Language/PureScript/Publish/Registry/Compat.hs#L19
  --
  -- The resulting documentation will all use purescript- prefixes in keeping
  -- with the format used by Pursuit in PureScript versions at least up to 0.16
  compilerOutput <- liftAff $ Purs.callCompiler
    { command: Purs.Publish { resolutions: resolutionsFilePath }
    , version: Version.print compiler
    , cwd: Just packageSourceDir
    }

  publishJson <- case compilerOutput of
    Left MissingCompiler -> throwError $ Array.fold
      [ "Publishing failed because the build plan compiler version "
      , Version.print compiler
      , " is not supported. Please try again with a different compiler."
      ]
    Left (CompilationError errs) -> throwError $ String.joinWith "\n"
      [ "Publishing failed because the build plan does not compile with version " <> Version.print compiler <> " of the compiler:"
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

type PursuitResolutions = Map RawPackageName { version :: Version, path :: FilePath }

pursuitResolutionsCodec :: JsonCodec PursuitResolutions
pursuitResolutionsCodec = rawPackageNameMapCodec $ Json.object "Resolution" { version: Version.codec, path: CA.string }

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
  , uploadPackage: PackageStorage.upload
  , deletePackage: PackageStorage.delete
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
    packageList <- try (FS.Aff.readdir metadataDir) >>= case _ of
      Right list -> pure $ Array.mapMaybe (String.stripSuffix $ String.Pattern ".json") list
      Left err -> do
        error $ show err
        pure []
    packagesArray <- for packageList \rawPackageName -> do
      packageName <- case PackageName.parse rawPackageName of
        Right p -> pure p
        Left err -> do
          log $ "Encountered error while parsing package name! It was: " <> rawPackageName
          Aff.throwError $ Aff.error err
      let metadataPath = metadataFile registryDir packageName
      metadata <- Json.readJsonFile Metadata.codec metadataPath >>= case _ of
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

-- | Fetch the latest from the given repository. Will perform a fresh clone if
-- | a checkout of the repository does not exist at the given path, and will
-- | pull otherwise.
fetchRepo :: GitHubRepo -> FilePath -> Aff Unit
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
  liftAff $ fetchRepo Constants.packageIndex registryIndexPath

fetchRegistry :: RegistryM Unit
fetchRegistry = do
  registryPath <- asks _.registry
  log "Fetching the most recent registry ..."
  liftAff $ fetchRepo Constants.registry registryPath

data PursPublishMethod = LegacyPursPublish | PursPublish

-- | A temporary flag that records whether we are using legacy purs publish
-- | (which requires all packages to be a Git repository) or new purs publish
-- | (which accepts any directory with package sources).
pursPublishMethod :: PursPublishMethod
pursPublishMethod = LegacyPursPublish

fetchPackageSource
  :: { tmpDir :: FilePath, ref :: String, location :: Location }
  -> RegistryM { packageDirectory :: FilePath, publishedTime :: DateTime }
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
        Git.cloneGitTag (Array.fold [ "https://github.com/", owner, "/", repo ]) ref tmpDir
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
  Git.runGit_ [ "add", ManifestIndex.packageEntryFilePath packageName ] (Just registryIndexDir)
  Git.runGit_ [ "commit", "-m", "Update manifests for package " <> PackageName.print packageName ] (Just registryIndexDir)
  let upstreamRepo = Constants.packageIndex.owner <> "/" <> Constants.packageIndex.repo
  let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
  void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryIndexDir)

pacchettiBottiPushToRegistryMetadata :: PackageName -> FilePath -> Aff (Either String Unit)
pacchettiBottiPushToRegistryMetadata packageName registryDir = Except.runExceptT do
  GitHubToken token <- Git.configurePacchettiBotti (Just registryDir)
  Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryDir)
  Git.runGit_ [ "add", Path.concat [ "metadata", PackageName.print packageName <> ".json" ] ] (Just registryDir)
  Git.runGit_ [ "commit", "-m", "Update metadata for package " <> PackageName.print packageName ] (Just registryDir)
  let upstreamRepo = Constants.registry.owner <> "/" <> Constants.registry.repo
  let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
  void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryDir)

pacchettiBottiPushToRegistryPackageSets :: Version -> String -> FilePath -> Aff (Either String Unit)
pacchettiBottiPushToRegistryPackageSets version commitMessage registryDir = Except.runExceptT do
  GitHubToken token <- Git.configurePacchettiBotti (Just registryDir)
  Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryDir)
  Git.runGit_ [ "add", Path.concat [ Constants.packageSetsDirectory, Version.print version <> ".json" ] ] (Just registryDir)
  Git.runGit_ [ "commit", "-m", commitMessage ] (Just registryDir)
  let upstreamRepo = Constants.registry.owner <> "/" <> Constants.registry.repo
  let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
  void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryDir)

-- | Copy files from the package source directory to the destination directory
-- | for the tarball. This will copy all always-included files as well as files
-- | provided by the user via the `files` key.
copyPackageSourceFiles :: Maybe (NonEmptyArray NonEmptyString) -> { source :: FilePath, destination :: FilePath } -> RegistryM Unit
copyPackageSourceFiles files { source, destination } = do
  userFiles <- case files of
    Nothing -> pure []
    Just nonEmptyGlobs -> do
      let globs = map NonEmptyString.toString $ NonEmptyArray.toArray nonEmptyGlobs
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
  liftAff $ Json.writeJsonFile Metadata.codec (metadataFile registryDir packageName) metadata
  updatePackagesMetadata packageName metadata
  commitMetadataFile packageName

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
acceptTrustees username authenticated maybeOwners = do
  { octokit, cache } <- ask
  if authenticated.email /= Git.pacchettiBottiEmail then
    pure (Tuple authenticated maybeOwners)
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

        signature <- liftAff (Auth.signPayload { publicKey, privateKey, rawPayload: authenticated.rawPayload }) >>= case _ of
          Left _ -> throwWithComment "Error signing transfer. cc: @purescript/packaging"
          Right signature -> pure signature

        let
          newAuthenticated = authenticated { signature = signature }

          pacchettiBottiOwner = Owner
            { email: Git.pacchettiBottiEmail
            , keytype: pacchettiBottiKeyType
            , public: publicKey
            }

          ownersWithPacchettiBotti = case maybeOwners of
            Nothing -> NonEmptyArray.singleton pacchettiBottiOwner
            Just owners -> NonEmptyArray.cons pacchettiBottiOwner owners

        pure (Tuple newAuthenticated (Just ownersWithPacchettiBotti))

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
scratchDir :: FilePath
scratchDir = "scratch"

cacheDir :: FilePath
cacheDir = ".cache"

envFilePath :: FilePath
envFilePath = ".env"

-- | Loads the `.env` file into the environment.
loadEnv :: Aff Dotenv.Settings
loadEnv = do
  contents <- Aff.try $ FS.Aff.readTextFile UTF8 envFilePath
  case contents of
    Left _ -> log ("Not loading .env file because none was found at path: " <> envFilePath) $> []
    Right string -> Dotenv.loadContents (String.trim string)

data LegacyRegistryFile = BowerPackages | NewPackages

derive instance Eq LegacyRegistryFile

legacyRegistryFilePath :: LegacyRegistryFile -> FilePath
legacyRegistryFilePath = case _ of
  BowerPackages -> "bower-packages.json"
  NewPackages -> "new-packages.json"

legacyRegistryCodec :: JsonCodec (Map String PackageURL)
legacyRegistryCodec = CA.Common.strMap (Profunctor.wrapIso PackageURL CA.string)

-- | A helper function that syncs API operations to the new-packages.json or
-- | bower-packages.json files, namely registrations and transfers.
syncLegacyRegistry :: PackageName -> Location -> RegistryM Unit
syncLegacyRegistry package location = do
  registryDir <- asks _.registry

  packageUrl <- case location of
    GitHub { owner, repo } -> pure $ GitHub.PackageURL $ Array.fold [ "https://github.com/", owner, "/", repo, ".git" ]
    _ -> throwWithComment "Could not sync package with legacy registry: packages must come from GitHub. (cc: @purescript/packaging)"

  let
    readLegacyFile file = do
      let path = Path.concat [ registryDir, legacyRegistryFilePath file ]
      liftAff (Json.readJsonFile legacyRegistryCodec path) >>= case _ of
        Left err -> throwWithComment $ "Could not sync package with legacy registry (could not read " <> path <> "(cc: @purescript/packaging): " <> err
        Right packages -> pure packages

  newPackages <- readLegacyFile NewPackages
  bowerPackages <- readLegacyFile BowerPackages

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
    let sourceFile = legacyRegistryFilePath target
    let packages = Map.insert rawPackageName packageUrl sourcePackages
    result <- liftAff $ Except.runExceptT do
      liftAff $ Json.writeJsonFile legacyRegistryCodec (Path.concat [ registryDir, sourceFile ]) packages
      GitHubToken token <- Git.configurePacchettiBotti (Just registryDir)
      Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryDir)
      Git.runGit_ [ "add", sourceFile ] (Just registryDir)
      let message = Array.fold [ "Mirror registry API operation to ", sourceFile ]
      Git.runGit_ [ "commit", "-m", message ] (Just registryDir)
      let upstreamRepo = Constants.registry.owner <> "/" <> Constants.registry.repo
      let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
      void $ Git.runGit_ [ "push", origin, "main" ] (Just registryDir)
    case result of
      Left err -> throwWithComment err
      Right _ -> comment "Synced new package with legacy registry files."

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
