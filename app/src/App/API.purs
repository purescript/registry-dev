module Registry.App.API where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Ansi.Codes (GraphicsParam)
import Control.Alternative as Alternative
import Control.Monad.Except as Except
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HTTP.Method (Method(..))
import Data.Map as Map
import Data.Newtype (over, unwrap)
import Data.Number.Format as Number.Format
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Dodo (Doc)
import Effect.Aff as Aff
import Effect.Ref as Ref
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
import Registry.App.Effect.Pursuit (PURSUIT)
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Manifest (LEGACY_CACHE)
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), rawPackageNameMapCodec)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (IssueNumber(..), Team)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tar as Foreign.Tar
import Registry.Foreign.Tmp as Tmp
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PackageSetUpdateData, PublishData)
import Registry.Operation as Operation
import Registry.Operation.Validation (UnpublishError(..))
import Registry.Operation.Validation as Operation.Validation
import Registry.Owner as Owner
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except as Run.Except
import Sunde as Sunde

-- | Operations can be exercised for old, pre-registry packages, or for packages
-- | which are on the 0.15 compiler series. If a true legacy package is uploaded
-- | then we do not require compilation to succeed and we don't publish docs.
data Source = Legacy | Current

derive instance Eq Source

printSource :: Source -> String
printSource = case _ of
  Legacy -> "legacy"
  Current -> "current"

type PackageSetUpdateEffects r = (REGISTRY + PACKAGE_SETS + GITHUB + GITHUB_EVENT_ENV + NOTIFY + LOG + LOG_EXCEPT + r)

-- | Process a package set update. Package set updates are only processed via
-- | GitHub and not the HTTP API, so they require access to the GitHub env.
packageSetUpdate :: forall r. PackageSetUpdateData -> Run (PackageSetUpdateEffects + r) Unit
packageSetUpdate payload = do
  { issue, username } <- Env.askGitHubEvent

  Log.debug $ Array.fold
    [ "Package set update created from issue " <> show (un IssueNumber issue) <> " by user " <> username
    , " with payload:\n" <> stringifyJson Operation.packageSetUpdateCodec payload
    ]

  latestPackageSet <- Registry.readLatestPackageSet >>= case _ of
    Nothing -> do
      Log.error "Could not read latest package set, but there must be an initial package set to process an update."
      Log.exit "There is no latest package set."
    Just set -> pure set

  Log.debug $ "Most recent package set was " <> stringifyJson PackageSet.codec latestPackageSet
  let prevCompiler = (un PackageSet latestPackageSet).compiler
  let prevPackages = (un PackageSet latestPackageSet).packages

  Log.debug "Determining whether authentication is required (the compiler changed or packages were removed)..."
  let didChangeCompiler = maybe false (not <<< eq prevCompiler) payload.compiler
  let didRemovePackages = any isNothing payload.packages

  -- Changing the compiler version or removing packages are both restricted
  -- to only the packaging team. We throw here if this is an authenticated
  -- operation and we can't verify they are a member of the packaging team.
  when (didChangeCompiler || didRemovePackages) do
    Log.debug "Authentication is required. Verifying the user can take authenticated actions..."
    GitHub.listTeamMembers packagingTeam >>= case _ of
      Left githubError -> do
        Log.error $ "Failed to retrieve the members of the packaging team from GitHub: " <> Octokit.printGitHubError githubError
        Log.exit $ Array.fold
          [ "This package set update changes the compiler version or removes a "
          , "package from the package set. Only members of the "
          , "@purescript/packaging team can take these actions, but we were "
          , "unable to authenticate your account."
          ]
      Right members -> do
        unless (Array.elem username members) do
          Log.error $ "Username " <> username <> " is not a member of the packaging team, aborting..."
          Log.exit $ Array.fold
            [ "This package set update changes the compiler version or "
            , "removes a package from the package set. Only members of the "
            , "@purescript/packaging team can take these actions, but your "
            , "username is not a member of the packaging team."
            ]
        Log.debug $ "Authentication verified for package set update by user " <> username

  -- The compiler version cannot be downgraded.
  for_ payload.compiler \version -> when (version < prevCompiler) do
    Log.error $ "New compiler version " <> Version.print version <> " is lower than the previous package set compiler " <> Version.print prevCompiler
    Log.exit $ Array.fold
      [ "You are downgrading the compiler used in the package set from "
      , "the current version (" <> Version.print prevCompiler <> ") "
      , "to a lower version (" <> Version.print version <> "). "
      , "The package set compiler version cannot be downgraded."
      ]

  -- Package versions cannot be downgraded, either.
  let
    downgradedPackages = Array.catMaybes do
      Tuple name version <- Map.toUnfoldable payload.packages
      pure do
        newVersion <- version
        prevVersion <- Map.lookup name prevPackages
        -- We want to fail if the existing version is greater than the
        -- new proposed version.
        Alternative.guard (prevVersion > newVersion)
        pure (Tuple name { old: prevVersion, new: newVersion })

  when (not (Array.null downgradedPackages)) do
    Log.exit $ Array.fold
      [ "You are attempting to downgrade one or more package versions from "
      , "their version in the previous set, but this is not allowed in the "
      , " package sets. Affected packages:\n\n"
      , String.joinWith "\n" $ downgradedPackages <#> \(Tuple name { old, new }) ->
          "  - " <> PackageName.print name <> " from " <> Version.print old <> " to " <> Version.print new
      ]

  -- With these conditions met, we can attempt to process the batch with the
  -- new packages and/or compiler version. Note: if the compiler is updated to
  -- a version that isn't supported by the registry then an 'unsupported
  -- compiler' error will be thrown.

  -- TODO: The candidates really ought to be a ChangeSet, and the validation
  -- function should probably be in Run and only fetch the manifests it needs
  -- on-demand instead of requiring the entire index.
  manifestIndex <- Registry.readAllManifests
  PackageSets.validatePackageSet latestPackageSet
  let candidates = PackageSets.validatePackageSetCandidates manifestIndex latestPackageSet payload.packages

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
    PackageSets.upgradeAtomic latestPackageSet (fromMaybe prevCompiler payload.compiler) changeSet >>= case _ of
      Nothing ->
        Log.exit "The package set produced from this suggested update does not compile."
      Just packageSet -> do
        let commitMessage = PackageSets.commitMessage latestPackageSet changeSet (un PackageSet packageSet).version
        Registry.writePackageSet packageSet commitMessage
        Notify.notify "Built and released a new package set! Now mirroring to the package-sets repo..."
        Registry.mirrorPackageSet packageSet
        Notify.notify "Mirrored a new legacy package set."

type AuthenticatedEffects r = (REGISTRY + STORAGE + GITHUB + PACCHETTIBOTTI_ENV + NOTIFY + LOG + LOG_EXCEPT + AFF + EFFECT + r)

-- | Run an authenticated package operation, ie. an unpublish or a transfer.
--
-- TODO: This currently requires a GitHub envent environment, but this should be
-- changed; this operation will be available via the API and we will not be able
-- to tell what "username" was used to produce the operation.
authenticated :: forall r. AuthenticatedData -> Run (AuthenticatedEffects + r) Unit
authenticated auth = case auth.payload of
  Unpublish payload -> do
    Log.debug $ "Processing authorized unpublish operation with payload: " <> stringifyJson Operation.authenticatedCodec auth
    let formatted = formatPackageVersion payload.name payload.version
    metadata <- Registry.readMetadata payload.name >>= case _ of
      Nothing -> do
        Log.error $ "No metadata found for package " <> PackageName.print payload.name
        Log.exit $ "This package cannot be unpublished because it has not been published before (no metadata was found)."
      Just value -> pure value

    now <- Run.liftEffect nowUTC
    published <- case Operation.Validation.validateUnpublish now payload.version metadata of
      Left NotPublished ->
        Log.exit $ "Cannot unpublish " <> formatted <> " because this version has not been published."
      Left AlreadyUnpublished ->
        Log.exit $ "Cannot unpublish " <> formatted <> " because it has already been unpublished."
      Left InternalError -> do
        Log.error $ formatted <> " is listed as both published and unpublished, which should be impossible!"
        Log.exit $ "Cannot unpublish " <> formatted <> " due to an internal error."
      Left (PastTimeLimit { difference, limit }) ->
        Log.exit $ Array.fold
          [ "Cannot unpublish " <> formatted <> " because it was published " <> Number.Format.toString (unwrap difference) <> " hours ago. "
          , "Packages can only be unpublished for " <> Number.Format.toString (unwrap limit) <> " hours after publication."
          ]
      Right published -> do
        Log.debug $ formatted <> " is an unpublishable version, continuing..."
        pure published

    case (un Metadata metadata).owners of
      Nothing -> do
        Log.error $ "Unpublishing is an authenticated operation, but no owners were listed in the metadata: " <> stringifyJson Metadata.codec metadata
        Log.exit $ String.joinWith " "
          [ "Cannot verify package ownership because no owners are listed in the package metadata."
          , "Please publish a package version with your SSH public key in the owners field."
          , "You can then retry unpublishing this version by authenticating with your private key."
          ]
      Just owners -> do
        pacchettiBotti <- getPacchettiBotti
        Run.liftAff (Auth.verifyPayload pacchettiBotti owners auth) >>= case _ of
          Left error -> do
            Log.error $ Array.fold
              [ "Failed to verify signed payload against owners with error:\n\n" <> error
              , "\n\nusing owners\n"
              , String.joinWith "\n" $ map (stringifyJson Owner.codec) $ NEA.toArray owners
              ]
            Log.exit $ "Could not unpublish " <> formatted <> " because we could not authenticate ownership of the package."
          Right _ -> do
            Log.debug $ "Successfully authenticated ownership of " <> formatted <> ", unpublishing..."
            let
              unpublished = { reason: payload.reason, publishedTime: published.publishedTime, unpublishedTime: now }
              updated = metadata # over Metadata \prev -> prev
                { published = Map.delete payload.version prev.published
                , unpublished = Map.insert payload.version unpublished prev.unpublished
                }
            Storage.deleteTarball payload.name payload.version
            Registry.writeMetadata payload.name updated
            Registry.deleteManifest payload.name payload.version
            Notify.notify $ "Unpublished " <> formatted <> "!"

  Transfer payload -> do
    Log.debug $ "Processing authorized transfer operation with payload: " <> stringifyJson Operation.authenticatedCodec auth
    metadata <- Registry.readMetadata payload.name >>= case _ of
      Nothing -> do
        Log.error $ "No metadata found for package " <> PackageName.print payload.name
        Log.exit $ "This package cannot be transferred because it has not been published before (no metadata was found)."
      Just value -> pure value

    case (un Metadata metadata).owners of
      Nothing -> do
        Log.error $ "Transferring is an authenticated operation, but no owners were listed in the metadata: " <> stringifyJson Metadata.codec metadata
        Log.exit $ String.joinWith " "
          [ "Cannot verify package ownership because no owners are listed in the package metadata."
          , "Please publish a package version with your SSH public key in the owners field."
          , "You can then retry transferring this version by authenticating with your private key."
          ]
      Just owners -> do
        pacchettiBotti <- getPacchettiBotti
        Run.liftAff (Auth.verifyPayload pacchettiBotti owners auth) >>= case _ of
          Left error -> do
            Log.error $ Array.fold
              [ "Failed to verify signed payload against owners with error:\n\n" <> error
              , "\n\nusing owners\n"
              , String.joinWith "\n" $ map (stringifyJson Owner.codec) $ NEA.toArray owners
              ]
            Log.exit $ "Could not transfer your package because we could not authenticate your ownership."
          Right _ -> do
            Log.debug $ "Successfully authenticated ownership, transferring..."
            let updated = metadata # over Metadata _ { location = payload.newLocation }
            Registry.writeMetadata payload.name updated
            Notify.notify "Successfully transferred your package!"
            Registry.mirrorLegacyRegistry payload.name payload.newLocation
            Notify.notify "Mirrored location change to the legacy registry."

type PublishEffects r = (PURSUIT + REGISTRY + STORAGE + GITHUB + LEGACY_CACHE + NOTIFY + LOG + LOG_EXCEPT + AFF + EFFECT + r)

-- | Publish a package via the 'publish' operation. If the package has not been
-- | published before then it will be registered and the given version will be
-- | upload. If it has been published before then the existing metadata will be
-- | updated with the new version.
publish :: forall r. Source -> PublishData -> Run (PublishEffects + r) Unit
publish source payload = do
  let printedName = PackageName.print payload.name

  Log.debug $ "Publishing " <> printSource source <> " package " <> printedName <> " with payload:\n" <> stringifyJson Operation.publishCodec payload

  Log.debug $ "Verifying metadata..."
  Metadata existingMetadata <- Registry.readMetadata payload.name >>= case _ of
    Nothing -> case payload.location of
      Nothing -> do
        Log.error $ "No existing metadata for " <> printedName <> " and no location provided in payload, cannot publish."
        Log.exit $ "Cannot register " <> printedName <> " because the payload did not include a 'location' field."
      Just location ->
        pure $ Metadata { location, owners: Nothing, published: Map.empty, unpublished: Map.empty }

    Just metadata -> case payload.location of
      -- The user can add a new version of their package if it comes from
      -- the same location listed in the metadata OR if they do not provide
      -- a location.
      Nothing -> pure metadata
      Just location | (un Metadata metadata).location == location -> pure metadata
      -- Otherwise, if they attempted to re-register the package under a new
      -- location, then they either did not know the package already existed or
      -- they are attempting a transfer. We do not accept transfers over the
      -- unauthenticated API.
      Just location -> do
        Log.error $ Array.fold
          [ "Metadata found for package "
          , printedName
          , " but the location in the payload ("
          , stringifyJson Location.codec location
          , ") is different from the location in the metadata ("
          , stringifyJson Location.codec (un Metadata metadata).location
          , "), which would indicate a package transfer and is therefore disallowed."
          ]

        Log.exit $ Array.fold
          [ "Cannot register " <> printedName <> " because it has already been registered."
          , " If you want to register your package, please choose a different package name."
          , " If you want to transfer your package to a new location, please submit a transfer operation instead."
          , " Transferring a package is an authenticated operation; make sure you have set the 'owners' key in your manifest."
          ]

  -- We fetch the repo into the temporary directory, returning the full path to
  -- the package directory along with its detected publish time.
  Log.debug "Metadata validated. Fetching package source code..."
  tmp <- Run.liftEffect Tmp.mkTmpDir
  { packageDirectory, publishedTime } <- fetchPackageSource { tmpDir: tmp, ref: payload.ref, location: existingMetadata.location }

  Log.debug $ "Package downloaded to " <> packageDirectory <> ", verifying it contains a src directory..."
  Run.liftAff (Operation.Validation.containsPursFile (Path.concat [ packageDirectory, "src" ])) >>= case _ of
    true ->
      Log.debug "Package contains .purs files in its src directory."
    _ ->
      Log.exit $ Array.fold
        [ "This package has no .purs files in the src directory. "
        , "All package sources must be in the `src` directory, with any additional "
        , "sources indicated by the `files` key in your manifest."
        ]

  -- If this is a legacy import, then we need to construct a `Manifest` for it.
  let packagePursJson = Path.concat [ packageDirectory, "purs.json" ]
  hadPursJson <- Run.liftEffect $ FS.Sync.exists packagePursJson
  unless hadPursJson do
    Notify.notify $ "Package source does not have a purs.json file. Creating one from your bower.json and/or spago.dhall files..."
    address <- case existingMetadata.location of
      Git _ -> Log.exit "Legacy packages can only come from GitHub."
      GitHub { subdir: Just subdir } -> Log.exit $ "Legacy packages cannot use the 'subdir' key, but this package specifies a " <> subdir <> " subdir."
      GitHub { owner, repo } -> pure { owner, repo }

    version <- case LenientVersion.parse payload.ref of
      Left _ -> Log.exit $ "The provided ref " <> payload.ref <> " is not a version of the form X.Y.Z or vX.Y.Z, so it cannot be used."
      Right result -> pure $ LenientVersion.version result

    Legacy.Manifest.fetchLegacyManifest payload.name address (RawVersion payload.ref) >>= case _ of
      Left manifestError -> do
        let formatError { error, reason } = reason <> " " <> Legacy.Manifest.printLegacyManifestError error
        Log.exit $ String.joinWith "\n"
          [ "Could not publish your package because there were issues converting its spago.dhall and/or bower.json files into a purs.json manifest:"
          , formatError manifestError
          ]
      Right legacyManifest -> do
        Log.debug $ "Successfully produced a legacy manifest from the package source. Writing it to the package source..."
        let manifest = Legacy.Manifest.toManifest payload.name version existingMetadata.location legacyManifest
        Run.liftAff $ writeJsonFile Manifest.codec packagePursJson manifest

  Log.debug "A valid purs.json is available in the package source."

  Manifest manifest <- Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 packagePursJson)) >>= case _ of
    Left error -> do
      Log.error $ "Could not read purs.json from path " <> packagePursJson <> ": " <> Aff.message error
      Log.exit $ "Could not find a purs.json file in the package source."
    Right string -> Run.liftAff (jsonToDhallManifest string) >>= case _ of
      Left error -> do
        Log.error $ "Manifest does not typecheck: " <> error
        Log.exit $ "Found a valid purs.json file in the package source, but it does not typecheck."
      Right _ -> case parseJson Manifest.codec string of
        Left err -> do
          Log.error $ "Failed to parse manifest: " <> CA.printJsonDecodeError err
          Log.exit $ "Found a purs.json file in the package source, but it could not be decoded."
        Right manifest -> do
          Log.debug $ "Read a valid purs.json manifest from the package source:\n" <> stringifyJson Manifest.codec manifest
          pure manifest

  Notify.notify "Verifying package..."

  -- We trust the manifest for any changes to the 'owners' field, but for all
  -- other fields we trust the registry metadata.
  let metadata = existingMetadata { owners = manifest.owners }
  unless (Operation.Validation.nameMatches (Manifest manifest) payload) do
    Log.exit $ Array.fold
      [ "The manifest file specifies a package name ("
      , PackageName.print manifest.name
      , ") that differs from the package name submitted to the API ("
      , PackageName.print payload.name
      , "). The manifest and API request must match."
      ]

  unless (Operation.Validation.locationMatches (Manifest manifest) (Metadata metadata)) do
    Log.exit $ Array.fold
      [ "The manifest file specifies a location ("
      , stringifyJson Location.codec manifest.location
      , ") that differs from the location in the registry metadata ("
      , stringifyJson Location.codec metadata.location
      , "). If you would like to change the location of your package you should "
      , "submit a transfer operation."
      ]

  when (Operation.Validation.isMetadataPackage (Manifest manifest)) do
    Log.exit "The `metadata` package cannot be uploaded to the registry because it is a protected package."

  for_ (Operation.Validation.isNotPublished (Manifest manifest) (Metadata metadata)) \info -> do
    Log.exit $ String.joinWith "\n"
      [ "You tried to upload a version that already exists: " <> Version.print manifest.version
      , "Its metadata is:"
      , "```json"
      , printJson Metadata.publishedMetadataCodec info
      , "```"
      ]

  for_ (Operation.Validation.isNotUnpublished (Manifest manifest) (Metadata metadata)) \info -> do
    Log.exit $ String.joinWith "\n"
      [ "You tried to upload a version that has been unpublished: " <> Version.print manifest.version
      , ""
      , "```json"
      , printJson Metadata.unpublishedMetadataCodec info
      , "```"
      ]

  Log.debug "Verifying the package build plan..."
  verifiedResolutions <- verifyResolutions (Manifest manifest) payload.resolutions

  Log.debug "Verifying that the package dependencies are all registered..."
  unregisteredRef <- Run.liftEffect $ Ref.new Map.empty
  forWithIndex_ verifiedResolutions \name version -> do
    Registry.readMetadata name >>= case _ of
      Nothing -> Run.liftEffect $ Ref.modify_ (Map.insert name version) unregisteredRef
      Just (Metadata { published }) -> case Map.lookup version published of
        Nothing -> Run.liftEffect $ Ref.modify_ (Map.insert name version) unregisteredRef
        Just _ -> pure unit
  unregistered <- Run.liftEffect $ Ref.read unregisteredRef
  _ <- Log.exit $ Array.fold
    [ "Cannot register this package because it has unregistered dependencies: "
    , Array.foldMap (\(Tuple name version) -> "\n  - " <> formatPackageVersion name version) (Map.toUnfoldable unregistered)
    ]

  Log.info "Packaging tarball for upload..."
  let newDir = PackageName.print manifest.name <> "-" <> Version.print manifest.version
  let packageSourceDir = Path.concat [ tmp, newDir ]
  Run.liftAff $ FS.Extra.ensureDirectory packageSourceDir
  -- We copy over all files that are always included (ie. src dir, purs.json file),
  -- and any files the user asked for via the 'files' key, and remove all files
  -- that should never be included (even if the user asked for them).
  copyPackageSourceFiles manifest.files { source: packageDirectory, destination: packageSourceDir }
  Run.liftAff $ removeIgnoredTarballFiles packageSourceDir
  let tarballPath = packageSourceDir <> ".tar.gz"
  Run.liftEffect $ Tar.create { cwd: tmp, folderName: newDir }

  Log.debug "Tarball created. Verifying its size..."
  FS.Stats.Stats { size: bytes } <- Run.liftAff $ FS.Aff.stat tarballPath
  for_ (Operation.Validation.validateTarballSize bytes) case _ of
    Operation.Validation.ExceedsMaximum maxPackageBytes ->
      Log.exit $ "Package tarball is " <> show bytes <> " bytes, which exceeds the maximum size of " <> show maxPackageBytes <> " bytes."
    Operation.Validation.WarnPackageSize maxWarnBytes ->
      Notify.notify $ "WARNING: Package tarball is " <> show bytes <> "bytes, which exceeds the warning threshold of " <> show maxWarnBytes <> " bytes."

  hash <- Run.liftAff $ Sha256.hashFile tarballPath
  Log.debug $ "Tarball size of " <> show bytes <> " is acceptable. Hash: " <> Sha256.print hash

  -- Now that we have the package source contents we can verify we can compile
  -- the package. We skip failures when the package is a legacy package.
  compilationResult <- compilePackage
    { packageSourceDir: packageDirectory
    , compiler: payload.compiler
    , resolutions: verifiedResolutions
    }

  case compilationResult of
    Left error
      -- We allow legacy packages to fail compilation because we do not
      -- necessarily know what compiler to use with them.
      | source == Legacy -> do
          Log.debug error
          Log.warn "Failed to compile, but continuing because this package is a legacy package."
      | otherwise ->
          Log.exit error
    Right _ ->
      pure unit

  Notify.notify "Package is verified! Uploading it to the storage backend..."
  Storage.uploadTarball manifest.name manifest.version tarballPath
  Log.debug $ "Adding the new version " <> Version.print manifest.version <> " to the package metadata file."
  let newMetadata = metadata { published = Map.insert manifest.version { hash, ref: payload.ref, publishedTime, bytes } metadata.published }
  Registry.writeMetadata manifest.name (Metadata newMetadata)
  Notify.notify "Successfully uploaded package to the registry! 🎉 🚀"

  -- After a package has been uploaded we add it to the registry index, we
  -- upload its documentation to Pursuit, and we can now process it for package
  -- sets when the next batch goes out.

  -- We write to the registry index if possible. If this fails, the packaging
  -- team should manually insert the entry.
  Registry.writeManifest (Manifest manifest)

  when (source == Current) $ case compilationResult of
    Left error -> do
      Log.error $ "Compilation failed, cannot upload to pursuit: " <> error
      Log.exit "Cannot publish to Pursuit because this package failed to compile."
    Right dependenciesDir -> do
      Log.debug "Uploading to Pursuit"
      publishToPursuit { packageSourceDir: packageDirectory, compiler: payload.compiler, resolutions: verifiedResolutions, dependenciesDir } >>= case _ of
        Left message -> Notify.notify message
        Right _ -> Notify.notify "Successfully published docs to pursuit!"

  Registry.mirrorLegacyRegistry payload.name newMetadata.location
  Notify.notify "Mirrored location change to the legacy registry."

-- | Verify the build plan for the package. If the user provided a build plan,
-- | we ensure that the provided versions are within the ranges listed in the
-- | manifest. If not, we solve their manifest to produce a build plan.
verifyResolutions :: forall r. Manifest -> Maybe (Map PackageName Version) -> Run (REGISTRY + LOG + LOG_EXCEPT + r) (Map PackageName Version)
verifyResolutions manifest resolutions = do
  Log.debug "Check the submitted build plan matches the manifest"
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
  -> Run (PURSUIT + LOG + NOTIFY + AFF + EFFECT + r) (Either (Doc GraphicsParam) Unit)
publishToPursuit { packageSourceDir, dependenciesDir, compiler, resolutions } = Run.Except.runExceptAt Log._logExcept do
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

  result <- Pursuit.publish publishJson
  case result of
    Left error ->
      Log.exit $ "Could not publish your package to Pursuit because an error was encountered (cc: @purescript/packaging): " <> error
    Right _ ->
      Notify.notify "Successfully uploaded package docs to Pursuit! 🎉 🚀"

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
          GitHub.getCommitDate { owner, repo } commit >>= case _ of
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

jsonToDhallManifest :: String -> Aff (Either String String)
jsonToDhallManifest jsonStr = do
  let cmd = "json-to-dhall"
  let stdin = Just jsonStr
  let args = [ "--records-loose", "--unions-strict", "." <> Path.sep <> Path.concat [ "types", "v1", "Manifest.dhall" ] ]
  result <- Sunde.spawn { cmd, stdin, args } ChildProcess.defaultSpawnOptions
  pure $ case result.exit of
    ChildProcess.Normally 0 -> Right jsonStr
    _ -> Left result.stderr

getPacchettiBotti :: forall r. Run (PACCHETTIBOTTI_ENV + r) Owner
getPacchettiBotti = do
  { publicKey } <- Env.askPacchettiBotti
  pure $ Owner
    { email: Env.pacchettibottiEmail
    , keytype: Env.pacchettibottiKeyType
    , public: publicKey
    }

packagingTeam :: Team
packagingTeam = { org: "purescript", team: "packaging" }
