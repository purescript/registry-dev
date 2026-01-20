module Registry.App.API
  ( AuthenticatedEffects
  , COMPILER_CACHE
  , CompilerCache(..)
  , LicenseValidationError(..)
  , PURS_GRAPH_CACHE
  , PackageSetUpdateEffects
  , PublishEffects
  , PursGraphCache(..)
  , _compilerCache
  , _pursGraphCache
  , authenticated
  , copyPackageSourceFiles
  , findAllCompilers
  , formatPursuitResolutions
  , getPacchettiBotti

  , packageSetUpdate
  , packagingTeam
  , printLicenseValidationError
  , publish
  , removeIgnoredTarballFiles
  , validateLicense
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Exists as Exists
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Newtype (over)
import Data.Number.Format as Number.Format
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.String.NonEmpty as NonEmptyString
import Data.String.Regex as Regex
import Effect.Aff as Aff
import Effect.Unsafe (unsafePerformEffect)
import JSON as JSON
import Node.ChildProcess.Types (Exit(..))
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
import Node.FS.Sync as FS.Sync
import Node.Library.Execa as Execa
import Node.Path as Path
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Registry.API.V1 (PackageSetJobData)
import Registry.App.Auth as Auth
import Registry.App.CLI.Licensee as Licensee
import Registry.App.CLI.Purs (CompilerFailure(..), compilerFailureCodec)
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.Archive (ARCHIVE)
import Registry.App.Effect.Archive as Archive
import Registry.App.Effect.Cache (class FsEncodable, Cache)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (PACCHETTIBOTTI_ENV, RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (Change(..), PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Pursuit (PURSUIT)
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as ManifestIndex
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Source (SOURCE)
import Registry.App.Effect.Source as Source
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Manifest (LEGACY_CACHE)
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), rawPackageNameMapCodec)
import Registry.App.Manifest.SpagoYaml as SpagoYaml
import Registry.App.Server.MatrixBuilder as MatrixBuilder
import Registry.Constants (ignoredDirectories, ignoredFiles, ignoredGlobs, includedGlobs, includedInsensitiveGlobs)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (Team)
import Registry.Foreign.Tmp as Tmp
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Path as Internal.Path
import Registry.License as License
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PublishData)
import Registry.Operation as Operation
import Registry.Operation.Validation (UnpublishError(..), ValidateDepsError(..), validateNoExcludedObligatoryFiles)
import Registry.Operation.Validation as Operation.Validation
import Registry.Owner as Owner
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.PursGraph (ModuleName(..))
import Registry.PursGraph as PursGraph
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver (CompilerIndex, DependencyIndex, Intersection, SolverErrors)
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Safe.Coerce as Safe.Coerce

-- | Effect row for package set updates. Authentication is done at the API
-- | boundary, so we don't need GITHUB or GITHUB_EVENT_ENV effects here.
type PackageSetUpdateEffects r = (REGISTRY + PACKAGE_SETS + LOG + EXCEPT String + r)

-- | Process a package set update from a queued job. Authentication has already
-- | been verified at the API boundary, so we don't need to check team membership.
packageSetUpdate :: forall r. PackageSetJobData -> Run (PackageSetUpdateEffects + r) Unit
packageSetUpdate details = do
  let Operation.PackageSetUpdate payload = details.payload

  Log.debug $ "Package set update job starting with payload:\n" <> stringifyJson Operation.packageSetUpdateCodec payload

  latestPackageSet <- Registry.readLatestPackageSet >>= case _ of
    Nothing -> do
      Log.error "Could not read latest package set, but there must be an initial package set to process an update."
      Except.throw "There is no latest package set."
    Just set -> pure set

  Log.debug $ "Most recent package set was " <> stringifyJson PackageSet.codec latestPackageSet
  let prevCompiler = (un PackageSet latestPackageSet).compiler
  let prevPackages = (un PackageSet latestPackageSet).packages

  -- Note: authentication for restricted operations (compiler change, package removal)
  -- is handled at the API boundary in the Router, not here.

  -- The compiler version cannot be downgraded.
  for_ payload.compiler \version -> when (version < prevCompiler) do
    Log.error $ "New compiler version " <> Version.print version <> " is lower than the previous package set compiler " <> Version.print prevCompiler
    Except.throw $ Array.fold
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
        guard (prevVersion > newVersion)
        pure (Tuple name { old: prevVersion, new: newVersion })

  when (not (Array.null downgradedPackages)) do
    Except.throw $ Array.fold
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
    Except.throw $ String.joinWith "\n"
      [ "One or more packages in the suggested batch cannot be processed.\n"
      , PackageSets.printRejections candidates.rejected
      ]

  when (Map.isEmpty candidates.accepted) $ case payload.compiler of
    Just compiler | compiler > prevCompiler ->
      Log.debug $ "No packages in the suggested batch can be processed, but the compiler version " <> Version.print compiler <> " was upgraded."
    _ ->
      Except.throw "No packages in the suggested batch can be processed (all failed validation checks) and the compiler version was not upgraded, so there is no upgrade to perform."

  let changeSet = candidates.accepted <#> maybe Remove Update
  Log.notice "Attempting to build package set update."
  PackageSets.upgradeSequential latestPackageSet (fromMaybe prevCompiler payload.compiler) changeSet >>= case _ of
    Nothing ->
      Except.throw "No packages could be added to the package set. All packages failed to compile."
    Just { failed, succeeded, result: packageSet } -> do
      unless (Map.isEmpty failed) do
        let
          formatFailed = String.joinWith "\n" $ Array.catMaybes $ flip map (Map.toUnfoldable failed) \(Tuple name change) ->
            case change of
              PackageSets.Update version -> Just $ "  - " <> formatPackageVersion name version
              PackageSets.Remove -> Nothing
        Log.warn $ "Some packages could not be added to the set:\n" <> formatFailed
      let commitMessage = PackageSets.commitMessage latestPackageSet succeeded (un PackageSet packageSet).version
      Registry.writePackageSet packageSet commitMessage
      Log.notice "Built and released a new package set! Now mirroring to the package-sets repo..."
      Registry.mirrorPackageSet packageSet
      Log.notice "Mirrored a new legacy package set."

type AuthenticatedEffects r = (REGISTRY + STORAGE + GITHUB + PACCHETTIBOTTI_ENV + LOG + EXCEPT String + AFF + EFFECT + r)

-- | Run an authenticated package operation, ie. an unpublish or a transfer.
authenticated :: forall r. AuthenticatedData -> Run (AuthenticatedEffects + r) Unit
authenticated auth = case auth.payload of
  Unpublish payload -> do
    Log.debug $ "Processing authorized unpublish operation with payload: " <> stringifyJson Operation.authenticatedCodec auth
    let formatted = formatPackageVersion payload.name payload.version
    metadata <- Registry.readMetadata payload.name >>= case _ of
      Nothing -> do
        Log.error $ "No metadata found for package " <> PackageName.print payload.name
        Except.throw $ "This package cannot be unpublished because it has not been published before (no metadata was found)."
      Just value -> pure value

    now <- nowUTC
    published <- case Operation.Validation.validateUnpublish now payload.version metadata of
      Left NotPublished ->
        Except.throw $ "Cannot unpublish " <> formatted <> " because this version has not been published."
      Left AlreadyUnpublished ->
        Except.throw $ "Cannot unpublish " <> formatted <> " because it has already been unpublished."
      Left InternalError -> do
        Log.error $ formatted <> " is listed as both published and unpublished, which should be impossible!"
        Except.throw $ "Cannot unpublish " <> formatted <> " due to an internal error."
      Left (PastTimeLimit { difference, limit }) ->
        Except.throw $ Array.fold
          [ "Cannot unpublish " <> formatted <> " because it was published " <> Number.Format.toString (unwrap difference) <> " hours ago. "
          , "Packages can only be unpublished for " <> Number.Format.toString (unwrap limit) <> " hours after publication."
          ]
      Right published -> do
        Log.debug $ formatted <> " is an unpublishable version, continuing..."
        pure published

    pacchettiBotti <- getPacchettiBotti
    let owners = maybe [] NonEmptyArray.toArray (un Metadata metadata).owners
    Run.liftAff (Auth.verifyPayload pacchettiBotti owners auth) >>= case _ of
      Left _ | [] <- owners -> do
        Log.error $ "Unpublishing is an authenticated operation, but no owners were listed in the metadata: " <> stringifyJson Metadata.codec metadata
        Except.throw $ String.joinWith " "
          [ "Cannot verify package ownership because no owners are listed in the package metadata."
          , "Please publish a package version with your SSH public key in the owners field."
          , "You can then retry unpublishing this version by authenticating with your private key."
          ]
      Left error -> do
        Log.error $ Array.fold
          [ "Failed to verify signed payload against owners with error:\n\n" <> error
          , "\n\nusing owners\n"
          , String.joinWith "\n" $ stringifyJson Owner.codec <$> owners
          ]
        Except.throw $ "Could not unpublish " <> formatted <> " because we could not authenticate ownership of the package."
      Right _ -> do
        Log.debug $ "Successfully authenticated ownership of " <> formatted <> ", unpublishing..."
        let
          unpublished = { reason: payload.reason, publishedTime: published.publishedTime, unpublishedTime: now }
          updated = metadata # over Metadata \prev -> prev
            { published = Map.delete payload.version prev.published
            , unpublished = Map.insert payload.version unpublished prev.unpublished
            }
        -- Delete the manifest entry first so ManifestIndex.delete can fail if other
        -- packages still depend on this version. This way, we detect dependency
        -- violations before performing any irreversible side effects like deleting
        -- the tarball from storage.
        Registry.deleteManifest payload.name payload.version
        Storage.delete payload.name payload.version
        Registry.writeMetadata payload.name updated
        Log.notice $ "Unpublished " <> formatted <> "!"

  Transfer payload -> do
    Log.debug $ "Processing authorized transfer operation with payload: " <> stringifyJson Operation.authenticatedCodec auth
    metadata <- Registry.readMetadata payload.name >>= case _ of
      Nothing -> do
        Log.error $ "No metadata found for package " <> PackageName.print payload.name
        Except.throw $ "This package cannot be transferred because it has not been published before (no metadata was found)."
      Just value -> pure value

    pacchettiBotti <- getPacchettiBotti
    let owners = maybe [] NonEmptyArray.toArray (un Metadata metadata).owners
    Run.liftAff (Auth.verifyPayload pacchettiBotti owners auth) >>= case _ of
      Left _ | [] <- owners -> do
        Log.error $ "Transferring is an authenticated operation, but no owners were listed in the metadata: " <> stringifyJson Metadata.codec metadata
        Except.throw $ String.joinWith " "
          [ "Cannot verify package ownership because no owners are listed in the package metadata."
          , "Please publish a package version with your SSH public key in the owners field."
          , "You can then retry transferring this version by authenticating with your private key."
          ]
      Left error -> do
        Log.error $ Array.fold
          [ "Failed to verify signed payload against owners with error:\n\n" <> error
          , "\n\nusing owners\n"
          , String.joinWith "\n" $ stringifyJson Owner.codec <$> owners
          ]
        Except.throw $ "Could not transfer your package because we could not authenticate your ownership."
      Right _ -> do
        Log.debug $ "Successfully authenticated ownership of " <> PackageName.print payload.name <> ", transferring..."
        let updated = metadata # over Metadata _ { location = payload.newLocation }
        Registry.writeMetadata payload.name updated
        Log.notice "Successfully transferred your package!"
        Registry.mirrorLegacyRegistry payload.name payload.newLocation
        Log.notice "Mirrored registry operation to the legacy registry."

type PublishEffects r = (RESOURCE_ENV + PURSUIT + REGISTRY + STORAGE + SOURCE + ARCHIVE + GITHUB + COMPILER_CACHE + PURS_GRAPH_CACHE + LEGACY_CACHE + LOG + EXCEPT String + AFF + EFFECT + r)

-- | Resolve both compiler and resolutions for a publish operation.
-- | Will come up with some sort of plan if not provided with a compiler and/or resolutions.
resolveCompilerAndDeps
  :: forall r
   . CompilerIndex
  -> Manifest
  -> Maybe Version -- payload.compiler
  -> Maybe (Map PackageName Version) -- payload.resolutions
  -> Run (REGISTRY + LOG + AFF + EXCEPT String + r) { compiler :: Version, resolutions :: Map PackageName Version }
resolveCompilerAndDeps compilerIndex manifest@(Manifest { dependencies }) maybeCompiler maybeResolutions = do
  Log.debug "Resolving compiler and dependencies..."
  case maybeCompiler of
    -- if we have a compiler we can worry about the rest of the build plan
    Just compiler -> do
      Log.debug $ "Using provided compiler " <> Version.print compiler
      resolutions <- case maybeResolutions of
        -- resolutions are provided so we just check them over
        Just provided -> do
          validateResolutions manifest provided
          pure provided
        -- no resolutions, invoke the solver with the compiler + dependencies
        Nothing ->
          case Operation.Validation.validateDependenciesSolve compiler manifest compilerIndex of
            Left errors -> Except.throw $ formatSolverErrors errors
            Right resolutions -> pure resolutions
      pure { compiler, resolutions }

    -- no compiler provided, we can figure it out. We only need one for publishing anyways
    Nothing -> do
      Log.debug "No compiler provided, solving for compiler and resolutions"
      -- If resolutions are provided, validate them against the manifest first
      for_ maybeResolutions \provided -> validateResolutions manifest provided
      let deps = maybe dependencies (map Range.exact) maybeResolutions
      Tuple compiler resolutions <- do
        allCompilers <- PursVersions.pursVersions
        let
          -- we pass in all compilers so the solver can pick one
          allCompilersRange = Range.mk
            (NonEmptyArray.head allCompilers)
            (Version.bumpPatch (NonEmptyArray.last allCompilers))
        case allCompilersRange of
          Nothing -> Except.throw "Could not construct compiler range"
          Just range ->
            case Solver.solveWithCompiler range compilerIndex deps of
              Left errors -> Except.throw $ formatSolverErrors errors
              Right result -> pure result

      Log.info $ "Discovered compiler " <> Version.print compiler
      pure { compiler, resolutions }
  where
  formatSolverErrors errors = String.joinWith "\n"
    [ "Could not produce valid dependencies for manifest."
    , "```"
    , errors # foldMapWithIndex \index error -> String.joinWith "\n"
        [ "[Error " <> show (index + 1) <> "]"
        , Solver.printSolverError error
        ]
    , "```"
    ]

-- | Publish a package via the 'publish' operation. If the package has not been
-- | published before then it will be registered and the given version will be
-- | upload. If it has been published before then the existing metadata will be
-- | updated with the new version.
--
-- The legacyIndex argument contains the unverified manifests produced by the
-- legacy importer; these manifests can be used on legacy packages to conform
-- them to the registry rule that transitive dependencies are not allowed.
publish :: forall r. Maybe Solver.TransitivizedRegistry -> PublishData -> Run (PublishEffects + r) (Maybe { compiler :: Version, dependencies :: Map PackageName Range, version :: Version })
publish maybeLegacyIndex payload = do
  let printedName = PackageName.print payload.name

  Log.debug $ "Publishing package " <> printedName <> " with payload:\n" <> stringifyJson Operation.publishCodec payload

  Log.debug $ "Verifying metadata..."
  Metadata existingMetadata <- Registry.readMetadata payload.name >>= case _ of
    Nothing -> case payload.location of
      Nothing -> do
        Log.error $ "No existing metadata for " <> printedName <> " and no location provided in payload, cannot publish."
        Except.throw $ "Cannot register " <> printedName <> " because the payload did not include a 'location' field."
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

        Except.throw $ Array.fold
          [ "Cannot register " <> printedName <> " because it has already been registered."
          , " If you want to register your package, please choose a different package name."
          , " If you want to transfer your package to a new location, please submit a transfer operation instead."
          , " Transferring a package is an authenticated operation; make sure you have set the 'owners' key in your manifest."
          ]

  -- We fetch the repo into the temporary directory, returning the full path to
  -- the package directory along with its detected publish time.
  Log.debug "Metadata validated. Fetching package source code..."
  tmp <- Tmp.mkTmpDir

  -- Legacy imports may encounter packages whose GitHub repositories no longer
  -- exist but whose tarballs are stored in the registry-archive. When Source.fetch
  -- fails with InaccessibleRepo during a legacy import, we fall back to fetching
  -- from the registry-archive instead.
  { path: downloadedPackage, published: publishedTime } <-
    Source.fetchEither tmp existingMetadata.location payload.ref >>= case _ of
      Right result ->
        pure result
      Left (Source.InaccessibleRepo address) | isJust maybeLegacyIndex -> do
        Log.warn $ Array.fold
          [ "GitHub repository inaccessible during legacy import: "
          , address.owner
          , "/"
          , address.repo
          ]
        Log.info "Falling back to registry-archive tarball..."
        version <- case LenientVersion.parse payload.ref of
          Left _ -> Except.throw $ Array.fold
            [ "Cannot fall back to archive: ref "
            , payload.ref
            , " is not a valid version"
            ]
          Right v -> pure $ LenientVersion.version v
        Archive.fetch tmp payload.name version
      Left err ->
        Except.throw $ Source.printFetchError err

  Log.debug $ "Package downloaded to " <> downloadedPackage <> ", verifying it contains a src directory..."
  srcPursFiles <- Internal.Path.readPursFiles (Path.concat [ downloadedPackage, "src" ]) >>= case _ of
    Nothing ->
      Except.throw $ Array.fold
        [ "This package has no PureScript files in its `src` directory. "
        , "All package sources must be in the `src` directory, with any additional "
        , "sources indicated by the `files` key in your manifest."
        ]
    Just files -> pure files

  -- If the package doesn't have a purs.json we can try to make one - possible scenarios:
  --  - in case it has a spago.yaml then we know how to read that, and have all the info to move forward
  --  - if it's a legacy import then we can try to infer as much info as possible to make a manifest
  let packagePursJson = Path.concat [ downloadedPackage, "purs.json" ]
  hadPursJson <- Run.liftEffect $ FS.Sync.exists packagePursJson

  let packageSpagoYaml = Path.concat [ downloadedPackage, "spago.yaml" ]
  hasSpagoYaml <- Run.liftEffect $ FS.Sync.exists packageSpagoYaml

  address <- case existingMetadata.location of
    Git _ -> Except.throw "Packages can only come from GitHub for now."
    GitHub { subdir: Just subdir } -> Except.throw $ "Packages cannot yet use the 'subdir' key, but this package specifies a " <> subdir <> " subdir."
    GitHub { owner, repo } -> pure { owner, repo }

  Manifest receivedManifest <-
    if hadPursJson then
      Run.liftAff (Aff.attempt (FS.Aff.readTextFile UTF8 packagePursJson)) >>= case _ of
        Left error -> do
          Except.throw $ "Could not read purs.json from path " <> packagePursJson <> ": " <> Aff.message error
        Right string -> Env.askResourceEnv >>= \{ dhallTypes } -> Run.liftAff (jsonToDhallManifest dhallTypes string) >>= case _ of
          Left error -> do
            Log.error $ "Manifest does not typecheck: " <> error
            Except.throw $ "Found a valid purs.json file in the package source, but it does not typecheck."
          Right _ -> case parseJson Manifest.codec string of
            Left err -> do
              Log.error $ "Failed to parse manifest: " <> CJ.DecodeError.print err
              Except.throw $ "Found a purs.json file in the package source, but it could not be decoded."
            Right manifest -> do
              Log.debug $ "Read a valid purs.json manifest from the package source:\n" <> stringifyJson Manifest.codec manifest
              pure manifest

    else if hasSpagoYaml then do
      Log.notice $ "Package source does not have a purs.json file, creating one from your spago.yaml file..."
      SpagoYaml.readSpagoYaml packageSpagoYaml >>= case _ of
        Left readErr -> Except.throw $ "Could not publish your package - a spago.yaml was present, but it was not possible to read it:\n" <> readErr
        Right config -> case SpagoYaml.spagoYamlToManifest payload.ref config of
          Left err -> Except.throw $ "Could not publish your package - there was an error while converting your spago.yaml into a purs.json manifest:\n" <> err
          Right manifest -> do
            Log.notice $ Array.fold
              [ "Converted your spago.yaml into a purs.json manifest to use for publishing:"
              , "\n```json\n"
              , printJson Manifest.codec manifest
              , "\n```\n"
              ]
            pure manifest

    else do
      Log.notice $ "Package source does not have a purs.json file. Creating one from your bower.json and/or spago.dhall files..."

      version <- case LenientVersion.parse payload.ref of
        Left _ -> Except.throw $ "The provided ref " <> payload.ref <> " is not a version of the form X.Y.Z or vX.Y.Z, so it cannot be used."
        Right result -> pure $ LenientVersion.version result

      Legacy.Manifest.fetchLegacyManifest payload.name address (RawVersion payload.ref) >>= case _ of
        Left manifestError -> do
          let formatError { error, reason } = reason <> " " <> Legacy.Manifest.printLegacyManifestError error
          Except.throw $ String.joinWith "\n"
            [ "Could not publish your package because there were issues converting its spago.dhall and/or bower.json files into a purs.json manifest:"
            , formatError manifestError
            ]
        Right legacyManifest -> do
          Log.debug $ "Successfully produced a legacy manifest from the package source."
          let manifest = Legacy.Manifest.toManifest payload.name version existingMetadata.location payload.ref legacyManifest
          Log.notice $ Array.fold
            [ "Converted your legacy manifest(s) into a purs.json manifest to use for publishing:"
            , "\n```json\n"
            , printJson Manifest.codec manifest
            , "\n```\n"
            ]
          pure manifest

  -- We trust the manifest for any changes to the 'owners' field, but for all
  -- other fields we trust the registry metadata.
  let metadata = existingMetadata { owners = receivedManifest.owners }
  unless (Operation.Validation.nameMatches (Manifest receivedManifest) payload) do
    Except.throw $ Array.fold
      [ "The manifest file specifies a package name ("
      , PackageName.print receivedManifest.name
      , ") that differs from the package name submitted to the API ("
      , PackageName.print payload.name
      , "). The manifest and API request must match."
      ]

  unless (Operation.Validation.locationMatches (Manifest receivedManifest) (Metadata metadata)) do
    if isJust maybeLegacyIndex then
      -- The legacy importer is sometimes run on older packages, some of which have been transferred. Since
      -- package metadata only records the latest location, this can cause a problem: the manifest reports
      -- the location at the time, but the metadata reports the current location.
      Log.warn $ Array.fold
        [ "In legacy mode and manifest location differs from existing metadata. This indicates a package that was "
        , "transferred from a previous location. Ignoring location match validation..."
        ]
    else
      Except.throw $ Array.fold
        [ "The manifest file specifies a location ("
        , stringifyJson Location.codec receivedManifest.location
        , ") that differs from the location in the registry metadata ("
        , stringifyJson Location.codec metadata.location
        , "). If you would like to change the location of your package you should "
        , "submit a transfer operation."
        ]

  when (Operation.Validation.isMetadataPackage (Manifest receivedManifest)) do
    Except.throw "The `metadata` package cannot be uploaded to the registry because it is a protected package."

  Log.info "Verifying licenses are consistent among manifest files..."
  validateLicense downloadedPackage receivedManifest.license >>= case _ of
    Nothing -> Log.debug "License validation passed."
    Just err -> Except.throw $ printLicenseValidationError err

  for_ (Operation.Validation.isNotUnpublished (Manifest receivedManifest) (Metadata metadata)) \info -> do
    Except.throw $ String.joinWith "\n"
      [ "You tried to upload a version that has been unpublished: " <> Version.print receivedManifest.version
      , ""
      , "```json"
      , printJson Metadata.unpublishedMetadataCodec info
      , "```"
      ]

  -- try to terminate early here: if the package is already published AND the docs
  -- are on Pursuit, then we can wrap up here
  for_ (Operation.Validation.isNotPublished (Manifest receivedManifest) (Metadata metadata)) \info -> do
    published <- Pursuit.getPublishedVersions receivedManifest.name >>= case _ of
      Left error -> Except.throw error
      Right versions -> pure versions
    for_ (Map.lookup receivedManifest.version published) \url ->
      Except.throw $ String.joinWith "\n"
        [ "You tried to upload a version that already exists: " <> Version.print receivedManifest.version
        , ""
        , "Its metadata is:"
        , "```json"
        , printJson Metadata.publishedMetadataCodec info
        , "```"
        , ""
        , "and its documentation is available here:"
        , url
        ]

  -- Resolve compiler and resolutions. If compiler was not provided,
  -- discover a compatible compiler based on dependencies.
  Log.info "Verifying the package build plan..."
  compilerIndex <- MatrixBuilder.readCompilerIndex
  { compiler, resolutions: validatedResolutions } <- resolveCompilerAndDeps compilerIndex (Manifest receivedManifest) payload.compiler payload.resolutions
  Log.info $ "Using compiler " <> Version.print compiler

  -- Validate PureScript module names now that we know the compiler.
  -- language-cst-parser only supports syntax back to 0.15.0, so we skip for older compilers.
  Operation.Validation.validatePursModules srcPursFiles >>= case _ of
    Left formattedError | compiler < Purs.minLanguageCSTParser -> do
      Log.debug $ "Package failed to parse in validatePursModules: " <> formattedError
      Log.debug $ "Skipping check because package is published with a pre-0.15.0 compiler (" <> Version.print compiler <> ")."
    Left formattedError ->
      Except.throw $ Array.fold
        [ "This package has either malformed or disallowed PureScript module names "
        , "in its source: "
        , formattedError
        ]
    Right _ ->
      Log.debug "Package contains well-formed .purs files in its src directory."

  case Operation.Validation.isNotPublished (Manifest receivedManifest) (Metadata metadata) of
    -- If the package has been published already but docs for this version are missing
    -- from Pursuit (we check earlier if the docs are there, so we end up here if they are not)
    -- then upload to Pursuit and terminate
    Just _ | compiler < Purs.minPursuitPublish -> do
      Log.notice $ Array.fold
        [ "This version has already been published to the registry, but the docs have not been "
        , "uploaded to Pursuit. Unfortunately, it is not possible to publish to Pursuit via the "
        , "registry using compiler versions prior to " <> Version.print Purs.minPursuitPublish
        , ". Please try with a later compiler."
        ]
      pure Nothing

    Just _ -> do
      Log.notice $ Array.fold
        [ "This version has already been published to the registry, but the docs have not been "
        , "uploaded to Pursuit. Skipping registry publishing and retrying Pursuit publishing..."
        ]
      let installedResolutions = Path.concat [ tmp, ".registry" ]
      buildPlan <- MatrixBuilder.resolutionsToBuildPlan validatedResolutions
      MatrixBuilder.installBuildPlan buildPlan installedResolutions
      compilationResult <- Run.liftAff $ Purs.callCompiler
        { command: Purs.Compile { globs: [ "src/**/*.purs", Path.concat [ installedResolutions, "*/src/**/*.purs" ] ] }
        , version: Just compiler
        , cwd: Just downloadedPackage
        }
      case compilationResult of
        Left compileFailure -> do
          let error = MatrixBuilder.printCompilerFailure compiler compileFailure
          Log.error $ "Compilation failed, cannot upload to pursuit: " <> error
          Except.throw "Cannot publish to Pursuit because this package failed to compile."
        Right _ -> do
          Log.debug "Uploading to Pursuit"
          -- While we have created a manifest from the package source, we
          -- still need to ensure a purs.json file exists for 'purs publish'.
          unless hadPursJson do
            existingManifest <- ManifestIndex.readManifest receivedManifest.name receivedManifest.version
            case existingManifest of
              Nothing -> Except.throw "Version was previously published, but we could not find a purs.json file in the package source, and no existing manifest was found in the registry."
              Just existing -> Run.liftAff $ writeJsonFile Manifest.codec packagePursJson existing
          publishToPursuit { source: downloadedPackage, compiler, resolutions: validatedResolutions, installedResolutions } >>= case _ of
            Left publishErr -> Except.throw publishErr
            Right _ -> do
              FS.Extra.remove tmp
              Log.notice "Successfully uploaded package docs to Pursuit! 🎉 🚀"
          pure Nothing

    -- In this case the package version has not been published, so we proceed
    -- with ordinary publishing.
    Nothing -> do
      Log.notice "Verifying unused and/or missing dependencies..."

      -- First we install the resolutions and call 'purs graph' to adjust the
      -- manifest as needed, but we defer compilation until after this check
      -- in case the package manifest and resolutions are adjusted.
      let installedResolutions = Path.concat [ tmp, ".registry" ]
      buildPlan <- MatrixBuilder.resolutionsToBuildPlan validatedResolutions
      MatrixBuilder.installBuildPlan buildPlan installedResolutions

      let srcGlobs = Path.concat [ downloadedPackage, "src", "**", "*.purs" ]
      let depGlobs = Path.concat [ installedResolutions, "*", "src", "**", "*.purs" ]
      let pursGraphCmd = Purs.Graph { globs: [ srcGlobs, depGlobs ] }

      -- We need to use the minimum compiler version that supports 'purs graph'.
      let pursGraphCompiler = if compiler >= Purs.minPursGraph then compiler else Purs.minPursGraph

      -- Path normalization for caching: we replace temporary directory paths
      -- with placeholders so that cache hits work across runs.
      let
        depsPlaceholder = "$DEPS"
        srcPlaceholder = "$SRC"
        normalizePaths =
          String.replaceAll (String.Pattern installedResolutions) (String.Replacement depsPlaceholder)
            >>> String.replaceAll (String.Pattern downloadedPackage) (String.Replacement srcPlaceholder)
        denormalizePaths =
          String.replaceAll (String.Pattern depsPlaceholder) (String.Replacement installedResolutions)
            >>> String.replaceAll (String.Pattern srcPlaceholder) (String.Replacement downloadedPackage)

      -- In this step we run 'purs graph' to get a graph of the package source
      -- and installed dependencies and use that to determine if the manifest
      -- contains any unused or missing dependencies. If it does and is a legacy
      -- manifest then we fix it and return the result. If does and is a modern
      -- manifest (spago.yaml, purs.json, etc.) then we reject it. If it doesn't
      -- then we simply return the manifest and resolutions we already had.
      pursGraphResult <- Cache.get _pursGraphCache (PursGraph (Manifest receivedManifest) validatedResolutions pursGraphCompiler) >>= case _ of
        Just cached -> do
          Log.debug "Using cached purs graph result"
          pure $ map denormalizePaths cached
        Nothing -> do
          Log.debug "No cached purs graph, running purs graph..."
          result <- Run.liftAff (Purs.callCompiler { command: pursGraphCmd, version: Just pursGraphCompiler, cwd: Nothing })
          let normalizedResult = map normalizePaths result
          Cache.put _pursGraphCache (PursGraph (Manifest receivedManifest) validatedResolutions pursGraphCompiler) normalizedResult
          pure result

      Tuple manifest resolutions <- case pursGraphResult of
        Left err -> case err of
          UnknownError str -> Except.throw str
          MissingCompiler -> Except.throw $ "Missing compiler " <> Version.print pursGraphCompiler
          CompilationError errs -> do
            Log.warn $ Array.fold
              [ "Failed to discover unused dependencies because purs graph failed:\n"
              , Purs.printCompilerErrors errs
              ]
            -- The purs graph command will fail if the source code uses syntax
            -- before the oldest usable purs graph compiler (ie. 0.14.0). In
            -- this case we simply accept the dependencies as-is, even though
            -- they could technically violate Registry rules around missing and
            -- unused dependencies. This only affects old packages and we know
            -- they compile, so we've decided it's an acceptable exception.
            pure $ Tuple (Manifest receivedManifest) validatedResolutions
        Right output -> case JSON.parse output of
          Left parseErr -> Except.throw $ "Failed to parse purs graph output as JSON while finding unused dependencies: " <> parseErr
          Right json -> case CJ.decode PursGraph.pursGraphCodec json of
            Left decodeErr -> Except.throw $ "Failed to decode JSON from purs graph output while finding unused dependencies: " <> CJ.DecodeError.print decodeErr
            Right graph -> do
              Log.debug "Got a valid graph of source and dependencies."
              let
                pathParser path = map _.name $ case String.stripPrefix (String.Pattern installedResolutions) path of
                  Just trimmed -> parseModulePath trimmed
                  Nothing -> case String.stripPrefix (String.Pattern downloadedPackage) path of
                    Just _ -> Right { name: receivedManifest.name, version: receivedManifest.version }
                    Nothing -> Left $ "Failed to parse module path " <> path <> " because it is not in the package source or installed dependencies."

              case Operation.Validation.noTransitiveOrMissingDeps (Manifest receivedManifest) graph pathParser of
                -- Association failures should always throw
                Left (Left assocErrors) ->
                  Except.throw $ Array.fold
                    [ "Failed to validate unused / missing dependencies because modules could not be associated with package names:"
                    , flip NonEmptyArray.foldMap1 assocErrors \{ error, module: ModuleName moduleName, path } ->
                        "\n  - " <> moduleName <> " (" <> path <> "): " <> error
                    ]

                -- FIXME: For now we attempt to fix packages if a legacy index
                -- is provided (ie. the publish is via the importer) but we
                -- should at some point make this a hard error.
                Left (Right depError) -> case maybeLegacyIndex of
                  Nothing ->
                    Except.throw $ "Failed to validate unused / missing dependencies: " <> Operation.Validation.printValidateDepsError depError
                  Just legacyIndex -> do
                    Log.info $ "Found fixable dependency errors: " <> Operation.Validation.printValidateDepsError depError
                    conformLegacyManifest (Manifest receivedManifest) compiler compilerIndex legacyIndex depError

                -- If the check passes then we can simply return the manifest and
                -- resolutions.
                Right _ -> pure $ Tuple (Manifest receivedManifest) validatedResolutions

      -- Now that we've verified the package we can write the manifest to the
      -- source directory.
      Run.liftAff $ writeJsonFile Manifest.codec packagePursJson manifest

      Log.info "Creating packaging directory"
      let packageDirname = PackageName.print receivedManifest.name <> "-" <> Version.print receivedManifest.version
      let packageSource = Path.concat [ tmp, packageDirname ]
      FS.Extra.ensureDirectory packageSource
      -- We copy over all files that are always included (ie. src dir, purs.json file),
      -- and any files the user asked for via the 'files' key, and remove all files
      -- that should never be included (even if the user asked for them).
      copyPackageSourceFiles { includeFiles: receivedManifest.includeFiles, excludeFiles: receivedManifest.excludeFiles, source: downloadedPackage, destination: packageSource }
      removeIgnoredTarballFiles packageSource

      -- Now that we have the package source contents we can verify we can compile
      -- the package with exactly what is going to be uploaded.
      Log.notice $ Array.fold
        [ "Verifying package compiles using compiler "
        , Version.print compiler
        , " and resolutions:\n"
        , "```json\n"
        , printJson (Internal.Codec.packageMap Version.codec) resolutions
        , "\n```"
        ]

      -- Only reinstall dependencies if the resolutions changed after purs
      -- graph / manifest adjustment. This avoids redundant downloads.
      when (resolutions /= validatedResolutions) do
        Log.debug "Resolutions changed after manifest adjustment, reinstalling dependencies..."
        Run.liftAff $ FS.Extra.remove installedResolutions
        buildPlanForBuild <- MatrixBuilder.resolutionsToBuildPlan resolutions
        MatrixBuilder.installBuildPlan buildPlanForBuild installedResolutions

      -- Check compiler cache before compiling - if we have a cached successful
      -- compilation with the same manifest/resolutions/compiler, skip compilation.
      Cache.get _compilerCache (Compilation manifest resolutions compiler) >>= case _ of
        Just { result: Right _ } -> do
          Log.debug "Using cached successful compilation, skipping compilation step"
        _ -> do
          compilationResult <- Run.liftAff $ Purs.callCompiler
            { command: Purs.Compile { globs: [ Path.concat [ packageSource, "src/**/*.purs" ], Path.concat [ installedResolutions, "*/src/**/*.purs" ] ] }
            , version: Just compiler
            , cwd: Just tmp
            }

          case compilationResult of
            Left compileFailure -> do
              let error = MatrixBuilder.printCompilerFailure compiler compileFailure
              Except.throw $ "Publishing failed due to a compiler error:\n\n" <> error
            Right _ -> do
              -- Cache the successful compilation so findAllCompilers can reuse it
              Cache.put _compilerCache (Compilation manifest resolutions compiler) { target: compiler, result: Right unit }
              pure unit

      Log.notice "Package source is verified! Packaging tarball and uploading to the storage backend..."
      let tarballName = packageDirname <> ".tar.gz"
      let tarballPath = Path.concat [ tmp, tarballName ]
      Tar.create { cwd: tmp, folderName: packageDirname }

      Log.info "Tarball created. Verifying its size..."
      bytes <- Run.liftAff $ map FS.Stats.size $ FS.Aff.stat tarballPath
      for_ (Operation.Validation.validateTarballSize bytes) case _ of
        Operation.Validation.ExceedsMaximum maxPackageBytes ->
          Except.throw $ "Package tarball is " <> show bytes <> " bytes, which exceeds the maximum size of " <> show maxPackageBytes <> " bytes."
        Operation.Validation.WarnPackageSize maxWarnBytes ->
          Log.notice $ "WARNING: Package tarball is " <> show bytes <> "bytes, which exceeds the warning threshold of " <> show maxWarnBytes <> " bytes."

      -- If a package has under ~30 bytes it's about guaranteed that packaging the
      -- tarball failed. This can happen if the system running the API has a non-
      -- GNU tar installed, for example.
      let minBytes = 30.0
      when (bytes < minBytes) do
        Except.throw $ "Package tarball is only " <> Number.Format.toString bytes <> " bytes, which indicates the source was not correctly packaged."

      hash <- Sha256.hashFile tarballPath
      Log.info $ "Tarball size of " <> show bytes <> " bytes is acceptable."
      Log.info $ "Tarball hash: " <> Sha256.print hash

      Storage.upload (un Manifest manifest).name (un Manifest manifest).version tarballPath
      Log.debug $ "Adding the new version " <> Version.print (un Manifest manifest).version <> " to the package metadata file."
      let newPublishedVersion = { hash, compilers: NonEmptyArray.singleton compiler, publishedTime, bytes }
      let newMetadata = metadata { published = Map.insert (un Manifest manifest).version newPublishedVersion metadata.published }

      Registry.writeMetadata (un Manifest manifest).name (Metadata newMetadata)
      Log.notice "Successfully uploaded package to the registry! 🎉 🚀"

      -- We write to the registry index if possible. If this fails, the packaging
      -- team should manually insert the entry.
      Log.debug "Adding the new version to the registry index"
      Registry.writeManifest manifest

      Registry.mirrorLegacyRegistry payload.name newMetadata.location
      Log.notice "Mirrored registry operation to the legacy registry!"

      -- Note: We don't publish to Pursuit when running the legacy importer with an
      -- existing index, as that's the reupload path.
      unless (isJust maybeLegacyIndex) do
        Log.debug "Uploading package documentation to Pursuit"
        if compiler >= Purs.minPursuitPublish then
          -- TODO: We must use the 'downloadedPackage' instead of 'packageSource'
          -- because Pursuit requires a git repository, and our tarball directory
          -- is not one. This should be changed once Pursuit no longer needs git.
          publishToPursuit { source: downloadedPackage, compiler, resolutions, installedResolutions } >>= case _ of
            Left publishErr -> do
              Log.error publishErr
              Log.notice $ "Failed to publish package docs to Pursuit: " <> publishErr
            Right _ ->
              Log.notice "Successfully uploaded package docs to Pursuit! 🎉 🚀"
        else
          Log.notice $ Array.fold
            [ "Skipping Pursuit publishing because this package was published with a pre-0.14.7 compiler ("
            , Version.print compiler
            , "). If you want to publish documentation, please try again with a later compiler."
            ]

      -- Note: this only runs for the Legacy Importer. In daily circumstances (i.e.
      -- when running the server) this will be taken care of by followup jobs invoking
      -- the MatrixBuilder for each compiler version
      when (isJust maybeLegacyIndex) do
        Log.notice "Determining all valid compiler versions for this package..."
        allCompilers <- PursVersions.pursVersions
        { failed: invalidCompilers, succeeded: validCompilers } <- case NonEmptyArray.fromFoldable $ NonEmptyArray.delete compiler allCompilers of
          Nothing -> pure { failed: Map.empty, succeeded: NonEmptySet.singleton compiler }
          Just try -> do
            found <- findAllCompilers
              { source: packageSource
              , manifest
              , compilers: try
              }
            pure { failed: found.failed, succeeded: NonEmptySet.cons compiler found.succeeded }

        unless (Map.isEmpty invalidCompilers) do
          Log.debug $ "Some compilers failed: " <> String.joinWith ", " (map Version.print (Set.toUnfoldable (Map.keys invalidCompilers)))

        Log.notice $ "Found compatible compilers: " <> String.joinWith ", " (map (\v -> "`" <> Version.print v <> "`") (NonEmptySet.toUnfoldable validCompilers))
        let metadataWithCompilers = newMetadata { published = Map.update (Just <<< (_ { compilers = NonEmptySet.toUnfoldable1 validCompilers })) (un Manifest manifest).version newMetadata.published }

        Registry.writeMetadata (un Manifest manifest).name (Metadata metadataWithCompilers)
        Log.debug $ "Wrote new metadata " <> printJson Metadata.codec (Metadata metadataWithCompilers)

        Log.notice "Wrote completed metadata to the registry!"

      FS.Extra.remove tmp
      pure $ Just { compiler, dependencies: (un Manifest manifest).dependencies, version: (un Manifest manifest).version }

validateResolutions :: forall r. Manifest -> Map PackageName Version -> Run (EXCEPT String + r) Unit
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
        guard (not Array.null missingPackages)
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
        guard (not Array.null incorrectVersions)
        pure
          $ String.joinWith "\n  - "
          $ Array.cons "The build plan provides dependencies at versions outside the range listed in the manifest:"
          $ map printPackageVersion incorrectVersions

    Except.throw $ String.joinWith "\n\n" $ Array.catMaybes
      [ Just "All dependencies from the manifest must be in the build plan at valid versions."
      , missingPackagesError
      , incorrectVersionsError
      ]

type FindAllCompilersResult =
  { failed :: Map Version (Either SolverErrors CompilerFailure)
  , succeeded :: Set Version
  }

-- | Find all compilers that can compile the package source code and installed
-- | resolutions from the given array of compilers.
findAllCompilers
  :: forall r
   . { source :: FilePath, manifest :: Manifest, compilers :: NonEmptyArray Version }
  -> Run (REGISTRY + STORAGE + COMPILER_CACHE + LOG + AFF + EFFECT + EXCEPT String + r) FindAllCompilersResult
findAllCompilers { source, manifest, compilers } = do
  compilerIndex <- MatrixBuilder.readCompilerIndex
  checkedCompilers <- for compilers \target -> do
    Log.debug $ "Trying compiler " <> Version.print target
    case Solver.solveWithCompiler (Range.exact target) compilerIndex (un Manifest manifest).dependencies of
      Left solverErrors -> do
        Log.info $ "Failed to solve with compiler " <> Version.print target
        pure $ Left $ Tuple target (Left solverErrors)
      Right (Tuple compiler resolutions) -> do
        Log.debug $ "Solved with compiler " <> Version.print target <> " and got resolutions:\n" <> printJson (Internal.Codec.packageMap Version.codec) resolutions
        when (compiler /= target) do
          Except.throw $ Array.fold
            [ "Produced a compiler-derived build plan that selects a compiler ("
            , Version.print compiler
            , ") that differs from the target compiler ("
            , Version.print target
            , ")."
            ]
        Cache.get _compilerCache (Compilation manifest resolutions target) >>= case _ of
          Nothing -> do
            Log.debug $ "No cached compilation, compiling with compiler " <> Version.print target
            workdir <- Tmp.mkTmpDir
            let installed = Path.concat [ workdir, ".registry" ]
            FS.Extra.ensureDirectory installed
            buildPlanForCompiler <- MatrixBuilder.resolutionsToBuildPlan resolutions
            MatrixBuilder.installBuildPlan buildPlanForCompiler installed
            result <- Run.liftAff $ Purs.callCompiler
              { command: Purs.Compile { globs: [ Path.concat [ source, "src/**/*.purs" ], Path.concat [ installed, "*/src/**/*.purs" ] ] }
              , version: Just target
              , cwd: Just workdir
              }
            FS.Extra.remove workdir
            case result of
              Left err -> do
                Log.info $ "Compilation failed with compiler " <> Version.print target <> ":\n" <> MatrixBuilder.printCompilerFailure target err
              Right _ -> do
                Log.debug $ "Compilation succeeded with compiler " <> Version.print target
            Cache.put _compilerCache (Compilation manifest resolutions target) { target, result: map (const unit) result }

            pure $ bimap (Tuple target <<< Right) (const target) result
          Just { result } ->
            pure $ bimap (Tuple target <<< Right) (const target) result

  let results = partitionEithers $ NonEmptyArray.toArray checkedCompilers
  pure { failed: Map.fromFoldable results.fail, succeeded: Set.fromFoldable results.success }

-- | Parse the name and version from a path to a module installed in the standard
-- | form: '<package-name>-<x.y.z>...'
parseModulePath :: FilePath -> Either String { name :: PackageName, version :: Version }
parseModulePath path = do
  packageVersion <- lmap Parsing.parseErrorMessage $ Parsing.runParser path do
    _ <- Parsing.Combinators.optional (Parsing.Combinators.try (Parsing.String.string Path.sep))
    Tuple packageVersionChars _ <- Parsing.Combinators.Array.manyTill_ Parsing.String.anyChar (Parsing.String.string Path.sep)
    pure $ String.CodeUnits.fromCharArray (Array.fromFoldable packageVersionChars)

  -- Then we can drop everything after the last hyphen (the
  -- version number) and join the rest back together.
  case NonEmptyArray.fromArray $ String.split (String.Pattern "-") packageVersion of
    Nothing -> Left $ "Could not parse package name and version from install path: " <> path
    Just separated -> do
      let packageString = String.joinWith "-" (NonEmptyArray.dropEnd 1 separated)
      name <- PackageName.parse packageString
      let versionString = NonEmptyArray.last separated
      version <- Version.parse versionString
      pure { name, version }

type PublishToPursuit =
  { source :: FilePath
  , compiler :: Version
  , resolutions :: Map PackageName Version
  , installedResolutions :: FilePath
  }

-- | Publishes a package to Pursuit.
-- |
-- | ASSUMPTIONS: This function should not be run on legacy packages or on
-- | packages where the `purescript-` prefix is still present. Cannot be used
-- | on packages prior to 'Purs.minPursuitPublish'
publishToPursuit
  :: forall r
   . PublishToPursuit
  -> Run (PURSUIT + LOG + AFF + EFFECT + r) (Either String Unit)
publishToPursuit { source, compiler, resolutions, installedResolutions } = Except.runExcept do
  Log.debug "Generating a resolutions file"
  tmp <- Tmp.mkTmpDir

  when (compiler < Purs.minPursuitPublish) do
    Except.throw $ "Cannot publish to Pursuit because this package was published with a pre-0.14.7 compiler (" <> Version.print compiler <> "). If you want to publish documentation, please try again with a later compiler."

  let
    resolvedPaths = formatPursuitResolutions { resolutions, installedResolutions }
    resolutionsFilePath = Path.concat [ tmp, "resolutions.json" ]

  Run.liftAff $ writeJsonFile pursuitResolutionsCodec resolutionsFilePath resolvedPaths

  -- The 'purs publish' command requires a clean working tree, but it isn't
  -- guaranteed that packages have an adequate .gitignore file. So we stash
  -- stash changes made during the publishing process before calling publish.
  -- https://git-scm.com/docs/gitignore
  Log.debug "Adding output and purs.json to local git excludes..."
  Run.liftAff $ FS.Aff.appendTextFile UTF8 (Path.concat [ source, ".git", "info", "exclude" ]) (String.joinWith "\n" [ "output", "purs.json" ])

  -- NOTE: The compatibility version of purs publish appends 'purescript-' to the
  -- package name in the manifest file:
  -- https://github.com/purescript/purescript/blob/a846892d178d3c9c76c162ca39b9deb6fad4ec8e/src/Language/PureScript/Publish/Registry/Compat.hs#L19
  --
  -- The resulting documentation will all use purescript- prefixes in keeping
  -- with the format used by Pursuit in PureScript versions at least up to 0.16
  compilerOutput <- Run.liftAff $ Purs.callCompiler
    { command: Purs.Publish { resolutions: resolutionsFilePath }
    , version: Just compiler
    , cwd: Just source
    }

  publishJson <- case compilerOutput of
    Left error ->
      Except.throw $ MatrixBuilder.printCompilerFailure compiler error
    Right publishResult -> do
      -- The output contains plenty of diagnostic lines, ie. "Compiling ..."
      -- but we only want the final JSON payload.
      let lines = String.split (String.Pattern "\n") publishResult
      case Array.last lines of
        Nothing -> Except.throw "Publishing failed because of an unexpected compiler error. cc @purescript/packaging"
        Just jsonString -> case JSON.parse jsonString of
          Left err -> Except.throw $ String.joinWith "\n"
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
      Except.throw $ "Could not publish your package to Pursuit because an error was encountered (cc: @purescript/packaging): " <> error
    Right _ ->
      FS.Extra.remove tmp

type PursuitResolutions = Map RawPackageName { version :: Version, path :: FilePath }

pursuitResolutionsCodec :: CJ.Codec PursuitResolutions
pursuitResolutionsCodec = rawPackageNameMapCodec $ CJ.named "Resolution" $ CJ.Record.object { version: Version.codec, path: CJ.string }

-- Resolutions format: https://github.com/purescript/purescript/pull/3565
--
-- Note: This interfaces with Pursuit, and therefore we must add purescript-
-- prefixes to all package names for compatibility with the Bower naming format.
formatPursuitResolutions :: { resolutions :: Map PackageName Version, installedResolutions :: FilePath } -> PursuitResolutions
formatPursuitResolutions { resolutions, installedResolutions } =
  Map.fromFoldable do
    Tuple name version <- Map.toUnfoldable resolutions
    let
      bowerPackageName = RawPackageName ("purescript-" <> PackageName.print name)
      packagePath = Path.concat [ installedResolutions, PackageName.print name <> "-" <> Version.print version ]
    [ Tuple bowerPackageName { path: packagePath, version } ]

-- | Copy files from the package source directory to the destination directory
-- | for the tarball. This will copy all always-included files as well as files
-- | provided by the user via the `files` key.
-- | Finally, it removes any files specified in the globs behind the `excludeFiles` key.
copyPackageSourceFiles
  :: forall r
   . { includeFiles :: Maybe (NonEmptyArray NonEmptyString)
     , excludeFiles :: Maybe (NonEmptyArray NonEmptyString)
     , source :: FilePath
     , destination :: FilePath
     }
  -> Run (LOG + EXCEPT String + AFF + EFFECT + r) Unit
copyPackageSourceFiles { includeFiles, excludeFiles, source, destination } = do
  Log.debug $ "Copying package source files from " <> source <> " to " <> destination

  userExcludeFiles <- case excludeFiles of
    Nothing -> pure []
    Just nonEmptyGlobs -> do
      let globs = map NonEmptyString.toString $ NonEmptyArray.toArray nonEmptyGlobs
      { succeeded, failed } <- FastGlob.match source globs
      -- Since we will only subtract the excluded globs we can safely ignore the failed globs
      Log.warn $ "The following paths matched by globs in the 'exclude' key are outside your package directory: " <> String.joinWith ", " failed
      case validateNoExcludedObligatoryFiles succeeded of
        Right _ -> pure succeeded
        Left removedObligatoryFiles -> do
          Log.warn $ "The following paths matched by globs in the 'excludeFiles' key will be included in the tarball and cannot be excluded: " <> String.joinWith ", " (NonEmptySet.toUnfoldable removedObligatoryFiles)
          pure $ succeeded Array.\\ NonEmptySet.toUnfoldable removedObligatoryFiles

  userFiles <- case includeFiles of
    Nothing -> pure []
    Just nonEmptyGlobs -> do
      let globs = map NonEmptyString.toString $ NonEmptyArray.toArray nonEmptyGlobs
      { succeeded, failed } <- FastGlob.match source globs
      let succeededAndNotExcluded = succeeded Array.\\ userExcludeFiles

      unless (Array.null failed) do
        Except.throw $ String.joinWith " "
          [ "Some paths matched by globs in the 'includeFiles' key are outside your package directory."
          , "Please ensure globs only match within your package directory, including symlinks."
          ]

      case NonEmptyArray.fromArray (Array.filter (Regex.test Internal.Path.pursFileExtensionRegex) succeededAndNotExcluded) of
        Nothing -> pure unit
        Just matches -> do
          let fullPaths = map (\path -> Path.concat [ source, path ]) matches
          Operation.Validation.validatePursModules fullPaths >>= case _ of
            Left formattedError ->
              Except.throw $ "Some PureScript modules listed in the 'includeFiles' section of your manifest contain malformed or disallowed module names." <> formattedError
            Right _ ->
              pure unit

      pure succeeded

  includedFiles <- FastGlob.match source includedGlobs
  includedInsensitiveFiles <- FastGlob.match' source includedInsensitiveGlobs { caseSensitive: false }

  let filesToCopy = (userFiles <> includedFiles.succeeded <> includedInsensitiveFiles.succeeded) Array.\\ userExcludeFiles
  let makePaths path = { from: Path.concat [ source, path ], to: Path.concat [ destination, path ], preserveTimestamps: true }

  case map makePaths filesToCopy of
    [] -> Log.warn "No files found in copyPackageSourceFiles to copy to the packaging directory."
    xs -> do
      Log.debug $ Array.fold
        [ "Found files to copy:"
        , Array.foldMap (\{ from, to } -> "\n  - " <> from <> " to " <> to) xs
        ]
      traverse_ FS.Extra.copy xs

-- | We always ignore some files and directories when packaging a tarball, such
-- | as common version control directories, even if a user has explicitly opted
-- | in to those files with the 'files' key.
-- |
-- | See also:
-- | https://docs.npmjs.com/cli/v8/configuring-npm/package-json#files
removeIgnoredTarballFiles :: forall m. MonadAff m => FilePath -> m Unit
removeIgnoredTarballFiles path = do
  globMatches <- FastGlob.match' path ignoredGlobs { caseSensitive: false }
  for_ (ignoredDirectories <> ignoredFiles <> globMatches.succeeded) \match ->
    FS.Extra.remove (Path.concat [ path, match ])

jsonToDhallManifest :: FilePath -> String -> Aff (Either String String)
jsonToDhallManifest dhallTypes jsonStr = do
  let cmd = "json-to-dhall"
  -- Dhall requires that the path begin with './', but joining paths together with Node
  -- will remove the './' prefix. We need to manually append this to the relative path.
  let args = [ "--records-loose", "--unions-strict", Path.concat [ dhallTypes, "v1", "Manifest.dhall" ] ]
  process <- Execa.execa cmd args identity
  for_ process.stdin \{ writeUtf8End } -> writeUtf8End jsonStr
  result <- process.getResult
  pure case result.exit of
    Normally 0 -> Right jsonStr
    _ -> Left result.stderr

getPacchettiBotti :: forall r. Run (PACCHETTIBOTTI_ENV + r) Owner
getPacchettiBotti = do
  { publicKey } <- Env.askPacchettiBotti
  pure $ Owner
    { id: Just pacchettibottiEmail
    , keytype: pacchettibottiKeyType
    , public: publicKey
    }

packagingTeam :: Team
packagingTeam = { org: "purescript", team: "packaging" }

type AdjustManifest =
  { source :: FilePath
  , compiler :: Version
  , manifest :: Manifest
  , legacyIndex :: Maybe DependencyIndex
  , currentIndex :: CompilerIndex
  , resolutions :: Maybe (Map PackageName Version)
  }

-- | Conform a legacy manifest to the Registry requirements for dependencies,
-- | ie. that all direct imports are listed (no transitive dependencies) and no
-- | unused dependencies are listed.
conformLegacyManifest
  :: forall r
   . Manifest
  -> Version
  -> CompilerIndex
  -> Solver.TransitivizedRegistry
  -> ValidateDepsError
  -> Run (LOG + EXCEPT String + r) (Tuple Manifest (Map PackageName Version))
conformLegacyManifest (Manifest manifest) compiler currentIndex legacyRegistry problem = do
  let
    manifestRequired :: SemigroupMap PackageName Intersection
    manifestRequired = Solver.initializeRequired manifest.dependencies

  legacyResolutions <- case Solver.solveFull { registry: legacyRegistry, required: manifestRequired } of
    Left unsolvableLegacy -> do
      Log.warn $ "Legacy resolutions not solvable\n" <> NonEmptyList.foldMap (append "\n  - " <<< Solver.printSolverError) unsolvableLegacy
      case Solver.solveWithCompiler (Range.exact compiler) currentIndex manifest.dependencies of
        Left unsolvableCurrent -> Except.throw $ "Resolutions not solvable\n" <> NonEmptyList.foldMap (append "\n  - " <<< Solver.printSolverError) unsolvableCurrent
        Right (Tuple _ solved) -> do
          Log.debug $ "Got current resolutions as a fallback to unsolvable legacy resolutions:\n" <> printJson (Internal.Codec.packageMap Version.codec) solved
          pure solved
    Right solved -> do
      Log.debug $ "Got legacy resolutions:\n" <> printJson (Internal.Codec.packageMap Version.codec) solved
      pure solved

  let
    legacyTransitive :: Map PackageName Range
    legacyTransitive =
      Map.mapMaybe (\intersect -> Range.mk (Solver.lowerBound intersect) (Solver.upperBound intersect))
        $ Safe.Coerce.coerce
        $ _.required
        $ Solver.solveSteps
        $ Solver.solveSeed
        $ Solver.withReachable { registry: legacyRegistry, required: manifestRequired }

  Log.debug $ "Got transitive solution:\n" <> printJson (Internal.Codec.packageMap Range.codec) legacyTransitive

  let
    associateMissing :: Array PackageName -> Map PackageName Range
    associateMissing packages = do
      -- First we look up the package in the produced transitive ranges, as those
      -- are the most likely to be correct.
      let associateTransitive pkg = maybe (Left pkg) (\range -> Right (Tuple pkg range)) (Map.lookup pkg legacyTransitive)
      let associated = partitionEithers (map associateTransitive packages)
      let foundFromTransitive = Map.fromFoldable associated.success

      -- If not found, we search for the ranges described for this dependency
      -- in the manifests of the packages in the resolutions.
      let
        resolutionRanges :: Map PackageName Range
        resolutionRanges = do
          let
            foldFn name prev version = fromMaybe prev do
              versions <- Map.lookup name (un SemigroupMap legacyRegistry)
              deps <- Map.lookup version (un SemigroupMap versions)
              let deps' = Map.mapMaybe (\intersect -> Range.mk (Solver.lowerBound intersect) (Solver.upperBound intersect)) (un SemigroupMap deps)
              pure $ Map.unionWith (\l r -> fromMaybe l (Range.intersect l r)) prev deps'

          foldlWithIndex foldFn Map.empty legacyResolutions

        foundFromResolutions :: Map PackageName Range
        foundFromResolutions = Map.fromFoldable do
          associated.fail # Array.mapMaybe \pkg -> map (Tuple pkg) (Map.lookup pkg resolutionRanges)

      Map.union foundFromTransitive foundFromResolutions

    fixUnused names (Manifest m) = do
      let unused = Map.fromFoldable $ NonEmptySet.map (\name -> Tuple name unit) names
      let fixedDependencies = Map.difference m.dependencies unused
      case Solver.solveWithCompiler (Range.exact compiler) currentIndex fixedDependencies of
        Left unsolvable -> do
          Log.warn $ "Fixed dependencies cannot be used to produce a viable solution: " <> printJson (Internal.Codec.packageMap Range.codec) fixedDependencies
          Except.throw $ "Resolutions not solvable\n" <> NonEmptyList.foldMap (append "\n  - " <<< Solver.printSolverError) unsolvable
        Right (Tuple _ solved) -> pure $ Tuple fixedDependencies solved

    fixMissing names (Manifest m) = do
      let fixedDependencies = Map.union m.dependencies (associateMissing (NonEmptySet.toUnfoldable names))
      -- Once we've fixed the missing dependencies we need to be sure we can still
      -- produce a viable solution with the current index.
      case Solver.solveWithCompiler (Range.exact compiler) currentIndex fixedDependencies of
        Left unsolvable -> do
          Log.warn $ "Fixed dependencies cannot be used to produce a viable solution: " <> printJson (Internal.Codec.packageMap Range.codec) fixedDependencies
          Except.throw $ "Resolutions not solvable\n" <> NonEmptyList.foldMap (append "\n  - " <<< Solver.printSolverError) unsolvable
        Right (Tuple _ solved) -> pure $ Tuple fixedDependencies solved

    previousDepsMessage = Array.fold
      [ "Your package is using a legacy manifest format, so we have adjusted your dependencies to remove unused ones and add direct-imported ones. "
      , "Your dependency list was:\n"
      , "```json\n"
      , printJson (Internal.Codec.packageMap Range.codec) manifest.dependencies
      , "\n```\n"
      ]

    newDepsMessage (Manifest new) = Array.fold
      [ "\nYour new dependency list is:\n"
      , "```json\n"
      , printJson (Internal.Codec.packageMap Range.codec) new.dependencies
      , "\n```\n"
      ]

  case problem of
    UnusedDependencies names -> do
      Tuple deps resolutions <- fixUnused names (Manifest manifest)
      let newManifest = Manifest (manifest { dependencies = deps })
      Log.notice $ Array.fold
        [ previousDepsMessage
        , "\nWe have removed the following packages: " <> String.joinWith ", " (map PackageName.print (NonEmptySet.toUnfoldable names)) <> "\n"
        , newDepsMessage newManifest
        ]
      pure $ Tuple newManifest resolutions
    MissingDependencies names -> do
      Tuple deps resolutions <- fixMissing names (Manifest manifest)
      let newManifest = Manifest (manifest { dependencies = deps })
      Log.notice $ Array.fold
        [ previousDepsMessage
        , "\nWe have added the following packages: " <> String.joinWith ", " (map PackageName.print (NonEmptySet.toUnfoldable names)) <> "\n"
        , newDepsMessage newManifest
        ]
      pure $ Tuple newManifest resolutions
    UnusedAndMissing { missing, unused } -> do
      let unused' = Map.fromFoldable $ NonEmptySet.map (\name -> Tuple name unit) unused
      let trimmed = Map.difference manifest.dependencies unused'
      Tuple newDeps newResolutions <- fixMissing missing (Manifest (manifest { dependencies = trimmed }))
      let newManifest = Manifest (manifest { dependencies = newDeps })
      Log.notice $ Array.fold
        [ previousDepsMessage
        , "\nWe have removed the following packages: " <> String.joinWith ", " (map PackageName.print (NonEmptySet.toUnfoldable unused)) <> "\n"
        , "We have added the following packages: " <> String.joinWith ", " (map PackageName.print (NonEmptySet.toUnfoldable missing)) <> "\n"
        , newDepsMessage newManifest
        ]
      pure $ Tuple newManifest newResolutions

type COMPILER_CACHE r = (compilerCache :: Cache CompilerCache | r)

_compilerCache :: Proxy "compilerCache"
_compilerCache = Proxy

data CompilerCache :: (Type -> Type -> Type) -> Type -> Type
data CompilerCache c a = Compilation Manifest (Map PackageName Version) Version (c { target :: Version, result :: Either CompilerFailure Unit } a)

instance Functor2 c => Functor (CompilerCache c) where
  map k (Compilation manifest resolutions compiler a) = Compilation manifest resolutions compiler (map2 k a)

instance FsEncodable CompilerCache where
  encodeFs = case _ of
    Compilation (Manifest manifest) resolutions compiler next -> do
      let
        baseKey = "Compilation__" <> PackageName.print manifest.name <> "__" <> Version.print manifest.version <> "__" <> Version.print compiler <> "__"
        hashKey = do
          let resolutions' = foldlWithIndex (\name prev version -> formatPackageVersion name version <> prev) "" resolutions
          unsafePerformEffect $ Sha256.hashString resolutions'
        cacheKey = baseKey <> Sha256.print hashKey

      let
        codec = CJ.named "FindAllCompilersResult" $ CJ.Record.object
          { target: Version.codec
          , result: CJ.Common.either compilerFailureCodec CJ.null
          }

      Exists.mkExists $ Cache.AsJson cacheKey codec next

type PURS_GRAPH_CACHE r = (pursGraphCache :: Cache PursGraphCache | r)

_pursGraphCache :: Proxy "pursGraphCache"
_pursGraphCache = Proxy

data PursGraphCache :: (Type -> Type -> Type) -> Type -> Type
data PursGraphCache c a = PursGraph Manifest (Map PackageName Version) Version (c (Either CompilerFailure String) a)

instance Functor2 c => Functor (PursGraphCache c) where
  map k (PursGraph manifest resolutions compiler a) = PursGraph manifest resolutions compiler (map2 k a)

instance FsEncodable PursGraphCache where
  encodeFs = case _ of
    PursGraph (Manifest manifest) resolutions compiler next -> do
      let
        baseKey = "PursGraph__" <> PackageName.print manifest.name <> "__" <> Version.print manifest.version <> "__" <> Version.print compiler <> "__"
        hashKey = do
          let resolutions' = foldlWithIndex (\name prev version -> formatPackageVersion name version <> prev) "" resolutions
          unsafePerformEffect $ Sha256.hashString resolutions'
        cacheKey = baseKey <> Sha256.print hashKey

      let codec = CJ.Common.either compilerFailureCodec CJ.string

      Exists.mkExists $ Cache.AsJson cacheKey codec next

-- | Errors that can occur when validating license consistency
data LicenseValidationError = LicenseMismatch
  { manifestLicense :: License
  , detectedLicenses :: Array License
  }

derive instance Eq LicenseValidationError

printLicenseValidationError :: LicenseValidationError -> String
printLicenseValidationError = case _ of
  LicenseMismatch { manifestLicense, detectedLicenses } -> Array.fold
    [ "License mismatch: The manifest specifies license '"
    , License.print manifestLicense
    , "' but the following license(s) were detected in your repository: "
    , String.joinWith ", " (map License.print detectedLicenses)
    , ". Please ensure your manifest license accurately represents all licenses "
    , "in your repository. If multiple licenses apply, join them using SPDX "
    , "conjunctions (e.g., 'MIT AND Apache-2.0' or 'MIT OR Apache-2.0')."
    ]

-- | Validate that the license in the manifest is consistent with licenses
-- | detected in the repository (LICENSE file, package.json, bower.json).
-- |
-- | This check ensures that the SPDX identifier asserted in the package
-- | manifest accurately represents the licenses present in the repository.
-- | If multiple distinct licenses are detected, they must all be represented
-- | in the manifest license (e.g., joined with AND or OR).
validateLicense :: forall r. FilePath -> License -> Run (LOG + AFF + r) (Maybe LicenseValidationError)
validateLicense packageDir manifestLicense = do
  Log.debug "Detecting licenses from repository files..."
  detected <- Run.liftAff $ Licensee.detect packageDir
  case detected of
    Left err -> do
      Log.warn $ "License detection failed, relying on manifest: " <> err
      pure Nothing
    Right detectedStrings -> do
      let
        parsedLicenses :: Array License
        parsedLicenses = Array.mapMaybe (hush <<< License.parse) detectedStrings

      Log.debug $ "Detected licenses: " <> String.joinWith ", " detectedStrings

      if Array.null parsedLicenses then do
        Log.debug "No licenses detected from repository files, nothing to validate."
        pure Nothing
      else case License.extractIds manifestLicense of
        Left err -> do
          -- This shouldn't be possible (we have already validated the license)
          -- as part of constructing the manifest
          Log.warn $ "Could not extract license IDs from manifest: " <> err
          pure Nothing
        Right manifestIds -> do
          let
            manifestIdSet = Set.fromFoldable manifestIds

            -- A detected license is covered if all its IDs are in the manifest IDs
            isCovered :: License -> Boolean
            isCovered license = case License.extractIds license of
              Left _ -> false
              Right ids -> Array.all (\id -> Set.member id manifestIdSet) ids

            uncoveredLicenses :: Array License
            uncoveredLicenses = Array.filter (not <<< isCovered) parsedLicenses

          if Array.null uncoveredLicenses then do
            Log.debug "All detected licenses are covered by the manifest license."
            pure Nothing
          else do
            Log.warn $ Array.fold
              [ "License mismatch detected: manifest has '"
              , License.print manifestLicense
              , "' but detected "
              , String.joinWith ", " (map License.print parsedLicenses)
              ]
            pure $ Just $ LicenseMismatch
              { manifestLicense
              , detectedLicenses: uncoveredLicenses
              }
