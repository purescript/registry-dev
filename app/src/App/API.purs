module Registry.App.API
  ( AuthenticatedEffects
  , COMPILER_CACHE
  , CompilerCache
  , PackageSetUpdateEffects
  , PublishEffects
  , _compilerCache
  , authenticated
  , copyPackageSourceFiles
  , findAllCompilers
  , formatPursuitResolutions
  , installBuildPlan
  , packageSetUpdate
  , packagingTeam
  , parseInstalledModulePath
  , publish
  , readCompilerIndex
  , removeIgnoredTarballFiles
  , spagoToManifest
  ) where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.Exists as Exists
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Newtype (over, unwrap)
import Data.Number.Format as Number.Format
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.String.NonEmpty (fromString) as NonEmptyString
import Data.String.NonEmpty.Internal (toString) as NonEmptyString
import Data.String.Regex as Regex
import Effect.Aff as Aff
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
import Node.FS.Sync as FS.Sync
import Node.Library.Execa as Execa
import Node.Path as Path
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Registry.App.Auth as Auth
import Registry.App.CLI.Purs (CompilerFailure(..), compilerFailureCodec)
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.PursVersions as PursVersions
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.Cache (class FsEncodable, Cache)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment (COMMENT)
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV, RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (Change(..), PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Pursuit (PURSUIT)
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Source (SOURCE)
import Registry.App.Effect.Source as Source
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.Manifest (LEGACY_CACHE)
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersion(..), rawPackageNameMapCodec)
import Registry.Constants (ignoredDirectories, ignoredFiles, ignoredGlobs, includedGlobs, includedInsensitiveGlobs)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (IssueNumber(..), Team)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tmp as Tmp
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Path as Internal.Path
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, AuthenticatedPackageOperation(..), PackageSetUpdateData, PublishData)
import Registry.Operation as Operation
import Registry.Operation.Validation (UnpublishError(..), validateNoExcludedObligatoryFiles)
import Registry.Operation.Validation as Operation.Validation
import Registry.Owner as Owner
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.PursGraph (ModuleName(..))
import Registry.PursGraph as PursGraph
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver (SolverErrors)
import Registry.Solver as Solver
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Spago.Core.Config as Spago.Config
import Spago.FS as Spago.FS

type PackageSetUpdateEffects r = (REGISTRY + PACKAGE_SETS + GITHUB + GITHUB_EVENT_ENV + COMMENT + LOG + EXCEPT String + r)

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
      Except.throw "There is no latest package set."
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
        Except.throw $ Array.fold
          [ "This package set update changes the compiler version or removes a "
          , "package from the package set. Only members of the "
          , "@purescript/packaging team can take these actions, but we were "
          , "unable to authenticate your account."
          ]
      Right members -> do
        unless (Array.elem username members) do
          Log.error $ "Username " <> username <> " is not a member of the packaging team, aborting..."
          Except.throw $ Array.fold
            [ "This package set update changes the compiler version or "
            , "removes a package from the package set. Only members of the "
            , "@purescript/packaging team can take these actions, but your "
            , "username is not a member of the packaging team."
            ]
        Log.debug $ "Authentication verified for package set update by user " <> username

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
  Comment.comment "Attempting to build package set update."
  PackageSets.upgradeAtomic latestPackageSet (fromMaybe prevCompiler payload.compiler) changeSet >>= case _ of
    Left error ->
      Except.throw $ "The package set produced from this suggested update does not compile:\n\n" <> error
    Right packageSet -> do
      let commitMessage = PackageSets.commitMessage latestPackageSet changeSet (un PackageSet packageSet).version
      Registry.writePackageSet packageSet commitMessage
      Comment.comment "Built and released a new package set! Now mirroring to the package-sets repo..."
      Registry.mirrorPackageSet packageSet
      Comment.comment "Mirrored a new legacy package set."

type AuthenticatedEffects r = (REGISTRY + STORAGE + GITHUB + PACCHETTIBOTTI_ENV + COMMENT + LOG + EXCEPT String + AFF + EFFECT + r)

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
        Storage.delete payload.name payload.version
        Registry.writeMetadata payload.name updated
        Registry.deleteManifest payload.name payload.version
        Comment.comment $ "Unpublished " <> formatted <> "!"

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
        Comment.comment "Successfully transferred your package!"
        Registry.mirrorLegacyRegistry payload.name payload.newLocation
        Comment.comment "Mirrored registry operation to the legacy registry."

type PublishEffects r = (RESOURCE_ENV + PURSUIT + REGISTRY + STORAGE + SOURCE + GITHUB + COMPILER_CACHE + LEGACY_CACHE + COMMENT + LOG + EXCEPT String + AFF + EFFECT + r)

-- | Publish a package via the 'publish' operation. If the package has not been
-- | published before then it will be registered and the given version will be
-- | upload. If it has been published before then the existing metadata will be
-- | updated with the new version.
publish :: forall r. PublishData -> Run (PublishEffects + r) Unit
publish payload = do
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
  { path: packageDirectory, published: publishedTime } <- Source.fetch tmp existingMetadata.location payload.ref

  Log.debug $ "Package downloaded to " <> packageDirectory <> ", verifying it contains a src directory with valid modules..."
  Internal.Path.readPursFiles (Path.concat [ packageDirectory, "src" ]) >>= case _ of
    Nothing ->
      Except.throw $ Array.fold
        [ "This package has no PureScript files in its `src` directory. "
        , "All package sources must be in the `src` directory, with any additional "
        , "sources indicated by the `files` key in your manifest."
        ]
    Just files ->
      -- The 'validatePursModules' function uses language-cst-parser, which only
      -- supports syntax back to 0.15.0. We'll still try to validate the package
      -- but it may fail to parse.
      Operation.Validation.validatePursModules files >>= case _ of
        Left formattedError | payload.compiler < unsafeFromRight (Version.parse "0.15.0") -> do
          Log.debug $ "Package failed to parse in validatePursModules: " <> formattedError
          Log.debug $ "Skipping check because package is published with a pre-0.15.0 compiler (" <> Version.print payload.compiler <> ")."
        Left formattedError ->
          Except.throw $ Array.fold
            [ "This package has either malformed or disallowed PureScript module names "
            , "in its source: "
            , formattedError
            ]
        Right _ ->
          Log.debug "Package contains well-formed .purs files in its src directory."

  -- If the package doesn't have a purs.json we can try to make one - possible scenarios:
  --  - in case it has a spago.yaml then we know how to read that, and have all the info to move forward
  --  - if it's a legacy import then we can try to infer as much info as possible to make a manifest
  let packagePursJson = Path.concat [ packageDirectory, "purs.json" ]
  hadPursJson <- Run.liftEffect $ FS.Sync.exists packagePursJson

  let packageSpagoYaml = Path.concat [ packageDirectory, "spago.yaml" ]
  hasSpagoYaml <- Run.liftEffect $ FS.Sync.exists packageSpagoYaml

  Manifest manifest <-
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
              Log.error $ "Failed to parse manifest: " <> CA.printJsonDecodeError err
              Except.throw $ "Found a purs.json file in the package source, but it could not be decoded."
            Right manifest -> do
              Log.debug $ "Read a valid purs.json manifest from the package source:\n" <> stringifyJson Manifest.codec manifest
              pure manifest

    else if hasSpagoYaml then do
      Comment.comment $ "Package source does not have a purs.json file, creating one from your spago.yaml file..."
      Run.liftAff (Spago.FS.readYamlDocFile Spago.Config.configCodec packageSpagoYaml) >>= case _ of
        Left readError -> Except.throw $ String.joinWith "\n"
          [ "Could not publish your package - a spago.yaml was present, but it was not possible to read it:"
          , readError
          ]
        Right { yaml: config } -> do
          -- Once we have the config we are still not entirely sure it fits into a Manifest
          -- e.g. need to make sure all the ranges are present
          case spagoToManifest config of
            Left err -> Except.throw $ String.joinWith "\n"
              [ "Could not publish your package - there was an error while converting your spago.yaml into a purs.json manifest:"
              , err
              ]
            Right manifest -> do
              Log.debug "Successfully converted a spago.yaml into a purs.json manifest"
              pure manifest
    else do
      Comment.comment $ "Package source does not have a purs.json file. Creating one from your bower.json and/or spago.dhall files..."
      address <- case existingMetadata.location of
        Git _ -> Except.throw "Legacy packages can only come from GitHub."
        GitHub { subdir: Just subdir } -> Except.throw $ "Legacy packages cannot use the 'subdir' key, but this package specifies a " <> subdir <> " subdir."
        GitHub { owner, repo } -> pure { owner, repo }

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
          let manifest = Legacy.Manifest.toManifest payload.name version existingMetadata.location legacyManifest
          pure manifest

  Comment.comment "Verifying package..."

  -- We trust the manifest for any changes to the 'owners' field, but for all
  -- other fields we trust the registry metadata.
  let metadata = existingMetadata { owners = manifest.owners }
  unless (Operation.Validation.nameMatches (Manifest manifest) payload) do
    Except.throw $ Array.fold
      [ "The manifest file specifies a package name ("
      , PackageName.print manifest.name
      , ") that differs from the package name submitted to the API ("
      , PackageName.print payload.name
      , "). The manifest and API request must match."
      ]

  unless (Operation.Validation.locationMatches (Manifest manifest) (Metadata metadata)) do
    Except.throw $ Array.fold
      [ "The manifest file specifies a location ("
      , stringifyJson Location.codec manifest.location
      , ") that differs from the location in the registry metadata ("
      , stringifyJson Location.codec metadata.location
      , "). If you would like to change the location of your package you should "
      , "submit a transfer operation."
      ]

  when (Operation.Validation.isMetadataPackage (Manifest manifest)) do
    Except.throw "The `metadata` package cannot be uploaded to the registry because it is a protected package."

  for_ (Operation.Validation.isNotUnpublished (Manifest manifest) (Metadata metadata)) \info -> do
    Except.throw $ String.joinWith "\n"
      [ "You tried to upload a version that has been unpublished: " <> Version.print manifest.version
      , ""
      , "```json"
      , printJson Metadata.unpublishedMetadataCodec info
      , "```"
      ]

  for_ (Operation.Validation.isNotPublished (Manifest manifest) (Metadata metadata)) \info -> do
    -- If the package has been published already, then we check whether the published
    -- version has made it to Pursuit or not. If it has, then we terminate here. If
    -- it hasn't then we skip to Pursuit publishing.
    published <- Pursuit.getPublishedVersions manifest.name >>= case _ of
      Left error -> Except.throw error
      Right versions -> pure versions

    case Map.lookup manifest.version published of
      Nothing | payload.compiler < unsafeFromRight (Version.parse "0.14.7") -> do
        Comment.comment $ Array.fold
          [ "This version has already been published to the registry, but the docs have not been "
          , "uploaded to Pursuit. Unfortunately, it is not possible to publish to Pursuit via the "
          , "registry using compiler versions prior to 0.14.7. Please try with a later compiler."
          ]
      Nothing -> do
        Comment.comment $ Array.fold
          [ "This version has already been published to the registry, but the docs have not been "
          , "uploaded to Pursuit. Skipping registry publishing and retrying Pursuit publishing..."
          ]
        verifiedResolutions <- verifyResolutions payload.compiler (Manifest manifest) payload.resolutions
        compilationResult <- compilePackage { source: packageDirectory, compiler: payload.compiler, resolutions: verifiedResolutions }
        case compilationResult of
          Left error -> do
            Log.error $ "Compilation failed, cannot upload to pursuit: " <> error
            Except.throw "Cannot publish to Pursuit because this package failed to compile."
          Right installedResolutions -> do
            Log.debug "Uploading to Pursuit"
            publishToPursuit { source: packageDirectory, compiler: payload.compiler, resolutions: verifiedResolutions, installedResolutions } >>= case _ of
              Left publishErr -> Except.throw publishErr
              Right _ -> do
                Log.debug "Package docs publish succeeded"
                Comment.comment "Successfully uploaded package docs to Pursuit! 🎉 🚀"

      Just url -> do
        Except.throw $ String.joinWith "\n"
          [ "You tried to upload a version that already exists: " <> Version.print manifest.version
          , ""
          , "Its metadata is:"
          , "```json"
          , printJson Metadata.publishedMetadataCodec info
          , "```"
          , ""
          , "and its documentation is available here:"
          , url
          ]

  -- Now that we've verified the package we can write the manifest to the source
  -- directory and then publish it.
  if hadPursJson then do
    -- No need to verify the generated manifest because nothing was generated,
    -- and no need to write a file (it's already in the package source.)
    publishRegistry
      { manifest: Manifest manifest
      , metadata: Metadata metadata
      , payload
      , publishedTime
      , tmp
      , packageDirectory
      }

  else if hasSpagoYaml then do
    -- We need to write the generated purs.json file, but because spago-next
    -- already does unused dependency checks and supports explicit test-only
    -- dependencies we can skip those checks.
    Run.liftAff $ writeJsonFile Manifest.codec packagePursJson (Manifest manifest)
    publishRegistry
      { manifest: Manifest manifest
      , metadata: Metadata metadata
      , payload
      , publishedTime
      , tmp
      , packageDirectory
      }

  -- Otherwise this is a legacy package, generated from a combination of bower,
  -- spago.dhall, and package set files, so we need to verify the generated
  -- manifest does not contain unused dependencies before writing it.
  else do
    Log.debug "Pruning unused dependencies from legacy package manifest..."

    Log.debug "Solving manifest to get all transitive dependencies."
    resolutions <- verifyResolutions payload.compiler (Manifest manifest) payload.resolutions

    Log.debug "Installing dependencies."
    tmpDepsDir <- Tmp.mkTmpDir
    installBuildPlan resolutions tmpDepsDir

    Log.debug "Discovering used dependencies from source."
    let srcGlobs = Path.concat [ packageDirectory, "src", "**", "*.purs" ]
    let depGlobs = Path.concat [ tmpDepsDir, "*", "src", "**", "*.purs" ]
    let command = Purs.Graph { globs: [ srcGlobs, depGlobs ] }
    -- We need to use the minimum compiler version that supports 'purs graph'
    let minGraphCompiler = unsafeFromRight (Version.parse "0.13.8")
    let callCompilerVersion = if payload.compiler >= minGraphCompiler then payload.compiler else minGraphCompiler
    Run.liftAff (Purs.callCompiler { command, version: Just callCompilerVersion, cwd: Nothing }) >>= case _ of
      Left err -> do
        let prefix = "Failed to discover unused dependencies because purs graph failed: "
        Except.throw $ prefix <> case err of
          UnknownError str -> str
          CompilationError errs -> "\n" <> Purs.printCompilerErrors errs
          MissingCompiler -> "missing compiler " <> Version.print payload.compiler
      Right output -> case Argonaut.Parser.jsonParser output of
        Left parseErr -> Except.throw $ "Failed to parse purs graph output as JSON while finding unused dependencies: " <> parseErr
        Right json -> case CA.decode PursGraph.pursGraphCodec json of
          Left decodeErr -> Except.throw $ "Failed to decode JSON from purs graph output while finding unused dependencies: " <> CA.printJsonDecodeError decodeErr
          Right graph -> do
            Log.debug "Got a valid graph of source and dependencies. Removing install dir and associating discovered modules with their packages..."
            FS.Extra.remove tmpDepsDir

            let
              -- We need access to a graph that _doesn't_ include the package
              -- source, because we only care about dependencies of the package.
              noSrcGraph = Map.filter (isNothing <<< String.stripPrefix (String.Pattern packageDirectory) <<< _.path) graph
              pathParser = map _.name <<< parseInstalledModulePath <<< { prefix: tmpDepsDir, path: _ }

            case PursGraph.associateModules pathParser noSrcGraph of
              Left errs ->
                Except.throw $ String.joinWith "\n"
                  [ "Failed to associate modules with packages while finding unused dependencies:"
                  , flip NonEmptyArray.foldMap1 errs \{ error, module: ModuleName moduleName, path } ->
                      "  - " <> moduleName <> " (" <> path <> "): " <> error <> "\n"
                  ]
              Right modulePackageMap -> do
                Log.debug "Associated modules with their package names. Finding all modules used in package source..."
                -- The modules used in the package source code are any that have
                -- a path beginning with the package source directory. We only
                -- care about dependents of these modules.
                let sourceModules = Map.keys $ Map.filter (isJust <<< String.stripPrefix (String.Pattern packageDirectory) <<< _.path) graph

                Log.debug "Found all modules used in package source. Finding all modules used by those modules..."
                let allReachableModules = PursGraph.allDependenciesOf sourceModules graph

                -- Then we can associate each reachable module with its package
                -- name to get the full set of used package names.
                let allUsedPackages = Set.mapMaybe (flip Map.lookup modulePackageMap) allReachableModules

                -- Finally, we can use this to find the unused dependencies.
                Log.debug "Found all packages reachable by the project source code. Determining unused dependencies..."
                case Operation.Validation.getUnusedDependencies (Manifest manifest) resolutions allUsedPackages of
                  Nothing -> do
                    Log.debug "No unused dependencies! This manifest is good to go."
                    Run.liftAff $ writeJsonFile Manifest.codec packagePursJson (Manifest manifest)
                    publishRegistry
                      { manifest: Manifest manifest
                      , metadata: Metadata metadata
                      , payload
                      , publishedTime
                      , tmp
                      , packageDirectory
                      }
                  Just isUnused -> do
                    let printed = String.joinWith ", " (PackageName.print <$> NonEmptySet.toUnfoldable isUnused)
                    Log.debug $ "Found unused dependencies: " <> printed
                    Comment.comment $ "Generated legacy manifest contains unused dependencies which will be removed: " <> printed
                    let verified = manifest { dependencies = Map.filterKeys (not <<< flip NonEmptySet.member isUnused) manifest.dependencies }
                    Log.debug "Writing updated, pruned manifest."
                    Run.liftAff $ writeJsonFile Manifest.codec packagePursJson (Manifest verified)
                    publishRegistry
                      { manifest: Manifest verified
                      , metadata: Metadata metadata
                      , payload
                      , publishedTime
                      , tmp
                      , packageDirectory
                      }

type PublishRegistry =
  { manifest :: Manifest
  , metadata :: Metadata
  , payload :: PublishData
  , publishedTime :: DateTime
  , tmp :: FilePath
  , packageDirectory :: FilePath
  }

-- A private helper function for publishing to the registry. Separated out of
-- the main 'publish' function because we sometimes use the publish function to
-- publish to Pursuit only (in the case the package has been pushed to the
-- registry, but docs have not been uploaded).
publishRegistry :: forall r. PublishRegistry -> Run (PublishEffects + r) Unit
publishRegistry { payload, metadata: Metadata metadata, manifest: Manifest manifest, publishedTime, tmp, packageDirectory } = do
  Log.debug "Verifying the package build plan..."
  verifiedResolutions <- verifyResolutions payload.compiler (Manifest manifest) payload.resolutions

  Log.debug "Verifying that the package dependencies are all registered..."
  unregisteredRef <- Run.liftEffect $ Ref.new Map.empty
  forWithIndex_ verifiedResolutions \name version -> do
    Registry.readMetadata name >>= case _ of
      Nothing -> Run.liftEffect $ Ref.modify_ (Map.insert name version) unregisteredRef
      Just (Metadata { published }) -> case Map.lookup version published of
        Nothing -> Run.liftEffect $ Ref.modify_ (Map.insert name version) unregisteredRef
        Just _ -> pure unit
  unregistered <- Run.liftEffect $ Ref.read unregisteredRef
  unless (Map.isEmpty unregistered) do
    Except.throw $ Array.fold
      [ "Cannot register this package because it has unregistered dependencies: "
      , Array.foldMap (\(Tuple name version) -> "\n  - " <> formatPackageVersion name version) (Map.toUnfoldable unregistered)
      ]

  Log.info "Packaging tarball for upload..."
  let newDir = PackageName.print manifest.name <> "-" <> Version.print manifest.version
  let packageSourceDir = Path.concat [ tmp, newDir ]
  Log.debug $ "Creating packaging directory at " <> packageSourceDir
  FS.Extra.ensureDirectory packageSourceDir
  -- We copy over all files that are always included (ie. src dir, purs.json file),
  -- and any files the user asked for via the 'files' key, and remove all files
  -- that should never be included (even if the user asked for them).
  copyPackageSourceFiles { includeFiles: manifest.includeFiles, excludeFiles: manifest.excludeFiles, source: packageDirectory, destination: packageSourceDir }
  Log.debug "Removing always-ignored files from the packaging directory."
  removeIgnoredTarballFiles packageSourceDir

  let tarballName = newDir <> ".tar.gz"
  let tarballPath = Path.concat [ tmp, tarballName ]
  Tar.create { cwd: tmp, folderName: newDir }

  Log.info "Tarball created. Verifying its size..."
  FS.Stats.Stats { size: bytes } <- Run.liftAff $ FS.Aff.stat tarballPath
  for_ (Operation.Validation.validateTarballSize bytes) case _ of
    Operation.Validation.ExceedsMaximum maxPackageBytes ->
      Except.throw $ "Package tarball is " <> show bytes <> " bytes, which exceeds the maximum size of " <> show maxPackageBytes <> " bytes."
    Operation.Validation.WarnPackageSize maxWarnBytes ->
      Comment.comment $ "WARNING: Package tarball is " <> show bytes <> "bytes, which exceeds the warning threshold of " <> show maxWarnBytes <> " bytes."

  -- If a package has under ~30 bytes it's about guaranteed that packaging the
  -- tarball failed. This can happen if the system running the API has a non-
  -- GNU tar installed, for example.
  let minBytes = 30.0
  when (bytes < minBytes) do
    Except.throw $ "Package tarball is only " <> Number.Format.toString bytes <> " bytes, which indicates the source was not correctly packaged."

  hash <- Sha256.hashFile tarballPath
  Log.info $ "Tarball size of " <> show bytes <> " bytes is acceptable."
  Log.info $ "Tarball hash: " <> Sha256.print hash

  -- Now that we have the package source contents we can verify we can compile
  -- the package. We skip failures when the package is a legacy package.
  Comment.comment $ Array.fold
    [ "Verifying package compiles using compiler "
    , Version.print payload.compiler
    , " and resolutions:\n"
    , "```json\n"
    , printJson (Internal.Codec.packageMap Version.codec) verifiedResolutions
    , "\n```"
    ]

  installedResolutions <- compilePackage { source: packageDirectory, compiler: payload.compiler, resolutions: verifiedResolutions } >>= case _ of
    Left error -> Except.throw error
    Right installed -> pure installed

  Comment.comment "Package is verified! Uploading it to the storage backend..."
  Storage.upload manifest.name manifest.version tarballPath
  Log.debug $ "Adding the new version " <> Version.print manifest.version <> " to the package metadata file."
  let newPublishedVersion = { hash, ref: payload.ref, compilers: Left payload.compiler, publishedTime, bytes }
  let newMetadata = metadata { published = Map.insert manifest.version newPublishedVersion metadata.published }
  Registry.writeMetadata manifest.name (Metadata newMetadata)
  Comment.comment "Successfully uploaded package to the registry! 🎉 🚀"

  -- We write to the registry index if possible. If this fails, the packaging
  -- team should manually insert the entry.
  Log.debug "Adding the new version to the registry index"
  Registry.writeManifest (Manifest manifest)

  Registry.mirrorLegacyRegistry payload.name newMetadata.location
  Comment.comment "Mirrored registry operation to the legacy registry!"

  Log.debug "Uploading package documentation to Pursuit"
  if payload.compiler >= unsafeFromRight (Version.parse "0.14.7") then
    publishToPursuit { source: packageDirectory, compiler: payload.compiler, resolutions: verifiedResolutions, installedResolutions } >>= case _ of
      Left publishErr -> do
        Log.error publishErr
        Comment.comment $ "Failed to publish package docs to Pursuit: " <> publishErr
      Right _ ->
        Comment.comment "Successfully uploaded package docs to Pursuit! 🎉 🚀"
  else do
    Comment.comment $ Array.fold
      [ "Skipping Pursuit publishing because this package was published with a pre-0.14.7 compiler ("
      , Version.print payload.compiler
      , "). If you want to publish documentation, please try again with a later compiler."
      ]

  allCompilers <- PursVersions.pursVersions
  { failed: invalidCompilers, succeeded: validCompilers } <- case NonEmptyArray.fromFoldable $ NonEmptyArray.filter (notEq payload.compiler) allCompilers of
    Nothing -> pure { failed: Map.empty, succeeded: Set.singleton payload.compiler }
    Just try -> do
      found <- findAllCompilers
        { source: packageDirectory
        , manifest: Manifest manifest
        , compilers: try
        }
      pure $ found { succeeded = Set.insert payload.compiler found.succeeded }

  unless (Map.isEmpty invalidCompilers) do
    Log.debug $ "Some compilers failed: " <> String.joinWith ", " (map Version.print (Set.toUnfoldable (Map.keys invalidCompilers)))

  let
    allVerified = case NonEmptySet.fromFoldable validCompilers of
      Nothing -> NonEmptyArray.singleton payload.compiler
      Just verified -> NonEmptyArray.fromFoldable1 verified

  Comment.comment $ "Found compatible compilers: " <> String.joinWith ", " (map (\v -> "`" <> Version.print v <> "`") (NonEmptyArray.toArray allVerified))
  let compilersMetadata = newMetadata { published = Map.update (Just <<< (_ { compilers = Right allVerified })) manifest.version newMetadata.published }
  Registry.writeMetadata manifest.name (Metadata compilersMetadata)
  Log.debug $ "Wrote new metadata " <> printJson Metadata.codec (Metadata compilersMetadata)

  Comment.comment "Wrote completed metadata to the registry!"
  FS.Extra.remove tmp
  FS.Extra.remove packageDirectory

-- | Verify the build plan for the package. If the user provided a build plan,
-- | we ensure that the provided versions are within the ranges listed in the
-- | manifest. If not, we solve their manifest to produce a build plan.
verifyResolutions :: forall r. Version -> Manifest -> Maybe (Map PackageName Version) -> Run (REGISTRY + LOG + AFF + EXCEPT String + r) (Map PackageName Version)
verifyResolutions compiler manifest resolutions = do
  Log.debug "Check the submitted build plan matches the manifest"
  compilerIndex <- readCompilerIndex
  case resolutions of
    Nothing -> case Operation.Validation.validateDependenciesSolve compiler manifest compilerIndex of
      Left errors -> do
        let
          printedError = String.joinWith "\n"
            [ "Could not produce valid dependencies for manifest."
            , "```"
            , errors # foldMapWithIndex \index error -> String.joinWith "\n"
                [ "[Error " <> show (index + 1) <> "]"
                , Solver.printSolverError error
                ]
            , "```"
            ]
        Except.throw printedError
      Right solved -> pure solved
    Just provided -> do
      validateResolutions manifest provided
      pure provided

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

type CompilePackage =
  { source :: FilePath
  , compiler :: Version
  , resolutions :: Map PackageName Version
  }

compilePackage :: forall r. CompilePackage -> Run (STORAGE + LOG + AFF + EFFECT + r) (Either String FilePath)
compilePackage { source, compiler, resolutions } = Except.runExcept do
  tmp <- Tmp.mkTmpDir
  output <- do
    if Map.isEmpty resolutions then do
      Log.debug "Compiling source code (no dependencies to install)..."
      Run.liftAff $ Purs.callCompiler
        { command: Purs.Compile { globs: [ "src/**/*.purs" ] }
        , version: Just compiler
        , cwd: Just source
        }
    else do
      Log.debug "Installing build plan..."
      installBuildPlan resolutions tmp
      Log.debug "Compiling..."
      Run.liftAff $ Purs.callCompiler
        { command: Purs.Compile { globs: [ "src/**/*.purs", Path.concat [ tmp, "*/src/**/*.purs" ] ] }
        , version: Just compiler
        , cwd: Just source
        }

  case output of
    Left err -> Except.throw $ printCompilerFailure compiler err
    Right _ -> pure tmp

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
  compilerIndex <- readCompilerIndex
  checkedCompilers <- for compilers \target -> do
    Log.debug $ "Trying compiler " <> Version.print target
    case Solver.solveWithCompiler (Range.exact target) compilerIndex (un Manifest manifest).dependencies of
      Left solverErrors -> pure $ Left $ Tuple target (Left solverErrors)
      Right (Tuple mbCompiler resolutions) -> do
        Log.debug $ "Solved with compiler " <> Version.print target <> " and got resolutions:\n" <> printJson (Internal.Codec.packageMap Version.codec) resolutions
        case mbCompiler of
          Nothing -> Except.throw "Produced a compiler-derived build plan with no compiler!"
          Just selected | selected /= target -> Except.throw $ Array.fold
            [ "Produced a compiler-derived build plan that selects a compiler ("
            , Version.print selected
            , ") that differs from the target compiler ("
            , Version.print target
            , ")."
            ]
          Just _ -> pure unit
        Cache.get _compilerCache (Compilation manifest resolutions target) >>= case _ of
          Nothing -> do
            workdir <- Tmp.mkTmpDir
            let installed = Path.concat [ workdir, ".registry" ]
            FS.Extra.ensureDirectory installed
            installBuildPlan resolutions installed
            result <- Run.liftAff $ Purs.callCompiler
              { command: Purs.Compile { globs: [ Path.concat [ source, "src/**/*.purs" ], Path.concat [ installed, "*/src/**/*.purs" ] ] }
              , version: Just target
              , cwd: Just workdir
              }
            FS.Extra.remove workdir
            Cache.put _compilerCache (Compilation manifest resolutions target) { target, result: map (const unit) result }
            pure $ bimap (Tuple target <<< Right) (const target) result
          Just { result } ->
            pure $ bimap (Tuple target <<< Right) (const target) result

  let results = partitionEithers $ NonEmptyArray.toArray checkedCompilers
  pure { failed: Map.fromFoldable results.fail, succeeded: Set.fromFoldable results.success }

printCompilerFailure :: Version -> CompilerFailure -> String
printCompilerFailure compiler = case _ of
  MissingCompiler -> Array.fold
    [ "Compilation failed because the build plan compiler version "
    , Version.print compiler
    , " is not supported. Please try again with a different compiler."
    ]
  CompilationError errs -> String.joinWith "\n"
    [ "Compilation failed because the build plan does not compile with version " <> Version.print compiler <> " of the compiler:"
    , "```"
    , Purs.printCompilerErrors errs
    , "```"
    ]
  UnknownError err -> String.joinWith "\n"
    [ "Compilation failed with version " <> Version.print compiler <> " because of an error :"
    , "```"
    , err
    , "```"
    ]

-- | Install all dependencies indicated by the build plan to the specified
-- | directory. Packages will be installed at 'dir/package-name-x.y.z'.
installBuildPlan :: forall r. Map PackageName Version -> FilePath -> Run (STORAGE + LOG + AFF + EXCEPT String + r) Unit
installBuildPlan resolutions dependenciesDir = do
  -- We fetch every dependency at its resolved version, unpack the tarball, and
  -- store the resulting source code in a specified directory for dependencies.
  forWithIndex_ resolutions \name version -> do
    let
      -- This filename uses the format the directory name will have once
      -- unpacked, ie. package-name-major.minor.patch
      filename = PackageName.print name <> "-" <> Version.print version <> ".tar.gz"
      filepath = Path.concat [ dependenciesDir, filename ]
    Storage.download name version filepath
    Run.liftAff (Aff.attempt (Tar.extract { cwd: dependenciesDir, archive: filename })) >>= case _ of
      Left error -> do
        Log.error $ "Failed to unpack " <> filename <> ": " <> Aff.message error
        Except.throw "Failed to unpack dependency tarball, cannot continue."
      Right _ ->
        Log.debug $ "Unpacked " <> filename
    Run.liftAff $ FS.Aff.unlink filepath
    Log.debug $ "Installed " <> formatPackageVersion name version

-- | Parse the name and version from a path to a module installed in the standard
-- | form: '<dir>/<package-name>-<x.y.z>/...'
parseInstalledModulePath :: { prefix :: FilePath, path :: FilePath } -> Either String { name :: PackageName, version :: Version }
parseInstalledModulePath { prefix, path } = do
  packageVersion <- lmap Parsing.parseErrorMessage $ Parsing.runParser path do
    _ <- Parsing.String.string prefix
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
-- | on packages prior to 0.14.7.
publishToPursuit
  :: forall r
   . PublishToPursuit
  -> Run (PURSUIT + COMMENT + LOG + AFF + EFFECT + r) (Either String Unit)
publishToPursuit { source, compiler, resolutions, installedResolutions } = Except.runExcept do
  Log.debug "Generating a resolutions file"
  tmp <- Tmp.mkTmpDir

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
      Except.throw $ printCompilerFailure compiler error
    Right publishResult -> do
      -- The output contains plenty of diagnostic lines, ie. "Compiling ..."
      -- but we only want the final JSON payload.
      let lines = String.split (String.Pattern "\n") publishResult
      case Array.last lines of
        Nothing -> Except.throw "Publishing failed because of an unexpected compiler error. cc @purescript/packaging"
        Just jsonString -> case Argonaut.Parser.jsonParser jsonString of
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

pursuitResolutionsCodec :: JsonCodec PursuitResolutions
pursuitResolutionsCodec = rawPackageNameMapCodec $ CA.Record.object "Resolution" { version: Version.codec, path: CA.string }

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
  process.stdin.writeUtf8End jsonStr
  result <- process.result
  pure case result of
    Right _ -> Right jsonStr
    Left { stderr } -> Left stderr

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

spagoToManifest :: Spago.Config.Config -> Either String Manifest
spagoToManifest config = do
  package@{ name, description, dependencies: Spago.Config.Dependencies deps } <- note "Did not find a package in the config" config.package
  publishConfig@{ version, license } <- note "Did not find a `publish` section in the package config" package.publish
  let includeFiles = NonEmptyArray.fromArray =<< (Array.mapMaybe NonEmptyString.fromString <$> publishConfig.include)
  let excludeFiles = NonEmptyArray.fromArray =<< (Array.mapMaybe NonEmptyString.fromString <$> publishConfig.exclude)
  location <- note "Did not find a `location` field in the publish config" publishConfig.location
  let
    checkRange :: Tuple PackageName (Maybe Range) -> Either PackageName (Tuple PackageName Range)
    checkRange (Tuple packageName maybeRange) = case maybeRange of
      Nothing -> Left packageName
      Just r -> Right (Tuple packageName r)
  let { fail: failedPackages, success } = partitionEithers $ map checkRange (Map.toUnfoldable deps :: Array _)
  dependencies <- case failedPackages of
    [] -> Right (Map.fromFoldable success)
    errs -> Left $ "The following packages did not have their ranges specified: " <> String.joinWith ", " (map PackageName.print errs)
  pure $ Manifest
    { version
    , license
    , name
    , location
    , description
    , dependencies
    , owners: Nothing -- TODO Spago still needs to add this to its config
    , includeFiles
    , excludeFiles
    }

readCompilerIndex :: forall r. Run (REGISTRY + AFF + EXCEPT String + r) Solver.CompilerIndex
readCompilerIndex = do
  metadata <- Registry.readAllMetadata
  manifests <- Registry.readAllManifests
  allCompilers <- PursVersions.pursVersions
  pure $ Solver.buildCompilerIndex allCompilers manifests metadata

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
        codec = CA.Record.object "FindAllCompilersResult"
          { target: Version.codec
          , result: CA.Common.either compilerFailureCodec CA.null
          }

      Exists.mkExists $ Cache.AsJson cacheKey codec next
