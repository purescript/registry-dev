module Registry.Scripts.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Alternative as Alternative
import Control.Monad.Except as Except
import Data.Argonaut as Json
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Interpolate (i)
import Data.Lens (_Left, preview)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Foreign.Tmp as Tmp
import Node.ChildProcess as ChildProcess
import Node.FS.Aff as FS
import Registry.Index (RegistryIndex, insertManifest)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..), Manifest)
import Registry.Scripts.BowerImport.BowerFile (BowerFile(..))
import Registry.Scripts.BowerImport.BowerFile as BowerFile
import Registry.Scripts.BowerImport.Error (ImportError(..), ManifestError(..), PackageFailures(..), RawPackageName(..), RawVersion(..))
import Registry.Scripts.BowerImport.Process as Process
import Registry.Scripts.BowerImport.Stats as Stats
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as StringParser

-- | This main loop uploads legacy packages to the new Registry
-- | In order to do this, we:
-- | - get an index of the legacy packages with their bowerfiles
-- | - create a graph (a tree really) where a package is a node and dependencies are edges
-- | - topologically sort this graph so that packages with no dependencies are at the root
-- | - go through this list: if the package is in the registry index then skip, otherwise upload
main :: Effect Unit
main = Aff.launchAff_ do
  log "Starting import from legacy registries..."
  registry <- downloadBowerRegistry

  let indexPath = "registry-index"
  log $ "Writing registry index to " <> indexPath

  exists <- FS.exists indexPath
  guard (not exists) $ FS.mkdir indexPath
  for_ registry \semVer -> for_ semVer (insertManifest indexPath)
  log "Done!"

downloadBowerRegistry :: Aff RegistryIndex
downloadBowerRegistry = do
  bowerPackages <- readRegistryFile "bower-packages.json"
  newPackages <- readRegistryFile "new-packages.json"

  let
    handleError _ = do
      log "bower-exclusions.json does not exist, writing..."
      let failures = PackageFailures Map.empty
      writeJsonFile "bower-exclusions.json" failures
      pure (Right failures)

  excludedPackages <- Except.catchError (readJsonFile "bower-exclusions.json") handleError >>= case _ of
    Left error -> throwError $ Aff.error $ Json.printJsonDecodeError error
    Right (failures :: PackageFailures) -> pure failures

  let
    allPackages = Map.union bowerPackages newPackages
    initialPackages =
      { failures: excludedPackages
      , packages: do
          -- We preemptively filter out packages we can't cache releases for.
          let
            shouldAccept = case _ of
              NoReleases -> false
              InvalidGitHubRepo _ -> false
              _ -> true

          Process.filterFailedPackages shouldAccept excludedPackages allPackages
      }

  log "Fetching package releases..."
  releaseIndex <- Process.forPackage initialPackages identity \name repoUrl -> do
    address <- case GitHub.parseRepo repoUrl of
      Left err -> throwError $ InvalidGitHubRepo $ StringParser.printParserError err
      Right address -> pure address

    let repoCache = Array.fold [ "releases__", address.owner, "__", address.repo ]

    releases <- Process.withCache repoCache (Just $ Hours 24.0) do
      log $ "Fetching releases for package " <> un RawPackageName name
      result <- lift $ try $ GitHub.getReleases address
      case result of
        Left err -> logShow err *> throwError NoReleases
        Right v -> pure v

    versions <- case NEA.fromArray releases of
      Nothing -> throwError NoReleases
      Just arr -> pure $ Map.fromFoldable $ map (\tag -> Tuple (RawVersion tag.name) unit) arr

    pure $ Tuple { name, address } versions

  let
    validBower = releaseIndex
      { packages = do
          -- We filter out package versions we can't cache a bowerfile for.
          let
            shouldAccept = case _ of
              MissingBowerfile -> false
              MalformedBowerJson _ -> false
              _ -> true

          Process.filterFailedPackageVersions shouldAccept releaseIndex.failures _.name releaseIndex.packages
      }

  log "Fetching package bowerfiles..."
  bowerRegistry <- Process.forPackageVersion validBower _.name identity \{ name, address } tag _ -> do
    bowerfile <- fetchBowerfile name address tag
    let packagesWithReleases = Set.map _.name $ Map.keys releaseIndex.packages
    selfContainedDependencies packagesWithReleases bowerfile
    pure bowerfile

  log "Parsing names and versions..."
  packageRegistry <- Process.forPackageVersionKeys bowerRegistry _.name identity \{ name, address } tag -> do
    packageName <- case PackageName.parse $ un RawPackageName name of
      Left err ->
        throwError $ MalformedPackageName $ StringParser.printParserError err
      Right pname ->
        pure pname

    packageSemVer <- case SemVer.parseSemVer $ un RawVersion tag of
      Nothing ->
        throwError $ ManifestError $ NEA.singleton $ BadVersion $ un RawVersion tag
      Just semVer ->
        pure semVer

    let
      outerKey = { name: packageName, original: name, address }
      innerKey = { semVer: packageSemVer, original: tag }

    pure $ Tuple outerKey innerKey

  log "Converting bowerfiles to manifests..."
  let forPackageRegistry = Process.forPackageVersion packageRegistry _.original _.original
  manifestRegistry <- forPackageRegistry \{ name, address } { semVer } bowerfile -> do
    let
      repo = GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
      liftError = map (lmap ManifestError)

    Except.mapExceptT liftError $ toManifest name repo semVer bowerfile

  let
    registryIndex :: RegistryIndex
    registryIndex = do
      let
        packageManifests :: Array (Tuple PackageName (Map SemVer Manifest))
        packageManifests =
          map (lmap _.name)
            $ map (map (Map.fromFoldable <<< map (lmap _.semVer) <<< (Map.toUnfoldable :: _ -> Array _)))
            $ Map.toUnfoldable manifestRegistry.packages

      Map.fromFoldable packageManifests

  log "Writing exclusions file..."
  writeJsonFile "./bower-exclusions.json" manifestRegistry.failures
  Stats.logStats $ Stats.errorStats manifestRegistry
  pure registryIndex

-- | Find the bower.json files associated with the package's released tags,
-- | caching the file to avoid re-fetching each time the tool runs.
fetchBowerfile :: RawPackageName -> GitHub.Address -> RawVersion -> ExceptT ImportError Aff BowerFile
fetchBowerfile name address tag = do
  let
    mkRawUrl file =
      i "https://raw.githubusercontent.com/" address.owner "/" address.repo "/" (un RawVersion tag) "/" file

    repoStr =
      i "https://github.com/" address.owner "/" address.repo

    bowerfileUrl = mkRawUrl "bower.json"
    spagoDhallUrl = mkRawUrl "spago.dhall"
    packagesDhallUrl = mkRawUrl "packages.dhall"

    bowerfileCache = "bowerfile__" <> un RawPackageName name <> "__" <> un RawVersion tag

    parseBowerfile body = case BowerFile.parse body of
      Left err -> do
        let printedErr = BowerFile.printBowerFileParseError err
        log $ "Unable to parse bowerfile: " <> printedErr <> " Malformed body: " <> body <> "."
        throwError $ MalformedBowerJson { error: printedErr, contents: body }
      Right bowerfile -> pure bowerfile

  Process.withCache bowerfileCache Nothing do
    liftAff (Http.get ResponseFormat.string bowerfileUrl) >>= case _ of
      -- TODO: We should retry requests if they fail, because likely GitHub
      -- rate-limited us. Or at least we should collect the errors and print them
      -- out because it's quite possible the Bowerfile actually does work and
      -- we could have more granular errors to avoid excluding those packages.
      Left err -> do
        log $ "Unable to retrieve bowerfile. Bad request: " <> Http.printError err
        throwError MissingBowerfile
      Right { body, status }
        | status == StatusCode 200 ->
            parseBowerfile body

        | status == StatusCode 404 -> do
            log $ Array.intercalate "\n"
              [ "Unable to retrieve bowerfile because none exists (404 error)."
              , "Attempting to use spago.dhall file for package: " <> un RawPackageName name
              ]

            spagoDhall <- liftAff (Http.get ResponseFormat.string spagoDhallUrl) >>= case _ of
              Right res | res.status == StatusCode 200 -> pure res.body
              _ -> do
                log "No spago.dhall file! Could not create a bower.json file."
                throwError MissingBowerfile

            packagesDhall <- liftAff (Http.get ResponseFormat.string packagesDhallUrl) >>= case _ of
              Right res | res.status == StatusCode 200 -> pure res.body
              _ -> do
                log "No packages.dhall file! Could not create a bower.json file."
                throwError MissingBowerfile

            let
              fixupDhall match val src = fromMaybe src do
                Alternative.guard $ not String.contains (String.Pattern match) src
                let lines = String.split (String.Pattern "\n") src
                ix <- Array.elemIndex "}" lines
                updated <- Array.insertAt ix val lines
                pure $ String.joinWith "\n" updated

            tmp <- liftEffect Tmp.mkTmpDir

            let
              write path = liftAff <<< FS.writeTextFile UTF8 (tmp <> "/" <> path)
              fixMetadataVersionPath = "spago-fix-metadata-version.dhall"

              -- If a Spago file is missing 'repository' or 'license' fields then
              -- it will fail to generate a Bower file. However, we already
              -- know the repository for the package, and we can insert an
              -- empty license for the sake of generating the file. The package
              -- will fail later with `MissingLicense`.
              fixupSpagoDhall = fixupRepo <<< fixupLicense
                where
                fixupRepo = fixupDhall "repository =" (", repository = \"" <> repoStr <> "\"")
                fixupLicense = fixupDhall "license =" ", license = \"\""

            write "spago.dhall" (fixupSpagoDhall spagoDhall)
            write "packages.dhall" packagesDhall
            write fixMetadataVersionPath "./spago.dhall with metadata.version = \"v0.14.0\""

            let
              generateFile = liftEffect $ map (lmap Aff.message) do
                _ <- ChildProcess.execSync "git init && git add -A && git commit -m 'yeesh'" (ChildProcess.defaultExecSyncOptions { cwd = Just tmp })
                let bumpCommand = fold [ "spago -x ", fixMetadataVersionPath, " bump-version minor --no-dry-run" ]
                try $ ChildProcess.execSync bumpCommand (ChildProcess.defaultExecSyncOptions { cwd = Just tmp })

            generated <- generateFile
            bowerContents <- liftAff $ try $ FS.readTextFile UTF8 (tmp <> "/" <> "bower.json")

            case bowerContents, generated of
              Left _, Left err -> do
                log "Could not generate bower.json file:"
                log err
                throwError MissingBowerfile
              Left _, Right _ -> do
                log "Could not generate bower.json file:"
                throwError MissingBowerfile
              Right contents, _ -> do
                log $ "Successfully fell back to spago file!"
                log contents
                parseBowerfile contents

        | otherwise -> do
            log $ "Unable to retrieve bowerfile. Bad status: " <> body
            throwError $ BadStatus $ un StatusCode status

-- | Verify that the dependencies listed in the bower.json files are all
-- | contained within the registry.
selfContainedDependencies :: Set RawPackageName -> BowerFile -> ExceptT ImportError Aff Unit
selfContainedDependencies registry (BowerFile { dependencies, devDependencies }) = do
  let allDeps = Object.keys $ dependencies <> devDependencies
  outsideDeps <- for allDeps \packageName -> do
    name <- cleanPackageName $ RawPackageName packageName
    pure $ if Set.member name registry then Nothing else Just name
  for_ (NEA.fromArray $ Array.catMaybes outsideDeps) \outside ->
    throwError $ NonRegistryDependencies $ coerce outside

-- | Convert a package from Bower to a Manifest.
-- This function is written a bit awkwardly because we want to collect validation
-- errors that occur rather than just throw the first one.
toManifest
  :: PackageName
  -> Repo
  -> SemVer
  -> BowerFile
  -> ExceptT (NonEmptyArray ManifestError) Aff Manifest
toManifest package repository version (BowerFile bowerfile) = do
  let
    mkError :: forall a. ManifestError -> Either (NonEmptyArray ManifestError) a
    mkError = Left <<< NEA.singleton

    eitherLicense = do
      let
        rewrite = case _ of
          [ "Apache 2" ] -> [ "Apache-2.0" ]
          [ "Apache-2" ] -> [ "Apache-2.0" ]
          [ "Apache 2.0" ] -> [ "Apache-2.0" ]
          [ "BSD" ] -> [ "BSD-3-Clause" ]
          [ "BSD3" ] -> [ "BSD-3-Clause" ]
          [ "BSD-3" ] -> [ "BSD-3-Clause" ]
          [ "3-Clause BSD" ] -> [ "BSD-3-Clause" ]
          other -> other

      case bowerfile.license of
        Nothing -> mkError MissingLicense
        Just licenses -> do
          let
            parsed = map SPDX.parse $ rewrite $ NEA.toArray licenses
            { fail, success } = partitionEithers parsed

          case fail, success of
            [], [] -> mkError MissingLicense
            [], _ -> Right $ SPDX.joinWith SPDX.Or success
            _, _ -> mkError $ BadLicense fail

    eitherTargets = do
      let
        -- We trim out packages that don't begin with `purescript-`, as these
        -- are JavaScript dependencies being specified in the Bowerfile.
        filterNames = catMaybes <<< map \(Tuple packageName versionRange) ->
          case String.take 11 packageName of
            "purescript-" -> Just $ Tuple (String.drop 11 packageName) versionRange
            _ -> Nothing

        parsePairs = map \(Tuple packageName versionRange) -> case PackageName.parse packageName of
          Left _ -> Left packageName
          Right name -> Right (Tuple name versionRange)

        normalizeDeps deps = do
          let { fail, success } = partitionEithers $ parsePairs $ filterNames deps
          case NEA.fromArray fail of
            Nothing -> pure success
            Just err -> mkError $ InvalidDependencyNames err

        checkDepPair (Tuple packageName versionStr) =
          case SemVer.parseRange versionStr of
            Nothing -> Left { dependency: packageName, failedBounds: versionStr }
            Just range -> Right $ Tuple (PackageName.print packageName) range

      normalizedDeps <- normalizeDeps $ Object.toUnfoldable bowerfile.dependencies
      normalizedDevDeps <- normalizeDeps $ Object.toUnfoldable bowerfile.devDependencies

      let
        readDeps = map checkDepPair >>> partitionEithers >>> \{ fail, success } ->
          case NEA.fromArray fail of
            Nothing ->
              Right success
            Just err ->
              mkError $ BadDependencyVersions err

        eitherDeps = readDeps normalizedDeps
        eitherDevDeps = readDeps normalizedDevDeps

      case eitherDeps, eitherDevDeps of
        Left e1, Left e2 -> Left (e1 <> e2)
        Left e, Right _ -> Left e
        Right _, Left e -> Left e
        Right deps, Right devDeps -> Right $ Object.fromFoldable $ Array.catMaybes
          [ Just $ Tuple "lib"
              { sources: [ "src/**/*.purs" ]
              , dependencies: Object.fromFoldable deps
              }
          , if (Array.null devDeps) then Nothing
            else Just $ Tuple "test"
              { sources: [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies: Object.fromFoldable (deps <> devDeps)
              }
          ]

    errs = do
      let
        toMaybeErrors :: forall e a. Either e a -> Maybe e
        toMaybeErrors = preview _Left

      map NEA.concat $ NEA.fromArray $ Array.catMaybes
        [ toMaybeErrors eitherLicense
        , toMaybeErrors eitherTargets
        ]

  case errs of
    Nothing -> do
      -- Technically this shouldn't be needed, since we've already checked these
      -- for errors, but this is just so the types all work out.
      license <- Except.except eitherLicense
      targets <- Except.except eitherTargets
      pure { name: package, license, repository, targets, version }

    Just err ->
      throwError err

-- Packages can be specified either in 'package-name' format or
-- in owner/package-name format. This function ensures we don't pick
-- up owner names as part of package names.
--
-- Example:
-- https://github.com/newlandsvalley/purescript-abc-parser/blob/1.1.2/bower.json
cleanPackageName :: forall m. Monad m => RawPackageName -> ExceptT ImportError m RawPackageName
cleanPackageName (RawPackageName name) = do
  let
    split = String.split (String.Pattern "/") <<< coerce
    strip = coerce <<< stripPureScriptPrefix

  map strip $ case split name of
    [ packageName ] -> pure packageName
    [ _owner, repo ] -> pure repo
    _ -> throwError $ MalformedPackageName name

-- | Read the list of packages in a registry file
readRegistryFile :: FilePath -> Aff (Map RawPackageName String)
readRegistryFile source = do
  registryFile <- readJsonFile ("../" <> source)
  case registryFile of
    Left err -> do
      let decodeError = "Decoding " <> source <> "failed with error:\n\n" <> Json.printJsonDecodeError err
      throwError $ Aff.error decodeError
    Right packages -> do
      let toPackagesArray = Object.toArrayWithKey \k -> Tuple (RawPackageName $ stripPureScriptPrefix k)
      pure $ Map.fromFoldable $ toPackagesArray packages
