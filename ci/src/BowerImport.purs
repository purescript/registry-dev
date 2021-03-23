module Registry.BowerImport where

import Registry.Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.DateTime (adjust) as Time
import Data.JSDate as JSDate
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Data.Time.Duration (Hours(..))
import Dhall as Dhall
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now (nowDateTime) as Time
import Foreign.Object as Foreign
import GitHub as GitHub
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Registry.PackageName as PackageName
import Registry.SPDXLicense (SPDXConjunction(..))
import Registry.SPDXLicense as SPDXLicense
import Registry.Schema (Manifest, Repo(..))
import Registry.Version as Version
import Text.Parsing.StringParser as Parser
import Web.Bower.PackageMeta (Dependencies(..))
import Web.Bower.PackageMeta as Bower


type ReleasesIndex = Map String Package

type Package = { address :: GitHub.Address, releases :: Set GitHub.Tag }


main :: Effect Unit
main = Aff.launchAff_ do
  log "Starting import from Bower.."

  -- Get the lists of packages: Bower packages and new packages
  -- Assumption: we are running in the `ci` folder or the registry repo
  bowerPackagesStr <- FS.readTextFile UTF8 "../bower-packages.json"
  newPackagesStr <- FS.readTextFile UTF8 "../new-packages.json"

  let
    parseJsonMap str =
      Json.jsonParser str
        >>= (Json.decodeJson >>> lmap Json.printJsonDecodeError)
        >>> map (Map.fromFoldableWithIndex :: Foreign.Object String -> Map String String)

  case parseJsonMap bowerPackagesStr, parseJsonMap newPackagesStr of
    Left err, _ -> error $ "Error: couldn't parse bower-packages.json, error: " <> err
    _, Left err -> error $ "Error: couldn't parse new-packages.json, error: " <> err
    Right bowerPackages, Right newPackages -> do
      -- as first thing we iterate through all the packages and fetch all the
      -- releases from GitHub, to populate an in-memory "releases index".
      -- This is necessary so that we can do the "self-containment" check later.
      -- We keep a temporary cache on disk, so that it's easier to do development
      -- without consuming the GitHub request limit.
      let (SemigroupMap allPackages) = SemigroupMap bowerPackages <> SemigroupMap newPackages
      releaseIndex <- Map.fromFoldable <$> forWithIndex allPackages \nameWithPrefix repoUrl -> do
        let name = stripPureScriptPrefix nameWithPrefix
        let address = fromRight' (\_ -> unsafeCrashWith "Unexpected Left") $ GitHub.parseRepo repoUrl
        releases <- withCache ("releases__" <> address.owner <> "__" <> address.repo) (Just $ Hours 24.0) $ do
          log $ "Fetching releases for package " <> show name
          Set.fromFoldable <$> GitHub.getReleases address
        pure $ Tuple name { releases, address }

      -- once we have the index we can go through it and write to file all
      -- the manifests that we're missing
      forWithIndex_ releaseIndex \name { address, releases } -> do
        -- we first check that we have a directory for every package.
        -- If not, we make one
        let packageFolder = "../examples/" <> name
        unlessM (FS.exists packageFolder) (FS.mkdir packageFolder)
        -- then we list all the files in that package directory - every file is a version
        manifests <- FS.readdir packageFolder
        -- are there any releases that we don't have the file for?
        for_ releases \release -> do
          let
            manifestIsMissing = isNothing $ Array.findIndex (_ == release.name <> ".json") manifests
            shouldSkip = Set.member (Tuple name release.name) toSkip
            -- TODO: we limit the package list to these three packages just to print example manifests
            examplePackage = Set.member name (Set.fromFoldable ["aff", "mysql", "prelude"])
            shouldFetch = manifestIsMissing && not shouldSkip && examplePackage

          when shouldFetch do
            -- if yes, then..
            log $ "Could not find manifest for version " <> release.name <> " of " <> show address <> ", making it..."
            -- we download the Bower file or use the cached one if available.
            -- note that we don't need to expire the cache ever here, because the
            -- tags are supposed to be immutable
            let fetchBowerfile = do
                  let url = "https://raw.githubusercontent.com/" <> address.owner <> "/" <> address.repo <> "/" <> release.name <> "/bower.json"
                  log $ "Fetching bowerfile: " <> url
                  Http.get ResponseFormat.json url >>= case _ of
                    Left err -> do
                      error $ "Got error while fetching bowerfile, you might want to add the following to the packages to skip: " <> show name <> " /\\ " <> show release.name
                      Aff.throwError $ Exception.error $ Http.printError err
                    Right { body } -> case Json.decodeJson body of
                      Left err -> Aff.throwError $ Exception.error $ Json.printJsonDecodeError err
                      Right (bowerfile :: Bower.PackageMeta) -> pure bowerfile
            bowerfile <- withCache ("bowerfile__" <> name <> "__" <> release.name) Nothing fetchBowerfile
            -- then we check if all dependencies/versions are self-contained in the registry
            if not selfContainedDependencies releaseIndex bowerfile then
               error $ Array.fold
                 [ "Dependencies for the package "
                 , show name
                 , " are not all contained in the registry, skipping."
                 ]
            else do
              -- now we should be ready to convert it
              let manifestPath = packageFolder <> "/" <> release.name <> ".json"
              let manifestStr
                    = stringifyWithIndent 2
                    $ Json.encodeJson
                    $ toManifest bowerfile release.name
                    $ GitHub { owner: address.owner, repo: address.repo, subdir: Nothing }
              -- we then conform to Dhall type. If that does works out then
              -- write it to the manifest file, otherwise print the error
              Dhall.jsonToDhallManifest manifestStr >>= case _ of
                Right _ -> do
                  FS.writeTextFile UTF8 manifestPath manifestStr
                Left result -> error result

readBowerfile :: String -> Aff (Either String Bower.PackageMeta)
readBowerfile path = do
  let fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)
  ifM (not <$> FS.exists path)
    (pure $ Left $ "Bowerfile not found at " <> path)
    do
      strResult <- FS.readTextFile UTF8 path
      pure $ fromJson strResult

-- | Convert a Bowerfile into a Registry Manifest
toManifest :: Bower.PackageMeta -> String -> Repo -> Either String Manifest
toManifest (Bower.PackageMeta bowerfile) ref address = do
  name <- lmap Parser.printParserError $ PackageName.parse $ stripPureScriptPrefix bowerfile.name
  license <- SPDXLicense.joinWith Or <$> traverse SPDXLicense.parse bowerfile.license
  version <- Version.parse ref
  pure { name, license, repository, targets, version }
  where
  subdir = Nothing
  repository = case _.url <$> bowerfile.repository of
    Nothing -> address
    Just url -> case GitHub.parseRepo url of
      Left _err -> Git { url, subdir }
      Right { repo, owner } -> GitHub { repo, owner, subdir }
  toDepPair { packageName, versionRange } = (stripPureScriptPrefix packageName) /\ versionRange
  deps = map toDepPair $ un Dependencies bowerfile.dependencies
  devDeps = map toDepPair $ un Dependencies bowerfile.devDependencies
  targets = Foreign.fromFoldable $ Array.catMaybes
    [ pure $ Tuple "lib"
        { sources: ["src/**/*.purs"]
        , dependencies: Foreign.fromFoldable deps
        }
    , guard (Array.null (un Dependencies bowerfile.devDependencies)) $ Just $ Tuple "test"
        { sources: ["src/**/*.purs", "test/**/*.purs"]
        , dependencies: Foreign.fromFoldable (deps <> devDeps)
        }
    ]


-- | Are all the dependencies PureScript packages or are there any external Bower/JS packages?
selfContainedDependencies :: ReleasesIndex -> Bower.PackageMeta -> Boolean
selfContainedDependencies packageIndex (Bower.PackageMeta { dependencies, devDependencies }) =
  let
    (Bower.Dependencies allDeps) = dependencies <> devDependencies
    isInRegistry { packageName } = case Map.lookup (stripPureScriptPrefix packageName) packageIndex of
      Nothing -> false
      Just _ -> true
  in and (map isInRegistry allDeps)

-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall a
   . Json.DecodeJson a
  => Json.EncodeJson a
  => String -> Maybe Hours -> Aff a -> Aff a
withCache path maybeDuration action = do
  let cacheFolder = ".cache"
  let objectPath = cacheFolder <> "/" <> path
  let dump = Json.encodeJson >>> stringifyWithIndent 2
  let fromJson = Json.jsonParser >=>  (lmap Json.printJsonDecodeError <<< Json.decodeJson)
  let yolo a = unsafePartial $ fromJust a
  let cacheHit = do
        exists <- FS.exists objectPath
        expired <- case exists, maybeDuration of
          _, Nothing -> pure false
          false, _ -> pure false
          true, Just duration -> do
            lastModified
              <- (yolo <<< JSDate.toDateTime <<< _.mtime <<< (\(Stats s) -> s))
              <$> FS.stat objectPath
            now <- liftEffect $ Time.nowDateTime
            let expiryTime = yolo $ Time.adjust duration lastModified
            pure (now > expiryTime)
        pure (exists && not expired)
  unlessM (FS.exists cacheFolder) (FS.mkdir cacheFolder)
  cacheHit >>= case _ of
    true -> do
      log $ "Using cache for " <> show path
      strResult <- FS.readTextFile UTF8 objectPath
      case (fromJson strResult) of
        Right res -> pure res
        -- Here we just blindly assume that we are the only ones to serialize here
        Left err -> Aff.throwError $ Exception.error err
    false -> do
      result <- action
      FS.writeTextFile UTF8 objectPath (dump result)
      pure result

toSkip :: Set (Tuple String String)
toSkip = Set.fromFoldable
  -- The following releases are missing a bower.json:
  [ "b64" /\ "v0.0.6"
  , "batteries" /\ "v0.0.0"
  , "bouzuya-http-method" /\ "v0.3.0"
  , "bouzuya-http-method" /\ "v1.0.0"
  , "bouzuya-http-status-code" /\ "v0.2.0"
  , "bouzuya-http-status-code" /\ "v1.0.0"
  , "bq" /\ "v0.1.0"
  , "bq" /\ "v0.1.1"
  , "concur-core" /\ "v0.3.9"
  , "concur-core" /\ "v0.4.0"
  , "concur-react" /\ "v0.3.9"
  , "concur-react" /\ "v0.4.0"
  , "const" /\ "0.0.1"
  , "dodo-printer" /\ "v1.0.0"
  , "dodo-printer" /\ "v1.0.1"
  , "dodo-printer" /\ "v1.0.2"
  , "dodo-printer" /\ "v1.0.3"
  , "dodo-printer" /\ "v1.0.4"
  , "dodo-printer" /\ "v1.0.5"
  , "dodo-printer" /\ "v1.0.6"
  , "dodo-printer" /\ "v1.0.7"
  , "dodo-printer" /\ "v1.0.8"
  , "encoding" /\ "v0.0.6"
  , "endpoints-express" /\ "0.0.1"
  , "error" /\ "v1.0.0"
  , "gomtang-basic" /\ "v0.0.1"
  , "halogen-formless" /\ "v1.0.0"
  , "idiomatic-node-buffer" /\ "v0.3.0"
  , "idiomatic-node-buffer" /\ "v0.3.1"
  , "idiomatic-node-buffer" /\ "v0.3.2"
  , "idiomatic-node-buffer" /\ "v0.3.3"
  , "idiomatic-node-crypto" /\ "v0.1.0"
  , "idiomatic-node-errors" /\ "v0.1.0"
  , "idiomatic-node-errors" /\ "v0.2.0"
  , "idiomatic-node-events" /\ "v0.3.0"
  , "idiomatic-node-events" /\ "v0.3.1"
  , "idiomatic-node-http" /\ "v0.1.0"
  , "idiomatic-node-http" /\ "v0.2.0"
  , "idiomatic-node-http" /\ "v0.2.1"
  , "idiomatic-node-http" /\ "v0.3.0"
  , "idiomatic-node-process" /\ "v0.1.0"
  , "idiomatic-node-process" /\ "v0.2.0"
  , "idiomatic-node-server" /\ "v0.4.0"
  , "idiomatic-node-stream" /\ "v0.4.0"
  , "idiomatic-node-stream" /\ "v0.4.1"
  , "idiomatic-node-stream" /\ "v0.5.0"
  , "idiomatic-node-stream" /\ "v0.5.1"
  , "inject" /\ "0.0.1"
  , "jarilo" /\ "v0.1.0"
  , "jarilo" /\ "v0.2.0"
  , "jarilo" /\ "v0.3.0"
  , "jwt" /\ "v0.0.7"
  , "kushiyaki" /\ "v0.0.1"
  , "metajelo-web" /\ "v1.0.0"
  , "metajelo-web" /\ "v1.0.1"
  , "minimist" /\ "v0.5.0"
  , "minimist" /\ "v0.5.1"
  , "msgpack-msgpack" /\ "v0.5.0"
  , "node-electron" /\ "v0.0.1"
  , "ocelot" /\ "v0.21.0"
  , "ocelot" /\ "v0.21.1"
  , "ocelot" /\ "v0.22.0"
  , "ocelot" /\ "v0.23.0"
  , "optlicative" /\ "v0.1.0"
  , "optlicative" /\ "v0.1.1"
  , "optlicative" /\ "v0.2.0"
  , "optlicative" /\ "v0.3.0"
  , "optlicative" /\ "v0.4.0"
  , "optlicative" /\ "v0.4.1"
  , "optlicative" /\ "v4.0.2"
  , "pg" /\ "v0.1.0"
  , "pg" /\ "v0.2.0"
  , "pg" /\ "v0.2.1"
  , "pg" /\ "v0.3.0"
  , "pux-devtool" /\ "v5.0.0"
  , "rdkafka" /\ "v0.2.0"
  , "react-stylesheet" /\ "v0.0.1"
  , "refract" /\ "v0.0.1"
  , "routing" /\ "v3.1.0"
  , "sparse-matrices" /\ "v1.0.0"
  , "sparse-matrices" /\ "v1.1.0"
  , "specular" /\ "v0.3.0"
  , "specular" /\ "v0.4.0"
  , "specular" /\ "v0.4.1"
  , "stringutils" /\ "v0.0.10"
  , "undefined" /\ "v1.0.0"
  , "yaml-next" /\ "v2.0.0"
  , "graphql-gen" /\ "v0.0.0"
  , "graphql-parser" /\ "v0.0.0"
  , "graphql-validator" /\ "v0.0.0"
  , "graphql-validator" /\ "v0.0.1"
  , "graphql-validator" /\ "v0.0.2"
  , "graphql-validator" /\ "v0.0.3"
  , "halogen-hooks-extra" /\ "v0.1.0"
  , "halogen-hooks-extra" /\ "v0.1.1"
  , "halogen-hooks-extra" /\ "v0.2.0"
  , "halogen-hooks-extra" /\ "v0.2.1"
  , "halogen-hooks-extra" /\ "v0.2.2"
  , "halogen-hooks-extra" /\ "v0.3.0"
  , "halogen-hooks-extra" /\ "v0.4.0"
  , "halogen-hooks-extra" /\ "v0.5.0"
  , "halogen-hooks-extra" /\ "v0.5.1"
  , "halogen-hooks-extra" /\ "v0.6.0"
  , "openapi" /\ "v0.0.0"
  -- The following have a malformed one:
  , "bifunctors" /\ "v0.0.5"
  , "facebook" /\ "v0.0.1"
  , "facebook" /\ "v0.1.0"
  , "facebook" /\ "v0.2.0"
  , "facebook" /\ "v0.2.1"
  , "facebook" /\ "v0.2.2"
  , "facebook" /\ "v0.2.3"
  , "facebook" /\ "v0.3.0"
  , "ide-purescript-core" /\ "v0.5.2"
  , "var" /\ "v0.0.1"
  , "tree-rose" /\ "v2.0.0"
  ]
