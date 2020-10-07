module BowerImport where

import Prelude

import Affjax as Http
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.DateTime (adjust) as Time
import Data.Either (Either(..), fromRight)
import Data.Foldable (and)
import Data.FoldableWithIndex (forWithIndex_)
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Dhall as Dhall
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Effect.Exception as Exception
import Effect.Now (nowDateTime) as Time
import Foreign.Object as Foreign
import GitHub as GitHub
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Partial.Unsafe (unsafePartial)
import Registry.Schema (Manifest, Repo(..))
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
  let parseJsonMap str
        = Json.jsonParser str
        >>= (lmap Json.printJsonDecodeError <<< Json.decodeJson)
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
      let allPackages = bowerPackages <> newPackages
      releaseIndex <- Map.fromFoldable <$> forWithIndex allPackages \nameWithPrefix repoUrl -> do
        let name = stripPurescriptPrefix nameWithPrefix
        let address = unsafePartial $ fromRight $ GitHub.parseRepo repoUrl
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
        for_ releases \release ->
          let
            manifestIsMissing = isNothing $ Array.findIndex (_ == release.name <> ".json") manifests
            shouldSkip = Set.member (Tuple name release.name) toSkip
            -- TODO: we limit the package list to these three packages just to print example manifests
            examplePackage = Set.member name (Set.fromFoldable ["aff", "mysql", "prelude"])
            shouldFetch = manifestIsMissing && not shouldSkip && examplePackage
          in when shouldFetch do
            -- if yes, then..
            log $ "Could not find manifest for version " <> release.name <> " of " <> show address <> ", making it.."
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
                    Right { body } -> case (Json.decodeJson body) of
                      Left err -> Aff.throwError $ Exception.error $ Json.printJsonDecodeError err
                      Right (bowerfile :: Bower.PackageMeta) -> pure bowerfile
            bowerfile <- withCache ("bowerfile__" <> name <> "__" <> release.name) Nothing fetchBowerfile
            -- then we check if all dependencies/versions are self-contained in the registry
            if (not $ selfContainedDependencies releaseIndex bowerfile)
            then error $ "Dependencies for the package " <> show name <> " are not all contained in the registry, skipping."
            else do
              -- now we should be ready to convert it
              let manifestPath = packageFolder <> "/" <> release.name <> ".json"
              let manifestStr = stringifyWithIndent 2 $ Json.encodeJson $ toManifest bowerfile release.name address
              -- we then conform to Dhall type. If that does works out then
              -- write it to the manifest file, otherwise print the error
              Dhall.jsonToDhall manifestStr >>= case _ of
                Right _ -> do
                  FS.writeTextFile UTF8 manifestPath manifestStr
                Left result -> error result


-- | Convert a Bowerfile into a Registry Manifest
toManifest :: Bower.PackageMeta -> String -> GitHub.Address -> Manifest
toManifest (Bower.PackageMeta bowerfile) version address
  = { name, license, repository, targets }
  where
    subdir = Nothing
    name = stripPurescriptPrefix bowerfile.name
    license = String.joinWith " OR " bowerfile.license
    repository = case _.url <$> bowerfile.repository of
      Nothing -> GitHub { repo: address.repo, owner: address.owner, version, subdir }
      Just url -> case GitHub.parseRepo url of
        Left _err -> Git { url, version, subdir }
        Right { repo, owner } -> GitHub { repo, owner, version, subdir }
    toDepPair { packageName, versionRange } = Tuple packageName versionRange
    deps = map toDepPair $ unwrap bowerfile.dependencies
    devDeps = map toDepPair $ unwrap bowerfile.devDependencies
    targets = Foreign.fromFoldable $
      [ Tuple "lib"
          { sources: ["src/**/*.purs"]
          , dependencies: Foreign.fromFoldable deps
          }
      ] <> if Array.null (unwrap bowerfile.devDependencies)
           then []
           else [ Tuple "test"
                    { sources: ["src/**/*.purs", "test/**/*.purs"]
                    , dependencies: Foreign.fromFoldable (deps <> devDeps)
                    }
                ]


-- | Are all the dependencies PureScript packages or are there any external Bower/JS packages?
selfContainedDependencies :: ReleasesIndex -> Bower.PackageMeta -> Boolean
selfContainedDependencies packageIndex (Bower.PackageMeta { dependencies, devDependencies }) =
  let
    (Bower.Dependencies allDeps) = dependencies <> devDependencies
    isInRegistry { packageName } = case Map.lookup (stripPurescriptPrefix packageName) packageIndex of
      Nothing -> false
      Just _ -> true
  in and (map isInRegistry allDeps)


stripPurescriptPrefix :: String -> String
stripPurescriptPrefix name
  = fromMaybe name
  $ String.stripPrefix (String.Pattern "purescript-") name



-- | Optionally-expirable cache: when passing a Duration then we'll consider
-- | the object expired if its lifetime is past the duration.
-- | Otherwise, this will behave like a write-only cache.
withCache
  :: forall a
  .  Json.DecodeJson a
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
