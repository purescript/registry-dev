-- | Script for verifying that the registry and registry-index repos match each other and S3
module Registry.Scripts.VerifyIntegrity where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Apply (lift2)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Either (isLeft)
import Data.Foldable (class Foldable, foldMap, intercalate)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Node.Path as Path
import Node.Process as Process
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Git (GitEnv, PullMode(..), WriteMode(..))
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

data InputMode = File FilePath | Package PackageName | All

derive instance Eq InputMode

parser :: ArgParser InputMode
parser = Arg.choose "input (--file or --package)"
  [ Arg.argument [ "--file" ]
      """Verify packages from a JSON file like: [ "prelude", "console" ]"""
      # Arg.unformat "FILE_PATH" pure
      # map File
  , Arg.argument [ "--package" ]
      "Verify the indicated package"
      # Arg.unformat "NAME" PackageName.parse
      # map Package
  , Arg.flag [ "--all" ] "Verify all packages" $> All
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "A script for verifying that the registry and registry-index repos match each other and S3."
  arguments <- case Arg.parseArgs "package-verify" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  s3 <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)

  -- Git
  debouncer <- Git.newDebouncer
  let
    gitEnv :: GitEnv
    gitEnv =
      { write: ReadOnly
      , pull: Autostash
      , repos: Git.defaultRepos
      , workdir: scratchDir
      , debouncer
      }

  -- GitHub
  octokit <- Octokit.newOctokit token

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "verify-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]
  log $ "Logs available at " <> logPath

  selectedPackages <- case arguments of
    -- --package name@version
    Package name -> pure (Just [ name ])
    -- --file packagesversions.json
    File path -> liftAff (readJsonFile (CA.array PackageName.codec) path) >>= case _ of
      Left err -> Console.log err *> liftEffect (Process.exit 1)
      Right values -> pure (Just values)
    All -> pure Nothing

  let
    interpret =
      Except.catch (\error -> Run.liftEffect (Console.log error *> Process.exit 1))
        >>> Registry.interpret (Registry.handle registryCacheRef)
        >>> Storage.interpret (Storage.handleS3 { s3, cache })
        >>> Git.interpret (Git.handle gitEnv)
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
        >>> Run.runBaseAff'

  interpret do
    allMetadata <- Registry.readAllMetadata
    allManifests <- Registry.readAllManifests
    let
      packages = case selectedPackages of
        Just ps -> ps
        Nothing -> do
          Array.fromFoldable
            $ Map.keys allMetadata
            <> Map.keys (ManifestIndex.toMap allManifests)
    Log.info $ Array.fold
      [ "Verifying package versions:"
      , do
          let foldFn name = "\n  - " <> PackageName.print name
          foldMap foldFn packages
      ]

    results <- for packages \name -> do
      result <- Except.runExcept do
        published <- Storage.query name
        verifyPackage allMetadata allManifests (Set.fromFoldable published) name
      result <$ case result of
        Left err -> do
          Log.error $ "Failed to verify " <> PackageName.print name <> ": " <> err
        Right _ ->
          Log.info $ "Verified " <> PackageName.print name

    Log.info "Finished."
    when (any isLeft results) do
      liftEffect $ Process.exit 1

intercalateMap :: forall f a d. Monoid d => Foldable f => d -> (a -> d) -> f a -> d
intercalateMap s f = intercalate s <<< map f <<< Array.fromFoldable

setOf :: forall f a. Foldable f => (a -> String) -> f a -> String
setOf f vs = "{" <> intercalateMap "," f vs <> "}"

dblDiff :: forall k. Ord k => Set k -> Set k -> Maybe { left :: Set k, right :: Set k }
dblDiff a b | a == b = Nothing
dblDiff a b = Just { left: Set.difference a b, right: Set.difference b a }

printDblDiff :: forall k. (k -> String) -> { left :: String, right :: String } -> { left :: Set k, right :: Set k } -> String
printDblDiff f names { left, right } = intercalate ", " $ join
  [ if Set.isEmpty right then mempty
    else
      [ names.left <> " missing " <> setOf f right ]
  , if Set.isEmpty left then mempty
    else
      [ names.right <> " missing " <> setOf f left ]
  ]

-- | Check that the metadata (from the registry repo), manifests (from
-- | registry-index) and S3 storage all agree on what versions of the package
-- | have been published. We have read the metadata and manifests from disk
-- | above and queried S3 as well. We don't download and verify the hashes of
-- | those resources match what is in the metadata and the manifests match,
-- | but we could ...
-- |
-- | If there is a discrepancy, we will take the metadata to be the source of
-- | truth, as the registry-index is just a cache of manifests that exist in
-- | the tarballs.
verifyPackage :: forall r. Map PackageName Metadata -> ManifestIndex -> Set Version -> PackageName -> Run (EXCEPT String + LOG + r) Unit
verifyPackage allMetadata allManifests publishedS3 name = do
  let formatted = PackageName.print name
  Log.info $ "Checking versions for " <> formatted
  metadataVersions <- case Map.lookup name allMetadata of
    Nothing -> Except.throw $ "Missing metadata for " <> formatted
    Just (Metadata { published }) -> pure $ Map.keys published
  manifestVersions <- case Map.lookup name $ ManifestIndex.toMap allManifests of
    Nothing ->
      if Set.isEmpty metadataVersions then pure Set.empty
      else Except.throw $ "Missing manifests for " <> formatted
    Just manifests -> pure $ Map.keys manifests

  for_ (dblDiff metadataVersions manifestVersions) \diff -> do
    Except.throw $ printDblDiff Version.print { left: "metadata", right: "manifests" } diff
  let versions = Set.intersection metadataVersions manifestVersions
  for_ (dblDiff publishedS3 versions) \diff -> do
    Except.throw $ printDblDiff Version.print { left: "S3", right: "manifests/metadata" } diff
  pure unit
