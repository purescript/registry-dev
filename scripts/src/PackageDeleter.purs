module Registry.Scripts.PackageDeleter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Apply (lift2)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Source as Source
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Manifest (_legacyCache)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (Run)
import Run as Run
import Run.Except as Except

type Arguments =
  { inputMode :: InputMode
  , reimport :: Boolean
  , commit :: Boolean
  , upload :: Boolean
  , pullMode :: Git.PullMode
  }

data InputMode = File FilePath | Package PackageName Version

derive instance Eq InputMode

type DeletePackages = Map PackageName (Array Version)

deletePackagesCodec :: JsonCodec DeletePackages
deletePackagesCodec = Internal.Codec.packageMap (CA.array Version.codec)

parser :: ArgParser Arguments
parser = Arg.fromRecord
  { reimport:
      Arg.flag [ "--reimport" ] "Reimport packages immediately after deleting" # Arg.boolean
  , inputMode: Arg.choose "input (--file or --package)"
      [ Arg.argument [ "--file" ]
          """Delete package versions from a JSON file like: { "prelude": [ "1.0.0", "1.1.1" ] }"""
          # Arg.unformat "FILE_PATH" pure
          # map File
      , Arg.argument [ "--package" ]
          "Delete the indicated package at the given version, separated by '@'"
          # Arg.unformat "NAME@VERSION" parsePackage
          # map (uncurry Package)
      ]
  , commit: Arg.choose "commit-mode (--commit or --no-commit)"
      [ Arg.flag [ "--commit" ] "Commit changes to ./scratch/registry/metadata and ./scratch/registry-index" $> true
      , Arg.flag [ "--no-commit" ] "Do not commit changes" $> false
      ]
  , upload: Arg.choose "upload-mode (--upload or --no-upload)"
      [ Arg.flag [ "--upload" ] "Upload changes to S3 storage" $> true
      , Arg.flag [ "--no-upload" ] "Do not upload changes to S3 storage" $> false
      ]
  , pullMode:
      Arg.flag [ "--autostash" ] "Autostash when pulling, instead of requiring a clean checkout" $> Git.Autostash
        # Arg.default Git.OnlyClean
  }
  where
  parsePackage :: String -> Either String (Tuple PackageName Version)
  parsePackage input = do
    let split = String.split (String.Pattern "@") input
    case Array.length split of
      0 -> Left "Expected package@version but received nothing."
      2 -> do
        rawPackage <- note "Unexpected error" (Array.index split 0)
        package <- lmap (append ("Failed to parse package name '" <> rawPackage <> "': ")) (PackageName.parse rawPackage)
        rawVersion <- note "Unexpected error" (Array.index split 1)
        version <- lmap (append ("Failed to parse version '" <> rawVersion <> "': ")) (Version.parse rawVersion)
        pure $ Tuple package version
      _ -> Left $ "Expected package@version but received an invalid format: " <> input

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "A script for deleting registry packages."
  arguments <- case Arg.parseArgs "package-deleter" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  resourceEnv <- Env.lookupResourceEnv
  s3 <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)

  -- GitHub
  octokit <- Octokit.newOctokit token resourceEnv.githubApiUrl

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef
  legacyCacheRef <- Cache.newCacheRef

  -- Registry
  debouncer <- Registry.newDebouncer
  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { write: Registry.ReadOnly -- We commit in bulk after running everything.
      , pull: arguments.pullMode
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "package-set-deleter-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]
  log $ "Logs available at " <> logPath

  deletions <- case arguments.inputMode of
    -- --package name@version
    Package name version -> pure $ Map.singleton name [ version ]
    -- --file packagesversions.json
    File path -> liftAff (readJsonFile deletePackagesCodec path) >>= case _ of
      Left err -> Console.log err *> liftEffect (Process.exit 1)
      Right values -> pure values

  let
    interpret =
      Registry.interpret (Registry.handle registryEnv)
        >>> Storage.interpret (if arguments.upload then Storage.handleS3 { s3, cache } else Storage.handleReadOnly cache)
        >>> Source.interpret Source.handle
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Pursuit.interpret Pursuit.handlePure
        >>> Cache.interpret _legacyCache (Cache.handleMemoryFs { ref: legacyCacheRef, cache })
        >>> Comment.interpret Comment.handleLog
        >>> Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
        >>> Env.runResourceEnv resourceEnv
        >>> Run.runBaseAff'

  interpret do
    Log.info $ Array.fold
      [ "Deleting package versions:"
      , do
          let foldFn name versions = "\n  - " <> PackageName.print name <> " " <> String.joinWith ", " (map Version.print versions)
          foldMapWithIndex foldFn deletions
      ]

    forWithIndex_ deletions \name versions ->
      for_ versions \version -> do
        result <- Except.runExcept $ deleteVersion arguments name version
        let formatted = formatPackageVersion name version
        case result of
          Left err -> do
            Log.error $ "Failed to delete " <> formatted <> ": " <> err
          Right _ ->
            Log.info $ "Successfully removed " <> formatted

    -- --commit
    when arguments.commit do
      -- Then we add our commits out-of-band by manually committing the
      -- repositories. This isn't generally recommended (we should commit as
      -- part of the registry effect), but for bulk deletions it works.
      commitMetadataResult <- Git.gitCommit
        { address: registryEnv.repos.registry
        , committer: Git.pacchettibottiCommitter token
        , commit: Registry.commitKeyToPaths Registry.CommitMetadataIndex
        , message: "Remove some package versions from metadata."
        }
        (Path.concat [ registryEnv.workdir, "registry" ])
      case commitMetadataResult of
        Left error -> Log.error $ "Failed to commit metadata: " <> error
        Right _ -> pure unit

      commitManifestIndexResult <- Git.gitCommit
        { address: registryEnv.repos.manifestIndex
        , committer: Git.pacchettibottiCommitter token
        , commit: Registry.commitKeyToPaths Registry.CommitManifestIndex
        , message: "Remove some package versions from manifest index."
        }
        (Path.concat [ registryEnv.workdir, "registry-index" ])
      case commitManifestIndexResult of
        Left error -> Log.error $ "Failed to commit manifest index: " <> error
        Right _ -> pure unit

    Log.info "Finished."

deleteVersion :: forall r. Arguments -> PackageName -> Version -> Run (API.PublishEffects + r) Unit
deleteVersion arguments name version = do
  let formatted = formatPackageVersion name version
  -- --upload
  when arguments.upload do
    Except.catch Log.error do
      Log.info $ "Deleting " <> formatted <> " from S3 storage"
      Storage.delete name version
  Log.info $ "Updating metadata for " <> formatted
  Registry.readMetadata name >>= case _ of
    Nothing -> Except.throw $ "Could not update metadata for " <> formatted <> " because no existing metadata was found."
    Just (Metadata oldMetadata) -> do
      publishment <-
        case Map.lookup version oldMetadata.published, Map.lookup version oldMetadata.unpublished of
          Just _, Just _ -> Except.throw $ "Package version was both published and unpublished: " <> formatted
          Just published, Nothing -> pure (Just (Right published))
          Nothing, Just unpublished -> pure (Just (Left unpublished))
          Nothing, Nothing -> pure Nothing
      let
        newMetadata = Metadata $ oldMetadata { published = Map.delete version oldMetadata.published, unpublished = Map.delete version oldMetadata.unpublished }
      Registry.writeMetadata name newMetadata
      Registry.deleteManifest name version
      -- --reimport
      when arguments.reimport do
        case publishment of
          Nothing -> Log.error "Cannot reimport a version that was not published"
          Just (Left _) -> Log.error "Cannot reimport a version that was specifically unpublished"
          Just (Right specificPackageMetadata) -> do
            -- Obtains `newMetadata` via cache
            API.publish API.Legacy
              { location: Just oldMetadata.location
              , name: name
              , ref: specificPackageMetadata.ref
              , compiler: unsafeFromRight $ Version.parse "0.15.4"
              , resolutions: Nothing
              }
