module Registry.App.Monad.Git where

import Registry.App.Prelude

import Affjax (URL)
import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty as NonEmptyArry
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Map as Map
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Time.Duration (Minutes(..))
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.ChildProcess as ChildProcess
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
import Registry.Constants as Constants
import Registry.Foreign.Octokit (Address, GitHubToken(..))
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version
import Safe.Coerce (coerce)
import Sunde as Sunde

-- | The result of running a Git action that can have no effect. For example,
-- | none of these will have an effect: committing an unchanged file path,
-- | fetching a repo which is up-to-date,  or pushing to a repo which is
-- | up-to-date. If you expected a change and none happened (or vice versa) then
-- | you may wish to throw an error.
data GitResult = NoChange | Changed

derive instance Eq GitResult

-- | A legend for repositories that can be fetched and committed to.
data RepoKey
  = RegistryRepo
  | ManifestIndexRepo
  | LegacyPackageSetsRepo

-- | A legend for values that can be committed. We know where each kind of value
-- | ought to exist, so we can create a correct path for any given type ourselves.
data CommitKey
  = CommitManifestEntry PackageName
  | CommitMetadataEntry PackageName
  | CommitManifestIndex
  | CommitMetadataIndex
  | CommitPackageSet Version
  | CommitLegacyRegistry
  | CommitLegacyPackageSets (Array FilePath)

class Monad m <= MonadGit m where
  getAddress :: RepoKey -> m Address
  getPath :: RepoKey -> m FilePath
  pull :: RepoKey -> m (Either String GitResult)
  commit :: CommitKey -> String -> m (Either String GitResult)
  push :: RepoKey -> m (Either String GitResult)
  tag :: RepoKey -> String -> m (Either String GitResult)
  pushTags :: RepoKey -> m (Either String GitResult)

instance MonadGit m => MonadGit (ExceptT e m) where
  getAddress = lift <<< getAddress
  getPath = lift <<< getPath
  pull = lift <<< pull
  commit key = lift <<< commit key
  push = lift <<< push
  tag key = lift <<< tag key
  pushTags = lift <<< pushTags

-- | Write a file to the repository associated with the commit key, given a
-- | callback that takes the file path of the repository on disk, writes the
-- | file(s), and returns a commit message which is used to commit to the
-- | repository. The result is pushed upstream.
writeCommitPush :: forall m. MonadGit m => CommitKey -> (FilePath -> m (Maybe String)) -> m (Either String GitResult)
writeCommitPush commitKey write = do
  let repoKey = commitKeyToRepoKey commitKey
  pull repoKey >>= case _ of
    Left error -> pure (Left error)
    Right _ -> do
      repoPath <- getPath repoKey
      write repoPath >>= case _ of
        Nothing -> pure $ Left $ "Failed to write file(s) to " <> repoPath
        Just message -> commit commitKey message >>= case _ of
          Left error -> pure (Left error)
          Right _ -> push repoKey

-- | Tag the repository with the given tags and push the result upstream.
tagAndPush :: forall m. MonadGit m => RepoKey -> Array String -> m (Either String GitResult)
tagAndPush key refs = do
  results <- traverse (tag key) refs
  let partition = partitionEithers results
  case Array.uncons partition.fail of
    Nothing | Array.any (_ == Changed) partition.success -> pushTags key
    Nothing -> pure (Right NoChange)
    Just { head } -> pure (Left head)

-- | How to sync a locally-checked-out repository.
-- |
-- | - Autostash will stash any changes you have, pull, then apply your changes.
-- | - ForceClean will remove untracked files and hard reset before pulling.
-- | - OnlyClean will abort in the presence of untracked or modified files.
data PullMode = Autostash | OnlyClean | ForceClean

derive instance Eq PullMode

data WriteMode = ReadOnly | CommitAs Committer

derive instance Eq WriteMode

type GitEnv =
  { repos :: Repos
  , workdir :: FilePath
  , pull :: PullMode
  , write :: WriteMode
  , debouncer :: Debouncer
  }

type GIT r = (git :: GitEnv | r)

type Debouncer = Ref (Map FilePath DateTime)

newDebouncer :: forall m. MonadEffect m => m Debouncer
newDebouncer = liftEffect $ Ref.new Map.empty

type Repos =
  { registry :: Address
  , manifestIndex :: Address
  , legacyPackageSets :: Address
  }

-- | The default repos to use with the Git effect
defaultRepos :: Repos
defaultRepos =
  { registry: Constants.registry
  , manifestIndex: Constants.manifestIndex
  , legacyPackageSets: Legacy.PackageSet.legacyPackageSetsRepo
  }

-- | An account to use as the git committer
type Committer =
  { email :: String
  , name :: String
  , token :: GitHubToken
  }

pacchettibottiCommitter :: GitHubToken -> Committer
pacchettibottiCommitter token =
  { name: "pacchettibotti"
  , email: pacchettibottiEmail
  , token
  }

handleGetAddress :: forall m r. MonadAsk { | GIT + r } m => RepoKey -> m Address
handleGetAddress key = do
  env <- asks _.git
  pure $ case key of
    RegistryRepo -> env.repos.registry
    ManifestIndexRepo -> env.repos.manifestIndex
    LegacyPackageSetsRepo -> env.repos.legacyPackageSets

handleGetPath :: forall m r. MonadAsk { | GIT + r } m => RepoKey -> m FilePath
handleGetPath key = do
  env <- asks _.git
  pure $ case key of
    RegistryRepo -> Path.concat [ env.workdir, "registry" ]
    ManifestIndexRepo -> Path.concat [ env.workdir, "registry-index" ]
    LegacyPackageSetsRepo -> Path.concat [ env.workdir, "package-sets" ]

handlePull
  :: forall m r
   . MonadAsk { | GIT + r } m
  => MonadLog m
  => MonadAff m
  => RepoKey
  -> m (Either String GitResult)
handlePull key = do
  path <- handleGetPath key
  address <- handleGetAddress key
  let formatted = address.owner <> "/" <> address.repo

  env <- asks _.git
  now <- nowUTC
  debouncers <- liftEffect $ Ref.read env.debouncer

  case Map.lookup path debouncers of
    -- We will be behind the upstream by at most this amount of time.
    Just prev | DateTime.diff now prev <= Minutes 1.0 -> pure $ Right NoChange
    -- If we didn't debounce, then we should fetch the upstream.
    _ -> do
      liftEffect $ Ref.modify_ (Map.insert path now) env.debouncer
      Log.debug $ "Fetching repo at path " <> path
      -- First, we need to verify whether we should clone or pull.
      liftAff (Aff.attempt (FS.Aff.stat path)) >>= case _ of
        -- When the repository doesn't exist at the given file path we can go
        -- straight to a clone.
        Left _ -> do
          let url = "https://github.com/" <> formatted <> ".git"
          Log.debug $ "Didn't find " <> formatted <> " locally, cloning..."
          Except.runExceptT (gitCLI [ "clone", url, path ] Nothing) >>= case _ of
            Left err -> do
              Log.error $ "Failed to git clone repo " <> url <> " due to a git error: " <> err
              pure $ Left $ "Could not read the repository at " <> formatted
            Right _ ->
              pure $ Right Changed

        -- If it does, then we should pull.
        Right _ -> Except.runExceptT do
          Log.debug $ "Found " <> formatted <> " at path " <> path <> ", pulling latest."
          let runGit args onError = Except.withExceptT onError (gitCLI args (Just path))
          let inRepoErr error = " in local checkout of " <> formatted <> " at path " <> path <> ": " <> error

          Log.debug $ "Found the " <> formatted <> " repo locally at " <> path <> ", fetching latest changes..."

          -- First we fetch the origin to make sure we're up-to-date.
          status <- gitStatus path
          when (status.branch `Array.notElem` [ "main", "master" ]) do
            Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> path <> ", there may be unexpected results."

          isClean <- case status.dirty of
            Nothing -> do
              Log.debug $ Array.fold
                [ "Local checkout of " <> formatted
                , " has no untracked or dirty files, it is safe to pull the latest."
                ]
              pure true
            Just files -> do
              Log.debug $ Array.fold
                [ "Some files are untracked or dirty in local checkout of " <> path <> ": "
                , NonEmptyArry.foldMap1 (append "\n  - ") files
                ]
              Log.warn $ Array.fold
                [ "Local checkout of " <> formatted
                , " has untracked or dirty files, it may not be safe to pull the latest."
                ]
              pure false

          case status.status of
            Current -> do
              Log.debug $ "Local checkout of " <> formatted <> " is up to date, not pulling."
              pure NoChange

            Ahead n -> do
              Log.warn $ Array.fold
                [ "Local checkout of " <> formatted <> " at branch " <> status.branch
                , " is ahead of " <> status.origin <> " by " <> show n <> " commits, not pulling."
                ]
              pure NoChange

            Behind n -> do
              Log.debug $ "Local checkout of " <> formatted <> " is behind " <> status.origin <> " by " <> show n <> " commits, pulling..."
              case env.pull of
                Autostash -> do
                  Log.debug $ "Pulling " <> formatted <> " in autostash mode, which preserves local changes."
                  _ <- runGit [ "pull", "--rebase", "--autostash" ] \error ->
                    "Failed to pull the latest changes" <> inRepoErr error
                  Log.debug $ "Pulled the latest changes for " <> formatted

                OnlyClean | not isClean -> Except.throwError do
                  "Not pulling changes for " <> formatted <> " because the local checkout is dirty and only-clean mode was specified."

                OnlyClean -> do
                  Log.debug $ "Pulling " <> formatted <> " in only-clean mode."
                  _ <- runGit [ "pull" ] \error ->
                    "Failed to pull the latest changes" <> inRepoErr error
                  Log.debug $ "Pulled the latest changes for " <> formatted

                ForceClean -> do
                  unless isClean do
                    Log.info $ "Cleaning local checkout of " <> formatted <> " because force-clean mode was specified."
                    -- First, we hard-reset to clear modified files
                    _ <- runGit [ "reset", "--hard" ] \error ->
                      "Failed to hard-reset" <> inRepoErr error
                    -- Second, we clean to remove untracked files
                    _ <- runGit [ "clean", "-d", "-x", "--force" ] \error ->
                      "Failed to clean untracked files" <> inRepoErr error
                    Log.debug $ "Cleaned local checkout."

                  Log.debug $ "Pulling " <> formatted <> " in force-clean mode."
                  _ <- runGit [ "pull" ] \error ->
                    "Failed to pull the latest changes" <> inRepoErr error
                  Log.debug $ "Pulled the latest changes for " <> formatted
              pure Changed

handleCommit
  :: forall m r
   . MonadAsk { | GIT + r } m
  => MonadLog m
  => MonadAff m
  => CommitKey
  -> String
  -> m (Either String GitResult)
handleCommit key message = do
  let repoKey = commitKeyToRepoKey key
  address <- handleGetAddress repoKey
  let formatted = address.owner <> "/" <> address.repo
  path <- handleGetPath repoKey
  env <- asks _.git
  case env.write of
    ReadOnly -> do
      Log.info $ "Skipping commit to repo " <> formatted <> " because write mode is 'ReadOnly'."
      pure $ Right NoChange
    CommitAs committer -> Except.runExceptT do
      let runGit args onError = Except.withExceptT onError (gitCLI args (Just path))
      let inRepoErr error = " in local checkout " <> path <> ": " <> error
      Log.debug $ "Committing to the " <> formatted <> " repo at " <> path <> " with message " <> message

      -- First we fetch the origin to make sure we're up-to-date.
      status <- gitStatus path
      when (status.branch `Array.notElem` [ "main", "master" ]) do
        Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> path <> ", there may be unexpected results."

      case status.status of
        Current -> pure unit
        Ahead n -> Log.warn do
          "Ahead of origin by " <> show n <> " commits in local checkout of " <> path <> ", something may be wrong."
        Behind n -> Except.throwError do
          "Behind of origin by " <> show n <> " commits in local checkout of " <> path <> ", committing would cause issues."

      -- Then we ensure the commit metadata will match the expected committer
      _ <- runGit [ "config", "user.name", committer.name ] \error ->
        "Failed to configure git user name as " <> committer.name <> inRepoErr error
      _ <- runGit [ "config", "user.email", "<" <> committer.email <> ">" ] \error ->
        "Failed to configure git user email as " <> committer.email <> inRepoErr error

      -- Then we attempt to stage changes associated with the given commit paths
      let commitPaths = coerce (commitRelativePaths key)
      _ <- runGit (Array.cons "add" commitPaths) \error ->
        "Failed to add path(s) " <> String.joinWith ", " commitPaths <> inRepoErr error

      -- Git will error if we try to commit without any changes actually staged,
      -- so the below command lists file paths (--name-only) that have changed
      -- between the index and current HEAD (--cached), only including files that
      -- have been added or modified (--diff-filter=AM).
      staged <- runGit [ "diff", "--name-only", "--cached", "--diff-filter=AM" ] \error ->
        "Failed to check whether any changes are staged " <> inRepoErr error

      -- If there are no staged files, then we have nothing to commit.
      if String.null staged then do
        Log.warn $ "Not committing paths " <> String.joinWith ", " commitPaths <> " in " <> path <> " because no matching files have been modified."
        pure NoChange
      -- But if there are staged files then we can commit them and report that
      -- we indeed had changes.
      else do
        _ <- runGit [ "commit", "-m", message ] \error ->
          "Failed to commit path(s) " <> String.joinWith ", " commitPaths <> inRepoErr error
        pure Changed

handlePush
  :: forall m r
   . MonadAsk { | GIT + r } m
  => MonadLog m
  => MonadAff m
  => RepoKey
  -> m (Either String GitResult)
handlePush key = do
  path <- handleGetPath key
  address <- handleGetAddress key
  let formatted = address.owner <> "/" <> address.repo
  env <- asks _.git
  case env.write of
    ReadOnly -> do
      Log.info $ "Skipping push to repo " <> formatted <> " because write mode is 'ReadOnly'."
      pure $ Right NoChange
    CommitAs committer -> Except.runExceptT do
      -- First we fetch the origin to make sure we're up-to-date.
      status <- gitStatus path
      when (status.branch `Array.notElem` [ "main", "master" ]) do
        Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> path <> ", there may be unexpected results."

      case status.status of
        Current -> do
          Log.info $ Array.fold
            [ "Not pushing from local checkout " <> path <> " because local branch " <> status.branch
            , " is up to date with " <> status.origin
            ]
          pure NoChange
        Behind n -> do
          Log.warn $ Array.fold
            [ "Not pushing from local checkout " <> path <> " because local branch " <> status.branch
            , " is behind " <> status.origin <> " by " <> show n <> " commits."
            ]
          pure NoChange
        Ahead n -> do
          when (n > 1) do
            Log.warn $ "Ahead of main by " <> show n <> " commits (expected 1)"

          let
            runGit args onError = Except.withExceptT onError (gitCLI args (Just path))
            inRepoErr error = " in local checkout " <> path <> ": " <> error

            origin :: URL
            origin = Array.fold [ "https://", String.toLower committer.name, ":", un GitHubToken committer.token, "@github.com/", address.owner, "/", address.repo, ".git" ]

          _ <- runGit [ "push", origin ] \error ->
            "Failed to push to " <> origin <> " from " <> status.branch <> inRepoErr error

          pure Changed

handleTag
  :: forall m r
   . MonadAsk { | GIT + r } m
  => MonadLog m
  => MonadAff m
  => RepoKey
  -> String
  -> m (Either String GitResult)
handleTag key ref = do
  address <- handleGetAddress key
  path <- handleGetPath key
  env <- asks _.git
  case env.write of
    ReadOnly -> do
      Log.info $ "Skipping push to repo " <> address.owner <> "/" <> address.repo <> " because write mode is 'ReadOnly'."
      pure $ Right NoChange
    CommitAs committer -> Except.runExceptT do
      let runGit args onError = Except.withExceptT onError (gitCLI args (Just path))
      existingTags <- runGit [ "tag", "--list" ] \error ->
        "Failed to list tags in local checkout " <> path <> ": " <> error
      if Array.elem ref $ String.split (String.Pattern "\n") existingTags then do
        Log.warn $ "Tag " <> ref <> " already exists."
        pure NoChange
      else do
        _ <- runGit [ "config", "user.name", committer.name ] \error ->
          "Failed to configure git user name as " <> committer.name <> " in " <> path <> ": " <> error
        _ <- runGit [ "config", "user.email", "<" <> committer.email <> ">" ] \error ->
          "Failed to configure git user email as " <> committer.email <> " in " <> path <> ": " <> error
        _ <- runGit [ "tag", ref ] \error ->
          "Failed to create new tag " <> ref <> " in local checkout " <> path <> ": " <> error
        pure Changed

handlePushTags
  :: forall m r
   . MonadAsk { | GIT + r } m
  => MonadLog m
  => MonadAff m
  => RepoKey
  -> m (Either String GitResult)
handlePushTags key = do
  address <- handleGetAddress key
  path <- handleGetPath key
  let formatted = address.owner <> "/" <> address.repo
  env <- asks _.git
  case env.write of
    ReadOnly -> do
      Log.info $ "Skipping push to repo " <> formatted <> " because write mode is 'ReadOnly'."
      pure $ Right NoChange
    CommitAs committer -> Except.runExceptT do
      let runGit args onError = Except.withExceptT onError (gitCLI args (Just path))
      let origin = authOrigin address committer
      output <- runGit [ "push", "--tags", origin ] \error ->
        "Failed to push tags in local checkout " <> path <> ": " <> error
      case String.contains (String.Pattern "Everything up-to-date") output of
        true -> pure NoChange
        _ -> pure Changed

-- | The result of a 'git status' command, which can be used to determine whether
-- | we are up-to-date with the origin.
data GitStatus = Current | Behind Int | Ahead Int

type StatusReport =
  { origin :: String
  , branch :: String
  , status :: GitStatus
  , dirty :: Maybe (NonEmptyArray String)
  }

gitStatus :: forall m. MonadAff m => FilePath -> ExceptT String m StatusReport
gitStatus path = do
  let runGit args onError = Except.withExceptT onError (gitCLI args (Just path))
  let inRepoErr error = " in local checkout " <> path <> ": " <> error

  -- First we fetch the origin to make sure we're up-to-date.
  _ <- runGit [ "fetch", "origin" ] \error ->
    "Failed to fetch origin " <> inRepoErr error

  -- Then we check the local status, which will return as its first line of
  -- output a status string like the below:
  --
  -- ## master...origin/master [ahead 3]
  statusOutput <- runGit [ "status", "--short", "--branch", "--porcelain" ] \error ->
    "Failed to check status " <> inRepoErr error

  case Array.uncons (String.split (String.Pattern "\n") statusOutput) of
    Nothing -> Except.throwError "Output of git status did not contain any text."
    Just { head, tail } -> case Parsing.runParser head gitStatusParser of
      Left error -> Except.throwError $ "Could not parse git status " <> head <> inRepoErr (Parsing.parseErrorMessage error)
      Right { origin, branch, status } -> pure { origin, branch, status, dirty: NonEmptyArray.fromArray tail }

-- | The 'git status' command in short mode, with branches displayed, returns
-- | one of the following three formats as the first line of output:
-- |
-- | ## master...origin/master
-- | ## master...origin/master [ahead 3]
-- | ## master...origin/master [behind 2]
-- |
-- | ...where the first means we are current, and the other two mean we are
-- | ahead or behind.
gitStatusParser :: Parsing.Parser String { origin :: String, branch :: String, status :: GitStatus }
gitStatusParser = do
  _ <- Parsing.String.string "##"
  _ <- Parsing.String.Basic.whiteSpace
  branchChars <- Parsing.Combinators.manyTill Parsing.String.anyChar (Parsing.String.string "...")
  let branch = CodeUnits.fromCharArray (Array.fromFoldable branchChars)
  -- Git branches cannot contain the ' ' or '[' characters, so we can safely
  -- scan ahead for them to determine whether there have been changes:
  -- https://git-scm.com/docs/git-check-ref-format
  originChars <- Parsing.Combinators.manyTill Parsing.String.anyChar (Parsing.String.char ' ' $> unit <|> Parsing.String.eof)
  let origin = CodeUnits.fromCharArray (Array.fromFoldable originChars)
  status <- Parsing.String.eof $> Current <|> do
    _ <- Parsing.String.char '['
    type_ <- Parsing.String.string "ahead" <|> Parsing.String.string "behind"
    _ <- Parsing.String.Basic.whiteSpace
    count <- Parsing.String.Basic.intDecimal
    _ <- Parsing.String.char ']'
    case type_ of
      "ahead" -> pure (Ahead count)
      "behind" -> pure (Behind count)
      _ -> Parsing.fail "Expected 'ahead' or 'behind'"
  pure { origin, branch, status }

-- | Get the pattern representing the paths that should be committed for each
-- | commit key, relative to the root of the repository. Suitable to be passed
-- | to a git 'add' command executed in the checkout.
commitRelativePaths :: CommitKey -> Array String.Pattern
commitRelativePaths = coerce <<< case _ of
  CommitManifestEntry name ->
    [ ManifestIndex.packageEntryFilePath name ]
  CommitMetadataEntry name ->
    [ Path.concat [ Constants.metadataDirectory, PackageName.print name <> ".json" ] ]
  CommitManifestIndex ->
    [ "." ]
  CommitMetadataIndex ->
    [ Constants.metadataDirectory <> Path.sep <> "*.json" ]
  CommitPackageSet version ->
    [ Path.concat [ Constants.packageSetsDirectory, Version.print version <> ".json" ] ]
  CommitLegacyRegistry ->
    [ "bower-packages.json", "new-packages.json" ]
  CommitLegacyPackageSets paths ->
    paths

commitKeyToRepoKey :: CommitKey -> RepoKey
commitKeyToRepoKey = case _ of
  CommitManifestEntry _ -> ManifestIndexRepo
  CommitMetadataEntry _ -> RegistryRepo
  CommitManifestIndex -> ManifestIndexRepo
  CommitMetadataIndex -> RegistryRepo
  CommitPackageSet _ -> RegistryRepo
  CommitLegacyRegistry -> RegistryRepo
  CommitLegacyPackageSets _ -> LegacyPackageSetsRepo

-- | Run the `git` tool via the command line. Suitable for implementing Aff-
-- | based interpreters.
gitCLI :: forall m. MonadAff m => Array String -> Maybe FilePath -> ExceptT String m String
gitCLI args cwd = Except.ExceptT do
  result <- liftAff $ Sunde.spawn { cmd: "git", args, stdin: Nothing } (ChildProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  pure $ case result.exit of
    ChildProcess.Normally 0 -> Right stdout
    _ -> Left (stdout <> stderr)

authOrigin :: Address -> Committer -> URL
authOrigin upstream committer = Array.fold
  [ "https://"
  , String.toLower committer.name
  , ":"
  , un GitHubToken committer.token
  , "@github.com/"
  , upstream.owner
  , "/"
  , upstream.repo
  , ".git"
  ]
