module Registry.App.Effect.Git where

import Registry.App.Prelude

import Affjax (URL)
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
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.Constants as Constants
import Registry.Foreign.Octokit (Address, GitHubToken(..))
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except as Run.Except
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

data Git a
  = Pull RepoKey (Either String GitResult -> a)
  | Commit CommitKey String (Either String GitResult -> a)
  | Push RepoKey (Either String GitResult -> a)
  | Tag RepoKey String (Either String GitResult -> a)
  | PushTags RepoKey (Either String GitResult -> a)
  | GetPath RepoKey (FilePath -> a)
  | GetAddress RepoKey (Address -> a)

derive instance Functor Git

type GIT r = (git :: Git | r)

_git :: Proxy "git"
_git = Proxy

-- | Get the repository at the given key, recording whether the pull or clone
-- | had any effect (ie. if the repo was already up-to-date).
pull :: forall r. RepoKey -> Run (GIT + r) (Either String GitResult)
pull key = Run.lift _git (Pull key identity)

-- | Commit the file(s) indicated by the commit key with a commit message.
commit :: forall r. CommitKey -> String -> Run (GIT + r) (Either String GitResult)
commit key message = Run.lift _git (Commit key message identity)

-- | Push the repository at the given key, recording whether the push had any
-- | effect (ie. if the repo was already up-to-date).
push :: forall r. RepoKey -> Run (GIT + r) (Either String GitResult)
push key = Run.lift _git (Push key identity)

-- | Tag the repository at the given key at its current commit with the tag
tag :: forall r. RepoKey -> String -> Run (GIT + r) (Either String GitResult)
tag key ref = Run.lift _git (Tag key ref identity)

-- | Push the repository tags at the given key to its upstream.
pushTags :: forall r. RepoKey -> Run (GIT + r) (Either String GitResult)
pushTags key = Run.lift _git (PushTags key identity)

-- | Get the absolute file path of the repository at the given key
getPath :: forall r. RepoKey -> Run (GIT + r) FilePath
getPath key = Run.lift _git (GetPath key identity)

-- | Get the address of the repository at the given key
getAddress :: forall r. RepoKey -> Run (GIT + r) Address
getAddress key = Run.lift _git (GetAddress key identity)

-- | Write a file to the repository associated with the commit key, given a
-- | callback that takes the file path of the repository on disk, writes the
-- | file(s), and returns a commit message which is used to commit to the
-- | repository. The result is pushed upstream.
writeCommitPush :: forall r. CommitKey -> (FilePath -> Run (GIT + r) (Maybe String)) -> Run (GIT + r) (Either String GitResult)
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
tagAndPush :: forall r. RepoKey -> Array String -> Run (GIT + r) (Either String GitResult)
tagAndPush key refs = do
  results <- traverse (tag key) refs
  let partition = partitionEithers results
  case Array.uncons partition.fail of
    Nothing | Array.any (_ == Changed) partition.success -> pushTags key
    Nothing -> pure (Right NoChange)
    Just { head } -> pure (Left head)

-- | Interpret the GIT effect given a handler, eliminating it from the effects.
runGit :: forall r a. (Git ~> Run r) -> Run (GIT + r) a -> Run r a
runGit handler = Run.interpret (Run.on _git handler Run.send)

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

-- | A handler for the Git effect which clones repos to the given file path
--
-- Note: This assumes that 'Pull' will be called before tagging, committing,
-- pushing, etc. but that's not necessarily true. In practice we always use
-- 'writeCommitPush', so this isn't an issue, but it would be nice to ensure
-- an initial pull.
handleGitAff :: forall r a. GitEnv -> Git a -> Run (LOG + AFF + EFFECT + r) a
handleGitAff env = case _ of
  GetPath key reply -> do
    pure $ reply $ filepath key

  GetAddress key reply -> do
    pure $ reply $ address key

  Pull key reply -> do
    let
      path = filepath key

      fetchLatest = do
        Log.debug $ "Fetching repo at path " <> path
        -- First, we need to verify whether we should clone or pull.
        Run.liftAff (Aff.attempt (FS.Aff.stat path)) >>= case _ of
          -- When the repository doesn't exist at the given file path we can go
          -- straight to a clone.
          Left _ -> do
            let { owner, repo } = address key
            let formatted = owner <> "/" <> repo
            let url = "https://github.com/" <> formatted <> ".git"
            Log.debug $ "Didn't find " <> formatted <> " locally, cloning..."
            Run.liftAff (gitCLI [ "clone", url, path ] Nothing) >>= case _ of
              Left err -> do
                Log.error $ "Failed to git clone repo " <> url <> " due to a git error: " <> err
                pure $ Left $ "Could not read the repository at " <> formatted
              Right _ ->
                pure $ Right Changed

          -- If it does, then we should pull.
          Right _ -> do
            Log.debug $ "Found repo at path " <> path <> ", pulling latest."
            result <- gitPull key
            pure result

    now <- nowUTC
    debouncers <- Run.liftEffect $ Ref.read env.debouncer
    case Map.lookup path debouncers of
      -- We will be behind the upstream by at most this amount of time.
      Just prev | DateTime.diff now prev <= Minutes 1.0 ->
        pure $ reply $ Right NoChange
      -- If we didn't debounce, then we should fetch the upstream.
      _ -> do
        result <- fetchLatest
        Run.liftEffect $ Ref.modify_ (Map.insert path now) env.debouncer
        pure $ reply result

  Commit commitKey message reply -> do
    let repoKey = commitKeyToRepoKey commitKey
    let { owner, repo } = address repoKey
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping commit to repo " <> owner <> "/" <> repo <> " because write mode is 'ReadOnly'."
        pure $ reply $ Right NoChange
      CommitAs committer -> do
        result <- gitCommit committer repoKey commitKey message
        pure $ reply result

  Push repoKey reply -> do
    let { owner, repo } = address repoKey
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping push to repo " <> owner <> "/" <> repo <> " because write mode is 'ReadOnly'."
        pure $ reply $ Right NoChange
      CommitAs committer -> do
        result <- gitPush committer repoKey
        pure $ reply result

  Tag repoKey ref reply -> do
    let { owner, repo } = address repoKey
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping push to repo " <> owner <> "/" <> repo <> " because write mode is 'ReadOnly'."
        pure $ reply $ Right NoChange
      CommitAs committer -> do
        result <- Run.Except.runExceptAt _gitException do
          let cwd = filepath repoKey
          existingTags <- execGit cwd [ "tag", "--list" ] \error ->
            "Failed to list tags in local checkout " <> cwd <> ": " <> error
          if Array.elem ref $ String.split (String.Pattern "\n") existingTags then do
            Log.warn $ "Tag " <> ref <> " already exists."
            pure NoChange
          else do
            _ <- execGit cwd [ "config", "user.name", committer.name ] \error ->
              "Failed to configure git user name as " <> committer.name <> " in " <> cwd <> ": " <> error
            _ <- execGit cwd [ "config", "user.email", "<" <> committer.email <> ">" ] \error ->
              "Failed to configure git user email as " <> committer.email <> " in " <> cwd <> ": " <> error
            _ <- execGit cwd [ "tag", ref ] \error ->
              "Failed to create new tag " <> ref <> " in local checkout " <> cwd <> ": " <> error
            pure Changed
        pure $ reply result

  PushTags repoKey reply -> do
    let { owner, repo } = address repoKey
    case env.write of
      ReadOnly -> do
        Log.info $ "Skipping push to repo " <> owner <> "/" <> repo <> " because write mode is 'ReadOnly'."
        pure $ reply $ Right NoChange
      CommitAs committer -> do
        result <- Run.Except.runExceptAt _gitException do
          let cwd = filepath repoKey
          let upstream = address repoKey
          let origin = authOrigin upstream committer
          output <- execGit cwd [ "push", "--tags", origin ] \error ->
            "Failed to push tags in local checkout " <> cwd <> ": " <> error
          if String.contains (String.Pattern "Everything up-to-date") output then pure NoChange else pure Changed
        pure $ reply result
  where
  address :: RepoKey -> Address
  address = case _ of
    RegistryRepo -> env.repos.registry
    ManifestIndexRepo -> env.repos.manifestIndex
    LegacyPackageSetsRepo -> env.repos.legacyPackageSets

  filepath :: RepoKey -> FilePath
  filepath = case _ of
    RegistryRepo -> registryPath
    ManifestIndexRepo -> manifestIndexPath
    LegacyPackageSetsRepo -> legacyPackageSetsPath

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

  registryPath :: FilePath
  registryPath = Path.concat [ env.workdir, "registry" ]

  manifestIndexPath :: FilePath
  manifestIndexPath = Path.concat [ env.workdir, "registry-index" ]

  legacyPackageSetsPath :: FilePath
  legacyPackageSetsPath = Path.concat [ env.workdir, "package-sets" ]

  execGit :: FilePath -> Array String -> (String -> String) -> Run _ String
  execGit cwd args onError = Run.liftAff (gitCLI args (Just cwd)) >>= case _ of
    Left error -> Run.Except.throwAt _gitException (onError error)
    Right stdout -> pure stdout

  _gitException :: Proxy "gitException"
  _gitException = Proxy

  -- | Attempt to fetch a repository to the given file path; if it already
  -- | exists there, use the provided pull mode to resolve changes.
  gitPull :: RepoKey -> Run _ (Either String GitResult)
  gitPull repoKey = Run.Except.runExceptAt _gitException do
    let { owner, repo } = address repoKey
    let cwd = filepath repoKey
    let exec = execGit cwd
    let inRepoErr error = " in local checkout " <> cwd <> ": " <> error
    let formatted = owner <> "/" <> repo

    Log.debug $ "Found the " <> formatted <> " repo locally at " <> cwd <> ", fetching latest changes..."

    -- First we fetch the origin to make sure we're up-to-date.
    status <- gitStatus repoKey
    when (status.branch `Array.notElem` [ "main", "master" ]) do
      Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> cwd <> ", there may be unexpected results."

    isClean <- case status.dirty of
      Nothing -> do
        Log.debug $ Array.fold
          [ "Local checkout of " <> formatted
          , " has no untracked or dirty files, it is safe to pull the latest."
          ]
        pure true
      Just files -> do
        Log.debug $ Array.fold
          [ "Some files are untracked or dirty in local checkout of " <> cwd <> ": "
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
            _ <- exec [ "pull", "--rebase", "--autostash" ] \error ->
              "Failed to pull the latest changes" <> inRepoErr error
            Log.debug $ "Pulled the latest changes for " <> formatted

          OnlyClean | not isClean ->
            Run.Except.throwAt _gitException do
              "Not pulling changes for " <> formatted <> " because the local checkout is dirty and only-clean mode was specified."

          OnlyClean -> do
            Log.debug $ "Pulling " <> formatted <> " in only-clean mode."
            _ <- exec [ "pull" ] \error ->
              "Failed to pull the latest changes" <> inRepoErr error
            Log.debug $ "Pulled the latest changes for " <> formatted

          ForceClean -> do
            unless isClean do
              Log.info $ "Cleaning local checkout of " <> formatted <> " because force-clean mode was specified."
              -- First, we hard-reset to clear modified files
              _ <- exec [ "reset", "--hard" ] \error ->
                "Failed to hard-reset" <> inRepoErr error
              -- Second, we clean to remove untracked files
              _ <- exec [ "clean", "-d", "-x", "--force" ] \error ->
                "Failed to clean untracked files" <> inRepoErr error
              Log.debug $ "Cleaned local checkout."

            Log.debug $ "Pulling " <> formatted <> " in force-clean mode."
            _ <- exec [ "pull" ] \error ->
              "Failed to pull the latest changes" <> inRepoErr error
            Log.debug $ "Pulled the latest changes for " <> formatted

        pure Changed

  -- | Commit file(s) at the given commit key to the given repository.
  gitCommit :: Committer -> RepoKey -> CommitKey -> String -> Run _ (Either String GitResult)
  gitCommit committer repoKey commitKey message = Run.Except.runExceptAt _gitException do
    let { owner, repo } = address repoKey
    let formatted = owner <> "/" <> repo
    let cwd = filepath repoKey
    let exec = execGit cwd
    let inRepoErr error = " in local checkout " <> cwd <> ": " <> error

    Log.debug $ "Committing to the " <> formatted <> " repo at " <> cwd <> " with message " <> message

    -- First we fetch the origin to make sure we're up-to-date.
    status <- gitStatus repoKey
    when (status.branch `Array.notElem` [ "main", "master" ]) do
      Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> cwd <> ", there may be unexpected results."

    case status.status of
      Current -> pure unit
      Ahead n -> Log.warn do
        "Ahead of origin by " <> show n <> " commits in local checkout of " <> cwd <> ", something may be wrong."
      Behind n -> Run.Except.throwAt _gitException do
        "Behind of origin by " <> show n <> " commits in local checkout of " <> cwd <> ", committing would cause issues."

    -- Then we ensure the commit metadata will match the expected committer
    _ <- exec [ "config", "user.name", committer.name ] \error ->
      "Failed to configure git user name as " <> committer.name <> inRepoErr error
    _ <- exec [ "config", "user.email", "<" <> committer.email <> ">" ] \error ->
      "Failed to configure git user email as " <> committer.email <> inRepoErr error

    -- Then we attempt to stage changes associated with the given commit paths
    let commitPaths = coerce (commitRelativePaths commitKey)
    _ <- exec (Array.cons "add" commitPaths) \error ->
      "Failed to add path(s) " <> String.joinWith ", " commitPaths <> inRepoErr error

    -- Git will error if we try to commit without any changes actually staged,
    -- so the below command lists file paths (--name-only) that have changed
    -- between the index and current HEAD (--cached), only including files that
    -- have been added or modified (--diff-filter=AM).
    staged <- exec [ "diff", "--name-only", "--cached", "--diff-filter=AM" ] \error ->
      "Failed to check whether any changes are staged " <> inRepoErr error

    -- If there are no staged files, then we have nothing to commit.
    if String.null staged then do
      Log.warn $ "Not committing paths " <> String.joinWith ", " commitPaths <> " in " <> cwd <> " because no matching files have been modified."
      pure NoChange
    -- But if there are staged files then we can commit them and report that
    -- we indeed had changes.
    else do
      _ <- exec [ "commit", "-m", message ] \error ->
        "Failed to commit path(s) " <> String.joinWith ", " commitPaths <> inRepoErr error
      pure Changed

  -- | Push to the indicated repository
  gitPush :: Committer -> RepoKey -> Run _ (Either String GitResult)
  gitPush committer repoKey = Run.Except.runExceptAt _gitException do
    let cwd = filepath repoKey

    -- First we fetch the origin to make sure we're up-to-date.
    status <- gitStatus repoKey
    when (status.branch `Array.notElem` [ "main", "master" ]) do
      Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> cwd <> ", there may be unexpected results."

    case status.status of
      Current -> do
        Log.info $ Array.fold
          [ "Not pushing from local checkout " <> cwd <> " because local branch " <> status.branch
          , " is up to date with " <> status.origin
          ]
        pure NoChange
      Behind n -> do
        Log.warn $ Array.fold
          [ "Not pushing from local checkout " <> cwd <> " because local branch " <> status.branch
          , " is behind " <> status.origin <> " by " <> show n <> " commits."
          ]
        pure NoChange
      Ahead n -> do
        when (n > 1) do
          Log.warn $ "Ahead of main by " <> show n <> " commits (expected 1)"

        let
          upstream = address repoKey
          inRepoErr error = " in local checkout " <> cwd <> ": " <> error

          origin :: URL
          origin = Array.fold
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

        _ <- execGit cwd [ "push", origin ] \error ->
          "Failed to push to " <> origin <> " from " <> status.branch <> inRepoErr error

        pure Changed

  gitStatus :: RepoKey -> Run _ { origin :: String, branch :: String, status :: GitStatus, dirty :: Maybe (NonEmptyArray String) }
  gitStatus repoKey = do
    let cwd = filepath repoKey
    let exec = execGit cwd
    let inRepoErr error = " in local checkout " <> cwd <> ": " <> error

    -- First we fetch the origin to make sure we're up-to-date.
    _ <- exec [ "fetch", "origin" ] \error ->
      "Failed to fetch origin " <> inRepoErr error

    -- Then we check the local status, which will return as its first line of
    -- output a status string like the below:
    --
    -- ## master...origin/master [ahead 3]
    statusOutput <- exec [ "status", "--short", "--branch", "--porcelain" ] \error ->
      "Failed to check status " <> inRepoErr error

    case Array.uncons (String.split (String.Pattern "\n") statusOutput) of
      Nothing -> Run.Except.throwAt _gitException "Output of git status did not contain any text."
      Just { head, tail } -> case Parsing.runParser head gitStatusParser of
        Left error -> Run.Except.throwAt _gitException $ "Could not parse git status " <> head <> inRepoErr (Parsing.parseErrorMessage error)
        Right { origin, branch, status } -> pure { origin, branch, status, dirty: NonEmptyArray.fromArray tail }

-- | The result of a 'git status' command, which can be used to determine whether
-- | we are up-to-date with the origin.
data GitStatus = Current | Behind Int | Ahead Int

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
gitCLI :: Array String -> Maybe FilePath -> Aff (Either String String)
gitCLI args cwd = do
  result <- Sunde.spawn { cmd: "git", args, stdin: Nothing } (ChildProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  pure $ case result.exit of
    ChildProcess.Normally 0 -> Right stdout
    _ -> Left (stdout <> stderr)
