module Registry.App.CLI.Git where

import Registry.App.Prelude

import Affjax (URL)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Node.Library.Execa as Execa
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Foreign.Octokit (GitHubToken(..))
import Registry.Foreign.Octokit as Octokit
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Safe.Coerce (coerce)

-- | The result of running a Git action that can have no effect. For example,
-- | none of these will have an effect: committing an unchanged file path,
-- | fetching a repo which is up-to-date,  or pushing to a repo which is
-- | up-to-date. If you expected a change and none happened (or vice versa) then
-- | you may wish to throw an error.
data GitResult = NoChange | Changed

derive instance Eq GitResult

-- | How to sync a locally-checked-out repository.
-- |
-- | - Autostash will stash any changes you have, pull, then apply your changes.
-- | - ForceClean will remove untracked files and hard reset before pulling.
-- | - OnlyClean will abort in the presence of untracked or modified files.
data PullMode = Autostash | OnlyClean | ForceClean

derive instance Eq PullMode

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

newtype AuthOrigin = AuthOrigin String

derive instance Newtype AuthOrigin _

mkAuthOrigin :: Octokit.Address -> Committer -> AuthOrigin
mkAuthOrigin address committer = AuthOrigin $ Array.fold
  [ "https://"
  , String.toLower committer.name
  , ":"
  , un GitHubToken committer.token
  , "@github.com/"
  , address.owner
  , "/"
  , address.repo
  , ".git"
  ]

-- | Run the `git` tool via the command line.
gitCLI :: Array String -> Maybe FilePath -> Aff (Either String String)
gitCLI args cwd = do
  result <- liftAff $ _.result =<< Execa.execa "git" args (_ { cwd = cwd })
  pure case result of
    Right { stdout } -> Right (String.trim stdout)
    Left { stdout, stderr } -> Left (stdout <> stderr)

withGit :: forall r. FilePath -> Array String -> (String -> String) -> Run (AFF + EXCEPT String + r) String
withGit cwd args onError = Run.liftAff (gitCLI args (Just cwd)) >>= case _ of
  Left error -> Except.throw (onError error)
  Right stdout -> pure stdout

type GitPullArgs =
  { address :: Octokit.Address
  , pullMode :: PullMode
  }

-- | Attempt to fetch a repository to the given file path; if it already
-- | exists there, use the provided pull mode to resolve changes.
gitPull :: forall r. GitPullArgs -> FilePath -> Run (LOG + AFF + r) (Either String GitResult)
gitPull { address: { owner, repo }, pullMode } cwd = Except.runExcept do
  let exec = withGit cwd
  let inRepoErr error = " in local checkout " <> cwd <> ": " <> error
  let formatted = owner <> "/" <> repo

  Log.debug $ "Found the " <> formatted <> " repo locally at " <> cwd <> ", fetching latest changes..."

  -- First we fetch the origin to make sure we're up-to-date.
  status <- gitStatus cwd
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
        , NonEmptyArray.foldMap1 (append "\n  - ") files
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
      case pullMode of
        Autostash -> do
          Log.debug $ "Pulling " <> formatted <> " in autostash mode, which preserves local changes."
          _ <- exec [ "pull", "--rebase", "--autostash" ] \error ->
            "Failed to pull the latest changes" <> inRepoErr error
          Log.debug $ "Pulled the latest changes for " <> formatted

        OnlyClean | not isClean ->
          Except.throw do
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

type GitCommitArgs =
  { address :: Octokit.Address
  , committer :: Committer
  , commit :: Array String.Pattern
  , message :: String
  }

-- | Commit file(s) at the given commit key to the given repository.
gitCommit :: forall r. GitCommitArgs -> FilePath -> Run (LOG + AFF + r) (Either String GitResult)
gitCommit { address: { owner, repo }, committer, commit, message } cwd = Except.runExcept do
  let formatted = owner <> "/" <> repo
  let exec = withGit cwd
  let inRepoErr error = " in local checkout " <> cwd <> ": " <> error

  Log.debug $ "Committing to the " <> formatted <> " repo at " <> cwd <> " with message " <> message

  -- First we fetch the origin to make sure we're up-to-date.
  status <- gitStatus cwd
  when (status.branch `Array.notElem` [ "main", "master" ]) do
    Log.warn $ "Not on 'main' or 'master' branch in local checkout " <> cwd <> ", there may be unexpected results."

  case status.status of
    Current -> pure unit
    Ahead n -> Log.warn do
      "Ahead of origin by " <> show n <> " commits in local checkout of " <> cwd <> ", something may be wrong."
    Behind n -> Except.throw do
      "Behind of origin by " <> show n <> " commits in local checkout of " <> cwd <> ", committing would cause issues."

  -- Then we ensure the commit metadata will match the expected committer
  _ <- exec [ "config", "user.name", committer.name ] \error ->
    "Failed to configure git user name as " <> committer.name <> inRepoErr error
  _ <- exec [ "config", "user.email", "<" <> committer.email <> ">" ] \error ->
    "Failed to configure git user email as " <> committer.email <> inRepoErr error

  -- Then we attempt to stage changes associated with the given commit paths
  let commitPaths = coerce commit
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

type GitPushArgs =
  { address :: Octokit.Address
  , committer :: Committer
  }

-- | Push to the indicated repository
gitPush :: forall r. GitPushArgs -> FilePath -> Run (LOG + AFF + r) (Either String GitResult)
gitPush { address, committer } cwd = Except.runExcept do
  -- First we fetch the origin to make sure we're up-to-date.
  status <- gitStatus cwd
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
        inRepoErr :: String -> String
        inRepoErr error = " in local checkout " <> cwd <> ": " <> error

        authOrigin :: URL
        authOrigin = coerce (mkAuthOrigin address committer)

      _ <- withGit cwd [ "push", authOrigin ] \error ->
        "Failed to push to " <> address.owner <> "/" <> address.repo <> " from " <> status.branch <> inRepoErr error

      pure Changed

type ParsedStatus =
  { origin :: String
  , branch :: String
  , status :: GitStatus
  , dirty :: Maybe (NonEmptyArray String)
  }

gitStatus :: forall r. FilePath -> Run (LOG + AFF + EXCEPT String + r) ParsedStatus
gitStatus cwd = do
  let exec = withGit cwd
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
    Nothing -> Except.throw "Output of git status did not contain any text."
    Just { head, tail } -> case Parsing.runParser head gitStatusParser of
      Left error -> Except.throw $ "Could not parse git status " <> head <> inRepoErr (Parsing.parseErrorMessage error)
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
