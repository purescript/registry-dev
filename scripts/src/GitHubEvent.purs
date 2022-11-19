module Registry.Scripts.GitHubEvent where

import Registry.App.Prelude

import Control.Monad.Except as Except
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.String as String
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..), IssueNumber)
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FS.Extra
import Node.FS.Aff as FS.Aff
import Node.Process as Node.Process
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.RegistryM as RegistryM
import Registry.Operation (AuthenticatedPackageOperation(..), PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect do
    Node.Process.lookupEnv "GITHUB_EVENT_PATH"
      >>= maybe (Exception.throw "GITHUB_EVENT_PATH not defined in the environment") pure

  githubToken <- liftEffect do
    Node.Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (Exception.throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken

  readOperation eventPath >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> do
      let
        comment = Array.fold
          [ "The JSON input for this package update is malformed:\n\n"
          , "```"
          , err
          , "```\n\n"
          , "You can try again by commenting on this issue with a corrected payload."
          ]

      Except.runExceptT (GitHub.createComment octokit issue comment) >>= case _ of
        Left githubError -> throwError $ Exception.error $ GitHub.printGitHubError githubError
        Right _ -> pure unit

    DecodedOperation issue username operation -> do
      FS.Extra.ensureDirectory API.scratchDir
      cache <- Cache.useCache API.cacheDir
      packagesMetadata <- liftEffect $ Ref.new Map.empty
      RegistryM.runRegistryM (API.mkEnv octokit cache packagesMetadata issue username) do
        RegistryM.comment $ case operation of
          Left packageSetOperation -> case packageSetOperation of
            PackageSetUpdate _ ->
              "Processing package set update."
          Right packageOperation -> case packageOperation of
            Publish { name, ref } ->
              "Publishing package `" <> PackageName.print name <> "` at the ref `" <> ref <> "`."
            Authenticated { payload } -> case payload of
              Unpublish { name, version } ->
                "Unpublishing `" <> PackageName.print name <> "` at version `" <> Version.print version <> "`."
              Transfer { name } ->
                "Transferring `" <> PackageName.print name <> "`."

        API.fetchRegistry
        API.fetchRegistryIndex
        API.fillMetadataRef
        API.runOperation API.API operation

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber String (Either PackageSetOperation PackageOperation)

derive instance Eq OperationDecoding

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.Aff.readTextFile UTF8 eventPath

  GitHub.Event { issueNumber, body, username } <- case Argonaut.Parser.jsonParser fileContents >>= GitHub.decodeEvent of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      throwError $ Exception.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  let
    -- TODO: Right now we parse all operations from GitHub issues, but we should
    -- in the future only parse out package set operations. The others should be
    -- handled via a HTTP API.
    decodeOperation :: Json -> Either CA.JsonDecodeError (Either PackageSetOperation PackageOperation)
    decodeOperation json =
      map (Left <<< PackageSetUpdate) (CA.decode Operation.packageSetUpdateCodec json)
        <|> map (Right <<< Publish) (CA.decode Operation.publishCodec json)
        <|> map (Right <<< Authenticated) (CA.decode Operation.authenticatedCodec json)

  case Argonaut.Parser.jsonParser (firstObject body) of
    Left err -> do
      log "Not JSON."
      logShow { err, body }
      pure NotJson
    Right json -> case decodeOperation json of
      Left jsonError -> do
        let printedError = CA.printJsonDecodeError jsonError
        log $ "Malformed JSON:\n" <> printedError
        log $ "Received body:\n" <> body
        pure $ MalformedJson issueNumber printedError
      Right operation ->
        pure $ DecodedOperation issueNumber username operation

-- | Users may submit issues with contents wrapped in code fences, perhaps with
-- | a language specifier, trailing lines, and other issues. This rudimentary
-- | cleanup pass retrieves all contents within an opening { and closing }
-- | delimiter.
firstObject :: String -> String
firstObject input = fromMaybe input do
  before <- String.indexOf (String.Pattern "{") input
  let start = String.drop before input
  after <- String.lastIndexOf (String.Pattern "}") start
  pure (String.take (after + 1) start)
