module TestPush where

import Registry.Prelude

import Data.Array as Array
import Data.Map as Map
import Effect.Aff as Aff
import Effect.Ref as Ref
import GitHub (IssueNumber(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Registry.API (OperationDecoding(..))
import Registry.API as API
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (comment, mkEnv, runRegistryM)
import Text.Parsing.StringParser as Parser

issueNumber :: IssueNumber
issueNumber = IssueNumber 149

metadataDir :: FilePath
metadataDir = "../metadata"

metadataFile :: PackageName -> FilePath
metadataFile packageName = metadataDir <> "/" <> PackageName.print packageName <> ".json"

main :: Effect Unit
main = launchAff_ $ do
  packagesMetadata <- do
    whenM (not <$> FS.exists metadataDir) do
      FS.mkdir metadataDir
    packageList <- FS.readdir metadataDir
    packagesArray <- for packageList \rawPackageName -> do
      packageName <- case PackageName.parse rawPackageName of
        Right p -> pure p
        Left err -> Aff.throwError $ Aff.error $ Parser.printParserError err
      metadata <- API.readJsonFile $ metadataFile packageName
      pure $ packageName /\ metadata
    liftEffect $ Ref.new $ Map.fromFoldable packagesArray
  let eventPath = Just "./test/fixtures/issue_created.json"
  API.readOperation (unsafePartial fromJust eventPath) >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> runRegistryM (mkEnv packagesMetadata issue) do
      comment $ Array.fold
        [ "The JSON input for this package update is malformed:"
        , newlines 2
        , "```" <> err <> "```"
        , newlines 2
        , "You can try again by commenting on this issue with a corrected payload."
        ]

    DecodedOperation issue op ->
      runRegistryM (mkEnv packagesMetadata issue) (API.runOperation op)