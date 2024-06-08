module Test.Registry.App.GitHubIssue
  ( spec
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Codec.JSON as CJ
import Data.Map as Map
import JSON as JSON
import Node.Path as Path
import Registry.App.GitHubIssue as GitHubIssue
import Registry.Foreign.Octokit (IssueNumber(..))
import Registry.Operation (PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec =
  Spec.describe "Decodes GitHub event to Operation"
    decodeEventsToOps

decodeEventsToOps :: Spec.Spec Unit
decodeEventsToOps = do
  Spec.it "decodes an Update operation" do
    let
      issueNumber = IssueNumber 43
      username = "Codertocat"
      operation = Publish
        { name: Utils.unsafePackageName "something"
        , ref: "v1.2.3"
        , compiler: Utils.unsafeVersion "0.15.0"
        , resolutions: Just $ Map.fromFoldable [ Utils.unsafePackageName "prelude" /\ Utils.unsafeVersion "1.0.0" ]
        , location: Nothing
        }

    res <- GitHubIssue.readOperation $ Path.concat [ "app", "fixtures", "update_issue_comment.json" ]
    res `Assert.shouldEqual` GitHubIssue.DecodedOperation issueNumber username (Right operation)

  Spec.it "decodes an Addition operation" do
    let
      issueNumber = IssueNumber 149
      username = "Codertocat"
      operation = Publish
        { name: Utils.unsafePackageName "prelude"
        , ref: "v5.0.0"
        , location: Just $ GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        , compiler: Utils.unsafeVersion "0.15.0"
        , resolutions: Just $ Map.fromFoldable [ Utils.unsafePackageName "prelude" /\ Utils.unsafeVersion "1.0.0" ]
        }

    res <- GitHubIssue.readOperation $ Path.concat [ "app", "fixtures", "addition_issue_created.json" ]
    res `Assert.shouldEqual` GitHubIssue.DecodedOperation issueNumber username (Right operation)

  Spec.it "decodes a Package Set Update operation" do
    let
      issueNumber = IssueNumber 149
      username = "Codertocat"
      operation = PackageSetUpdate
        { compiler: Nothing
        , packages: Map.fromFoldable
            [ Utils.unsafePackageName "aff" /\ Just (Utils.unsafeVersion "7.0.0")
            , Utils.unsafePackageName "argonaut" /\ Nothing
            ]
        }

    res <- GitHubIssue.readOperation $ Path.concat [ "app", "fixtures", "package-set-update_issue_created.json" ]
    res `Assert.shouldEqual` GitHubIssue.DecodedOperation issueNumber username (Left operation)

  Spec.it "decodes lenient JSON" do
    let
      operation = Publish
        { name: Utils.unsafePackageName "prelude"
        , ref: "v5.0.0"
        , location: Just $ GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        , compiler: Utils.unsafeVersion "0.15.0"
        , resolutions: Nothing
        }

      rawOperation = preludeAdditionString

      parseJson = bimap CJ.DecodeError.print Publish <<< CJ.decode Operation.publishCodec <=< JSON.parse

    parseJson (GitHubIssue.firstObject rawOperation) `Assert.shouldEqual` (Right operation)

  Spec.it "returns a sensible error message when the JSON fails to parse" do
    let
      rawOperation = packageNameTooLongString

      parseJson = bimap CJ.DecodeError.print Publish <<< CJ.decode Operation.publishCodec <=< JSON.parse
    
    parseJson (GitHubIssue.firstObject rawOperation) `Assert.shouldEqual` (Left "$.name: Could not decode Publish:\n  Could not decode PackageName:\n    Package name cannot be longer than 150 characters")

preludeAdditionString :: String
preludeAdditionString =
  """
  Here's my new package!

  ```json
  {
    "name": "prelude",
    "ref": "v5.0.0",
    "location": {
      "githubOwner": "purescript",
      "githubRepo": "purescript-prelude"
    },
    "compiler": "0.15.0"
  }
  ```

  Thanks!
  """

packageNameTooLongString :: String
packageNameTooLongString =
  """
  ```
  {
    "name": "packagenamewayyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyytoolong",
    "ref": "v5.0.0",
    "location": {
      "githubOwner": "purescript",
      "githubRepo": "purescript-prelude"
    },
    "compiler": "0.15.0"
  }
  ```
  """
