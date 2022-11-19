module Test.Scripts.GitHubEvent (spec) where

import Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Bifunctor (bimap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Foreign.GitHub (IssueNumber(..))
import Node.Path (FilePath)
import Node.Path as Path
import Registry.Location (Location(..))
import Registry.Operation (PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.Scripts.GitHubEvent as API
import Registry.Scripts.GitHubEvent as GitHubEvent
import Test.Spec as Spec
import Test.Utils as Utils
import Test.Utils.Assert as Assert

fixturePath :: FilePath
fixturePath = Path.concat [ "test", "_fixtures", "events" ]

spec :: Spec.Spec Unit
spec = do
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

    res <- GitHubEvent.readOperation $ Path.concat [ fixturePath, "update_issue_comment.json" ]
    res `Assert.shouldEqual` GitHubEvent.DecodedOperation issueNumber username (Right operation)

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

    res <- GitHubEvent.readOperation $ Path.concat [ fixturePath, "addition_issue_created.json" ]
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username (Right operation)

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

    res <- GitHubEvent.readOperation $ Path.concat [ fixturePath, "package-set-update_issue_created.json" ]
    res `Assert.shouldEqual` GitHubEvent.DecodedOperation issueNumber username (Left operation)

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

      parseJson = bimap CA.printJsonDecodeError Publish <<< CA.decode Operation.publishCodec <=< Argonaut.Parser.jsonParser

    parseJson (GitHubEvent.firstObject rawOperation) `Assert.shouldEqual` (Right operation)

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
