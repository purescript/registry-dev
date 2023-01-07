module Test.Registry.Main (main) where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Registry.App.Main as Main
import Registry.Foreign.Octokit (IssueNumber(..))
import Registry.Operation (PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Test.Assert as Assert
import Test.Registry.App.API (spec) as API
import Test.Registry.App.Auth as Auth
import Test.Registry.App.CLI.Licensee as Test.CLI.Licensee
import Test.Registry.App.CLI.Purs as Test.CLI.Purs
import Test.Registry.App.CLI.Tar as Test.CLI.Tar
import Test.Registry.App.Effect.PackageSets as Test.Effect.PackageSets
import Test.Registry.App.Legacy.LenientRange as Test.Legacy.LenientRange
import Test.Registry.App.Legacy.LenientVersion as Test.Legacy.LenientVersion
import Test.Registry.App.Legacy.Manifest as Test.Legacy.Manifest
import Test.Registry.App.Legacy.PackageSet as Test.Legacy.PackageSet
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Test.Utils as Utils

main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [ consoleReporter ] do
  Spec.describe "Registry.App.CLI" do
    Spec.describe "Licensee" Test.CLI.Licensee.spec
    Spec.describe "Tar" Test.CLI.Tar.spec
    Spec.describe "Purs" Test.CLI.Purs.spec

  Spec.describe "Registry.App.Effect" do
    Test.Effect.PackageSets.spec

  Spec.describe "Registry.App.Legacy" do
    Spec.describe "Lenient Version" Test.Legacy.LenientVersion.spec
    Spec.describe "Lenient Range" Test.Legacy.LenientRange.spec
    Spec.describe "Legacy Manifest" Test.Legacy.Manifest.spec
    Spec.describe "Legacy Package Set" Test.Legacy.PackageSet.spec

  Spec.describe "Registry.Auth" do
    Auth.spec

  Spec.describe "Registry.API" do
    API.spec

  Spec.describe "Registry.Main" do
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

    res <- Main.readOperation "test/_fixtures/update_issue_comment.json"
    res `Assert.shouldEqual` Main.DecodedOperation issueNumber username (Right operation)

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

    res <- Main.readOperation "test/_fixtures/addition_issue_created.json"
    res `Assert.shouldEqual` Main.DecodedOperation issueNumber username (Right operation)

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

    res <- Main.readOperation "test/_fixtures/package-set-update_issue_created.json"
    res `Assert.shouldEqual` Main.DecodedOperation issueNumber username (Left operation)

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

    parseJson (Main.firstObject rawOperation) `Assert.shouldEqual` (Right operation)

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
