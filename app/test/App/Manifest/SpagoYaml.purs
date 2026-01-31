module Test.Registry.App.Manifest.SpagoYaml where

import Registry.App.Prelude

import Data.Either (isLeft)
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Manifest.SpagoYaml (SpagoRange(..))
import Registry.App.Manifest.SpagoYaml as SpagoYaml
import Registry.Range as Range
import Registry.Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Parses spago.yaml fixtures" do
    let fixturesPath = Path.concat [ "app", "fixtures", "spago-yaml" ]
    fixturePaths <- FS.Aff.readdir fixturesPath
    for_ fixturePaths \path -> do
      config <- SpagoYaml.readSpagoYaml (Path.concat [ fixturesPath, path ]) >>= case _ of
        Left err -> Aff.throwError $ Aff.error err
        Right config -> pure config
      case SpagoYaml.spagoYamlToManifest "v1.0.0" config of
        Left err -> Assert.fail $ path <> " failed: " <> err
        Right _ -> pure unit

  Spec.describe "parseSpagoRange" do
    Spec.it "parses unbounded range '*'" do
      SpagoYaml.parseSpagoRange "*" `Assert.shouldEqual` Right Unbounded

    Spec.it "parses standard registry range" do
      let range = ">=6.0.0 <7.0.0"
      case SpagoYaml.parseSpagoRange range of
        Right (Bounded r) -> Range.print r `Assert.shouldEqual` range
        _ -> Assert.fail "Expected Bounded range"

    Spec.it "parses pinned version as exact range" do
      case SpagoYaml.parseSpagoRange "6.0.1" of
        Right (Bounded r) -> Range.print r `Assert.shouldEqual` ">=6.0.1 <6.0.2"
        _ -> Assert.fail "Expected Bounded range for pinned version"

    Spec.it "fails on invalid input" do
      SpagoYaml.parseSpagoRange "not-a-version" `Assert.shouldSatisfy` isLeft
