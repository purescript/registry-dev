module Test.Registry.App.Manifest.SpagoYaml where

import Registry.App.Prelude

import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.Manifest.SpagoYaml as SpagoYaml
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
