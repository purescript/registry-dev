-- | Render a canonical documentation artifact to the static package and module
-- | paths used by Pursuit.
-- |
-- |   nix run .#render-docs -- docs.json output-directory
module Registry.Scripts.RenderDocs where

import Registry.App.Prelude

import Data.Array as Array
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.Docgen.Codec as Docgen.Codec
import Registry.Docgen.Docs (DocModule(..), DocPackage(..))
import Registry.Docgen.Package.Render as Render
import Registry.Foreign.FSExtra as FS.Extra
import Registry.PackageName as PackageName
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  case args of
    [ input, output ] -> do
      result <- Aff.attempt $ renderDocs input output
      case result of
        Left error -> do
          Console.error $ Aff.message error
          liftEffect $ Process.exit' 1
        Right packagePath ->
          Console.log packagePath
    _ -> do
      Console.error "Usage: render-docs <docs.json> <output-directory>"
      liftEffect $ Process.exit' 1

renderDocs :: FilePath -> FilePath -> Aff FilePath
renderDocs input output = do
  decoded <- readJsonFile Docgen.Codec.docPackage input
  docs@(DocPackage { modules, name, version }) <- case decoded of
    Left error -> Aff.throwError $ Aff.error $ "Could not decode " <> input <> ": " <> error
    Right value -> pure value

  assetsPath <- fromMaybe "docgen/assets" <$> liftEffect (Process.lookupEnv "REGISTRY_DOCGEN_ASSETS")
  FS.Extra.copy
    { from: assetsPath
    , preserveTimestamps: true
    , to: Path.concat [ output, "static" ]
    }

  let packagePath = Path.concat [ output, "packages", PackageName.print name, Version.print version ]
  FS.Extra.ensureDirectory packagePath

  let linker = Render.defaultPackageLinker docs
  let packageTitle = PackageName.print name <> "@" <> Version.print version
  writePage (Path.concat [ packagePath, "index.html" ])
    $ Render.renderDocument Render.vendoredDocumentAssets
        { body: Render.renderContainer
            { anchorId: packageTitle
            , content: Render.renderPackageIndex linker docs
            }
        , title: packageTitle
        }

  for_ modules \module_@(DocModule { name: moduleName }) -> do
    let modulePath = Path.concat [ packagePath, "docs", unwrap moduleName ]
    FS.Extra.ensureDirectory modulePath
    writePage (Path.concat [ modulePath, "index.html" ])
      $ Render.renderDocument Render.vendoredDocumentAssets
          { body: Render.renderContainer
              { anchorId: unwrap moduleName
              , content: Render.renderModule linker docs module_
              }
          , title: unwrap moduleName <> " - " <> packageTitle
          }

  pure packagePath
  where
  writePage path = FS.Aff.writeTextFile UTF8 path <<< unwrap
