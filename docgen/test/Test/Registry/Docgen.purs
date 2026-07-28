module Test.Registry.Docgen (main) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import JSON as JSON
import JSON.Object as JObject
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import PureScript.CST (PartialModule(..), RecoveredParserResult(..), parsePartialModule)
import PureScript.CST.Types as CST
import Registry.Docgen.Codec as Docgen.Codec
import Registry.Docgen.Commonmark (renderMarkdownHTML)
import Registry.Docgen.Convert as Convert
import Registry.Docgen.Docs (DataConstructorName(..), DocChildDeclaration(..), DocChildDeclarationInfo(..), DocConstraint(..), DocDeclaration(..), DocDeclarationInfo(..), DocModule(..), DocPackage(..), DocReexport(..), DocType(..), ForallBinding(..), Ident(..), ModuleName(..), Qualified(..), RawRange(..), Readme(..), SourceArtifact(..), SourcePos(..), SourceSpan(..), TypeName(..), TypeVar(..), ValueName(..), schemaVersion)
import Registry.Docgen.Generate (GenerationError(..))
import Registry.Docgen.Generate as Generate
import Registry.Docgen.HTML as H
import Registry.Docgen.Legacy.Docs as L
import Registry.Docgen.Legacy.JSON as Legacy.JSON
import Registry.Docgen.Package.Render (defaultPackageLinker, htmlCodeRenderer, renderDeclarationInfo)
import Registry.Docgen.Reexports (ReexportError(..), modulesWithReexports, printReexportError)
import Registry.License as License
import Registry.Location (Location(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version (Version)
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Spec.describe "registry-docgen" do
    Spec.it "round trips schema 1 and rejects unsupported schema versions" do
      let encoded = Codec.encode Docgen.Codec.docPackage package
      let decoded = runExcept $ Codec.decode Docgen.Codec.docPackage encoded
      case decoded of
        Left err -> Assert.fail $ show err
        Right value -> Codec.encode Docgen.Codec.docPackage value `Assert.shouldEqual` encoded
      let unsupported = Utils.fromRight "unsupported JSON" $ JSON.parse $ String.replace (String.Pattern "\"schemaVersion\":1") (String.Replacement "\"schemaVersion\":2") $ JSON.print encoded
      case runExcept (Codec.decode Docgen.Codec.docPackage unsupported) of
        Left err -> shouldContainString (show err) "Unsupported documentation schema version 2"
        Right _ -> Assert.fail "schema version 2 unexpectedly decoded"
      let unsupportedOnly = Utils.fromRight "unsupported-only JSON" $ JSON.parse "{\"schemaVersion\":2}"
      case runExcept (Codec.decode Docgen.Codec.docPackage unsupportedOnly) of
        Left err -> shouldContainString (show err) "Unsupported documentation schema version 2"
        Right _ -> Assert.fail "incomplete schema version 2 unexpectedly decoded"
      case JSON.toJObject encoded >>= JObject.lookup "dependencies" of
        Just dependencies -> case JSON.toJObject dependencies of
          Just _ -> pure unit
          Nothing -> Assert.fail "Encoded dependencies were not a JSON object"
        Nothing -> Assert.fail "Encoded package omitted dependencies"

    Spec.it "round trips and converts visible kind applications" do
      let kindApp = TypeKindApp { function: TypeIdent (Ident "f"), arg: TypeIdent (Ident "k") }
      let encoded = Codec.encode Docgen.Codec.docType kindApp
      case runExcept (Codec.decode Docgen.Codec.docType encoded) of
        Right (TypeKindApp { function: TypeIdent (Ident "f"), arg: TypeIdent (Ident "k") }) -> pure unit
        _ -> Assert.fail "Canonical kind application did not round trip"
      case Convert.fromLegacyModule Map.empty (legacyModuleWithDeclaration "A" (legacyValue "x" (L.KindApp (L.TypeVar "f") (L.TypeVar "k")) Nothing)) of
        DocModule { declarations: [ DocDeclaration { info: DeclValue { signature: TypeKindApp { function: TypeIdent (Ident "f"), arg: TypeIdent (Ident "k") } } } ] } -> pure unit
        _ -> Assert.fail "Legacy kind application was not preserved"

    Spec.it "converts historical Pursuit JSON using explicit package-relative source paths" do
      let json = Utils.fromRight "Failed to parse historical fixture" $ JSON.parse historicalPackage
      let legacy = Utils.fromRight "Failed to decode historical fixture" $ Legacy.JSON.decodeDocPackage json
      let readme = Readme { content: "# Undefined", extension: Just "md" }
      let converted = Utils.fromRight "Failed to convert historical fixture" $ Convert.fromLegacyPackage (Map.singleton (ModuleName "Undefined") "custom/Undefined.purs") sourceArtifactFixture (Just readme) legacy
      goldenSource <- FS.Aff.readTextFile UTF8 "docgen/fixtures/undefined-1.0.2.json"
      let golden = Utils.fromRight "Failed to parse canonical golden fixture" $ JSON.parse goldenSource
      Codec.encode Docgen.Codec.docPackage converted `Assert.shouldEqual` golden
      let (DocPackage convertedPackage) = converted
      convertedPackage.name `Assert.shouldEqual` packageName "undefined"
      convertedPackage.compilerVersion `Assert.shouldEqual` version "0.12.2"
      case convertedPackage.modules of
        [ DocModule { declarations: [ DocDeclaration { sourceSpan: Just (SourceSpan span) } ] } ] ->
          span.path `Assert.shouldEqual` "custom/Undefined.purs"
        _ ->
          Assert.fail "Expected one converted declaration with a source span"
      case Convert.fromLegacyPackage Map.empty sourceArtifactFixture (Just readme) legacy of
        Left err -> shouldContainString err "Missing package-relative source path for module Undefined"
        Right _ -> Assert.fail "Conversion unexpectedly accepted a missing module source path"

    Spec.it "attributes legacy reexports to their origin module and source path" do
      let
        legacy = L.DocModule
          { comments: Nothing
          , declarations: [ legacyValue "own" (L.TypeVar "a") (Just sourceSpan) ]
          , name: ModuleName "A"
          , reExports: [ L.ReExport { moduleName: L.InPackage { item: ModuleName "B", package: Nothing }, declarations: [ legacyValue "other" (L.TypeVar "b") (Just sourceSpan) ] } ]
          }
      case Convert.fromLegacyModule (Map.singleton (ModuleName "A") "A.purs") legacy of
        DocModule
          { declarations: [ DocDeclaration { sourceSpan: Just (SourceSpan { path: "A.purs" }) } ]
          , reexports: [ DocReexport { declarations: [ DocDeclaration { info: DeclValue { name: Qualified { moduleName: ModuleName "B" } }, sourceSpan: Nothing } ] } ]
          } -> pure unit
        _ -> Assert.fail "Legacy reexport inherited owner provenance"

    Spec.describe "canonical package generation" do
      Spec.it "is deterministic across input order and records module ownership" do
        let generated = Utils.fromRight "Generation failed" $ Generate.generatePackage generationInput
        let reversed = Utils.fromRight "Reversed generation failed" $ Generate.generatePackage generationInput { modules = Array.reverse generationInput.modules }
        Codec.encode Docgen.Codec.docPackage generated `Assert.shouldEqual` Codec.encode Docgen.Codec.docPackage reversed
        case generated of
          DocPackage { modules: [ DocModule { name: ModuleName "A", reexports: [ DocReexport { moduleName: ModuleName "B", declarations: [ DocDeclaration { sourceSpan: Just (SourceSpan { path: "src/B.purs" }) } ] } ] } ], resolvedModulePackages } -> do
            Map.lookup (ModuleName "A") resolvedModulePackages `Assert.shouldEqual` Just (packageName "example")
            Map.lookup (ModuleName "B") resolvedModulePackages `Assert.shouldEqual` Just dependency
          _ -> Assert.fail "Generated package did not preserve deterministic ownership, reexports, and source paths"

      Spec.it "rejects duplicate modules and non-package-relative paths" do
        let duplicate = generationInput { modules = generationInput.modules <> [ Array.head generationInput.modules # Utils.fromJust "missing module" ] }
        case Generate.generatePackage duplicate of
          Left (DuplicateModule (ModuleName "B")) -> pure unit
          _ -> Assert.fail "Generation unexpectedly accepted a duplicate module"
        let absolute = generationInput { modules = map (\moduleInput -> if moduleInput.package == generationInput.name then moduleInput { sourcePath = "/tmp/A.purs" } else moduleInput) generationInput.modules }
        case Generate.generatePackage absolute of
          Left (InvalidSourcePath (ModuleName "A") "/tmp/A.purs") -> pure unit
          _ -> Assert.fail "Generation unexpectedly accepted an absolute source path"

    Spec.describe "re-export resolution" do
      Spec.it "resolves direct re-exports without dropping modules" do
        let docs = [ emptyModule "A", moduleWithValues "B" [ "foo" ] ]
        let resolved = resolveReexports docs [ directReexport "A" "B", plainModule "B" ]
        Array.length resolved `Assert.shouldEqual` 2
        reexportSummary "A" resolved `Assert.shouldEqual` [ { moduleName: "B", declarations: 1 } ]

      Spec.it "honors selective imports" do
        let docs = [ emptyModule "A", moduleWithValues "B" [ "foo", "bar" ] ]
        let resolved = resolveReexports docs [ selectiveReexport "A" "B" "foo", plainModule "B" ]
        reexportSummary "A" resolved `Assert.shouldEqual` [ { moduleName: "B", declarations: 1 } ]

      Spec.it "honors hiding imports" do
        let docs = [ emptyModule "A", moduleWithValues "B" [ "foo", "bar" ] ]
        let resolved = resolveReexports docs [ hidingReexport "A" "B" "foo", plainModule "B" ]
        reexportValueNames "A" resolved `Assert.shouldEqual` [ "bar" ]

      Spec.it "preserves constructor restrictions across repeated imports" do
        let docs = [ emptyModule "A", moduleWithData "B" ]
        let resolved = resolveReexports docs [ partialConstructorReexport, plainModule "B" ]
        reexportConstructorNames "A" resolved `Assert.shouldEqual` [ "C1", "C3" ]

      Spec.it "promotes selectively reexported class members with class evidence" do
        let docs = [ emptyModule "A", moduleWithClass "B", emptyModule "Prim" ]
        let resolved = resolveReexports docs [ selectiveReexport "A" "B" "m", plainModule "B", plainModule "Prim" ]
        case reexportDeclarations "A" resolved of
          [ DocDeclaration { info: DeclValue { signature: TypeForall { bindings, body: TypeConstrained { constraint: DocConstraint constraint } } } } ] -> do
            case NonEmptyArray.toArray bindings, constraint.name, constraint.args of
              [ ForallBinding { isVisible: false, name: Ident "a", signature: Just TypeWildcard } ], Qualified { moduleName: ModuleName "B", name: TypeName "C" }, [ TypeIdent (Ident "a") ] -> pure unit
              _, _, _ -> Assert.fail "Promoted class evidence had incorrect variables"
          _ -> Assert.fail "Class member was not promoted with quantified class evidence"

      Spec.it "resolves module aliases to the owning module" do
        let docs = [ emptyModule "A", moduleWithValues "B" [ "foo" ] ]
        let resolved = resolveReexports docs [ aliasedReexport "A" "B" "Alias", plainModule "B" ]
        reexportSummary "A" resolved `Assert.shouldEqual` [ { moduleName: "B", declarations: 1 } ]

      Spec.it "resolves transitive re-exports to original modules" do
        let docs = [ emptyModule "A", emptyModule "B", moduleWithValues "C" [ "foo" ] ]
        let resolved = resolveReexports docs [ directReexport "A" "B", directReexport "B" "C", plainModule "C" ]
        reexportSummary "A" resolved `Assert.shouldEqual` [ { moduleName: "C", declarations: 1 } ]
        reexportSummary "B" resolved `Assert.shouldEqual` [ { moduleName: "C", declarations: 1 } ]

      Spec.it "deduplicates diamond re-exports by origin and namespace" do
        let docs = [ emptyModule "A", emptyModule "B", emptyModule "C", moduleWithValues "D" [ "foo" ] ]
        let sources = [ twoReexports "A" "B" "C", directReexport "B" "D", directReexport "C" "D", plainModule "D" ]
        reexportSummary "A" (resolveReexports docs sources) `Assert.shouldEqual` [ { moduleName: "D", declarations: 1 } ]

      Spec.it "reports missing documentation targets" do
        case modulesWithReexports [ emptyModule "A" ] [ parseHeader $ directReexport "A" "Missing" ] of
          Left err@(MissingDocsTarget (ModuleName "A") (ModuleName "Missing")) ->
            shouldContainString (printReexportError err) "A reexports missing documentation module Missing"
          Left err -> Assert.fail $ "Unexpected re-export error: " <> printReexportError err
          Right _ -> Assert.fail "Missing documentation target unexpectedly resolved"

      Spec.it "reports docs modules with no source header" do
        case modulesWithReexports [ emptyModule "A", emptyModule "B" ] [ parseHeader $ plainModule "A" ] of
          Left err@(MissingSourceHeaders names) -> do
            names `Assert.shouldEqual` Utils.unsafeNonEmptyArray [ ModuleName "B" ]
            shouldContainString (printReexportError err) "Missing parsed source headers"
          Left err -> Assert.fail $ "Unexpected re-export error: " <> printReexportError err
          Right _ -> Assert.fail "Missing source header unexpectedly resolved"

      Spec.it "reports the complete cycle chain" do
        let sources = map parseHeader [ directReexport "A" "B", directReexport "B" "A" ]
        case modulesWithReexports [ emptyModule "A", emptyModule "B" ] sources of
          Left err@(ReexportCycle names) -> do
            names `Assert.shouldEqual` Utils.unsafeNonEmptyArray [ ModuleName "A", ModuleName "B", ModuleName "A" ]
            printReexportError err `Assert.shouldEqual` "Reexport cycle: A -> B -> A"
          Left err -> Assert.fail $ "Unexpected re-export error: " <> printReexportError err
          Right _ -> Assert.fail "Re-export cycle unexpectedly resolved"

    Spec.it "uses dependency package and exact owning version in links" do
      let linker = defaultPackageLinker package
      linker.getPackageLink dependency `Assert.shouldEqual`
        { href: "/packages/dependency/2.3.4", title: "dependency@2.3.4" }
      linker.getSourceLink { moduleName: ModuleName "Dependency", sourceSpan } `Assert.shouldEqual`
        { href: "https://www.purescript.org/registry-package-viewer/#/dependency/2.3.4/custom/Dependency.purs#4-8"
        , title: "dependency@2.3.4/custom/Dependency.purs"
        }

    Spec.it "renders foreign data, escapes HTML, and sanitizes unsafe Markdown" do
      let code = htmlCodeRenderer (defaultPackageLinker package) (ModuleName "Main")
      let rendered = unwrap (renderDeclarationInfo code foreignData).content
      shouldContainString rendered "foreign import data"
      shouldContainString rendered "Foreign"
      unwrap (H.text "<unsafe & text>") `Assert.shouldEqual` "&lt;unsafe &amp; text&gt;"
      let markdown = unwrap (renderMarkdownHTML { safe: true } "<script>x</script> [bad](javascript:alert(1)) **<ok>**")
      String.contains (String.Pattern "<script>") markdown `Assert.shouldEqual` false
      String.contains (String.Pattern "javascript:") markdown `Assert.shouldEqual` false

shouldContainString :: String -> String -> Aff Unit
shouldContainString actual expected = String.contains (String.Pattern expected) actual `Assert.shouldEqual` true

resolveReexports :: Array DocModule -> Array String -> Array DocModule
resolveReexports docs sources = case modulesWithReexports docs (map parseHeader sources) of
  Left err -> Utils.fromRight (printReexportError err) $ Left unit
  Right result -> result

parseHeader :: String -> CST.ModuleHeader Void
parseHeader source = case parsePartialModule source of
  ParseSucceeded (PartialModule { header }) -> header
  ParseSucceededWithErrors _ _ -> Utils.fromRight "Source fixture parsed with errors" $ Left unit
  ParseFailed _ -> Utils.fromRight "Source fixture failed to parse" $ Left unit

plainModule :: String -> String
plainModule name = "module " <> name <> " where"

directReexport :: String -> String -> String
directReexport owner target = String.joinWith "\n"
  [ "module " <> owner <> " (module " <> target <> ") where"
  , "import " <> target
  ]

selectiveReexport :: String -> String -> String -> String
selectiveReexport owner target value = String.joinWith "\n"
  [ "module " <> owner <> " (module " <> target <> ") where"
  , "import " <> target <> " (" <> value <> ")"
  ]

hidingReexport :: String -> String -> String -> String
hidingReexport owner target value = String.joinWith "\n"
  [ "module " <> owner <> " (module " <> target <> ") where"
  , "import " <> target <> " hiding (" <> value <> ")"
  ]

partialConstructorReexport :: String
partialConstructorReexport = String.joinWith "\n"
  [ "module A (module B) where"
  , "import B hiding (T(C1, C2))"
  , "import B (T(C1))"
  ]

twoReexports :: String -> String -> String -> String
twoReexports owner first second = String.joinWith "\n"
  [ "module " <> owner <> " (module " <> first <> ", module " <> second <> ") where"
  , "import " <> first
  , "import " <> second
  ]

aliasedReexport :: String -> String -> String -> String
aliasedReexport owner target alias = String.joinWith "\n"
  [ "module " <> owner <> " (module " <> alias <> ") where"
  , "import " <> target <> " as " <> alias
  ]

emptyModule :: String -> DocModule
emptyModule name = DocModule
  { comments: Nothing
  , declarations: []
  , name: ModuleName name
  , reexports: []
  }

moduleWithValues :: String -> Array String -> DocModule
moduleWithValues name values = DocModule
  { comments: Nothing
  , declarations: map (valueDeclaration name) values
  , name: ModuleName name
  , reexports: []
  }

moduleWithData :: String -> DocModule
moduleWithData name = DocModule
  { comments: Nothing
  , declarations:
      [ DocDeclaration
          { children: map constructorDeclaration [ "C1", "C2", "C3" ]
          , comments: Nothing
          , info: DeclData
              { isNewtype: false
              , name: Qualified { moduleName: ModuleName name, name: TypeName "T" }
              , roles: []
              , signature: Nothing
              , vars: []
              }
          , sourceSpan: Nothing
          }
      ]
  , name: ModuleName name
  , reexports: []
  }
  where
  constructorDeclaration constructor = DocChildDeclaration
    { comments: Nothing
    , info: ChildDeclConstructor
        { args: []
        , name: Qualified { moduleName: ModuleName name, name: DataConstructorName constructor }
        }
    , sourceSpan: Nothing
    }

moduleWithClass :: String -> DocModule
moduleWithClass name = DocModule
  { comments: Nothing
  , declarations:
      [ DocDeclaration
          { children:
              [ DocChildDeclaration
                  { comments: Nothing
                  , info: ChildDeclTypeClassMember
                      { name: Qualified { moduleName: ModuleName name, name: ValueName "m" }
                      , signature: TypeIdent (Ident "a")
                      }
                  , sourceSpan: Nothing
                  }
              ]
          , comments: Nothing
          , info: DeclTypeClass
              { funDeps: []
              , name: Qualified { moduleName: ModuleName name, name: TypeName "C" }
              , signature: Nothing
              , superClasses: []
              , vars: [ TypeVar { ident: Ident "a", signature: Just TypeWildcard } ]
              }
          , sourceSpan: Nothing
          }
      ]
  , name: ModuleName name
  , reexports: []
  }

valueDeclaration :: String -> String -> DocDeclaration
valueDeclaration moduleName name = DocDeclaration
  { children: []
  , comments: Nothing
  , info: DeclValue
      { name: Qualified { moduleName: ModuleName moduleName, name: ValueName name }
      , signature: TypeWildcard
      }
  , sourceSpan: Nothing
  }

reexportSummary :: String -> Array DocModule -> Array { moduleName :: String, declarations :: Int }
reexportSummary name modules = case Array.find (\(DocModule doc) -> doc.name == ModuleName name) modules of
  Nothing -> []
  Just (DocModule { reexports }) -> map
    ( \(DocReexport reexport) ->
        { moduleName: unwrap reexport.moduleName
        , declarations: Array.length reexport.declarations
        }
    )
    reexports

reexportDeclarations :: String -> Array DocModule -> Array DocDeclaration
reexportDeclarations name modules = case Array.find (\(DocModule doc) -> doc.name == ModuleName name) modules of
  Just (DocModule { reexports: [ DocReexport { declarations } ] }) -> declarations
  _ -> []

reexportValueNames :: String -> Array DocModule -> Array String
reexportValueNames name = reexportDeclarations name >>> Array.mapMaybe case _ of
  DocDeclaration { info: DeclValue { name: Qualified { name: ValueName value } } } -> Just value
  _ -> Nothing

reexportConstructorNames :: String -> Array DocModule -> Array String
reexportConstructorNames name = reexportDeclarations name >>> Array.concatMap case _ of
  DocDeclaration { children, info: DeclData _ } -> Array.mapMaybe constructorName children
  _ -> []
  where
  constructorName = case _ of
    DocChildDeclaration { info: ChildDeclConstructor { name: Qualified { name: DataConstructorName constructor } } } -> Just constructor
    _ -> Nothing

legacyModuleWithDeclaration :: String -> L.Declaration -> L.DocModule
legacyModuleWithDeclaration name declaration = L.DocModule
  { comments: Nothing
  , declarations: [ declaration ]
  , name: ModuleName name
  , reExports: []
  }

legacyValue :: String -> L.DocType -> Maybe SourceSpan -> L.Declaration
legacyValue title signature span = L.Declaration
  { children: []
  , comments: Nothing
  , info: L.ValueDeclaration signature
  , kindInfo: Nothing
  , sourceSpan: span
  , title
  }

generationInput :: Generate.PackageInput
generationInput =
  { compilerVersion: version "0.15.15"
  , dependencies: Map.singleton dependency (RawRange ">=2.0.0 <3.0.0")
  , description: Just "Example"
  , license: Utils.fromRight "license" $ License.parse "BSD-3-Clause"
  , location: Git { url: "https://example.com/repo.git", subdir: Just "packages/example" }
  , locationRef: Nothing
  , modules:
      [ { docs: legacyModuleWithDeclaration "B" (legacyValue "value" (L.TypeVar "a") (Just sourceSpan))
        , package: dependency
        , source: parseHeader $ plainModule "B"
        , sourcePath: "src/B.purs"
        }
      , { docs: L.DocModule { comments: Nothing, declarations: [], name: ModuleName "A", reExports: [] }
        , package: packageName "example"
        , source: parseHeader $ directReexport "A" "B"
        , sourcePath: "custom/A.purs"
        }
      ]
  , name: packageName "example"
  , readme: Just $ Readme { content: "# Example", extension: Just "md" }
  , resolvedDependencies: Map.singleton dependency (version "2.3.4")
  , sourceArtifact: sourceArtifactFixture
  , version: version "1.0.0"
  }

package :: DocPackage
package = DocPackage
  { schemaVersion
  , compilerVersion: version "0.15.15"
  , sourceArtifact: sourceArtifactFixture
  , dependencies: Map.singleton dependency (RawRange ">=2.0.0 <3.0.0")
  , description: Nothing
  , license: Utils.fromRight "license" $ License.parse "BSD-3-Clause"
  , location: Git { url: "https://example.com/repo.git", subdir: Nothing }
  , locationRef: Nothing
  , modules: [ DocModule { comments: Nothing, declarations: [], name: ModuleName "Main", reexports: [] } ]
  , name: packageName "example"
  , readme: Nothing
  , resolvedDependencies: Map.singleton dependency (version "2.3.4")
  , resolvedModulePackages: Map.singleton (ModuleName "Dependency") dependency
  , version: version "1.0.0"
  }

dependency :: PackageName
dependency = packageName "dependency"

packageName :: String -> PackageName
packageName = Utils.fromRight "package name" <<< PackageName.parse

version :: String -> Version
version = Utils.fromRight "version" <<< Version.parse

sourceArtifactFixture :: SourceArtifact
sourceArtifactFixture = SourceArtifact
  { bytes: 42.0
  , hash: Utils.fromRight "hash" $ Sha256.parse "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
  }

sourceSpan :: SourceSpan
sourceSpan = SourceSpan
  { path: "custom/Dependency.purs"
  , start: SourcePos { line: 4, column: 1 }
  , end: SourcePos { line: 8, column: 1 }
  }

foreignData :: DocDeclarationInfo
foreignData = DeclForeignData
  { name: Qualified { moduleName: ModuleName "Main", name: TypeName "Foreign" }
  , roles: []
  , signature: TypeConstructor $ Qualified { moduleName: ModuleName "Prim", name: TypeName "Type" }
  }

-- A small real package artifact from purescript/pursuit-backups. The original
-- declaration span is absolute; conversion must replace it with the explicit
-- package-tarball-relative path supplied by the caller.
historicalPackage :: String
historicalPackage =
  """{"uploader":"bklaric","packageMeta":{"homepage":"https://github.com/bklaric/purescript-undefined","repository":{"url":"https://github.com/bklaric/purescript-undefined.git","type":"git"},"ignore":["**/.*","node_modules","bower_components","test","tests"],"main":[""],"authors":[{"email":"branimir.klaric.bk@gmail.com","name":"Branimir Klarić"}],"name":"purescript-undefined","keywords":["purescript","javascript","undefined"],"license":["MIT"],"description":"Package containing the undefined value."},"tagTime":"2019-03-24T14:32:11+0000","modules":[{"reExports":[],"name":"Undefined","comments":null,"declarations":[{"children":[],"comments":null,"title":"undefined","info":{"declType":"value","type":{"annotation":[],"tag":"ForAll","contents":["anything",{"annotation":[],"tag":"TypeVar","contents":"anything"},null]}},"sourceSpan":{"start":[3,1],"name":"/home/bklaric/Documents/PureScript/purescript-undefined/src/Undefined.purs","end":[3,46]}}]}],"resolvedDependencies":{},"version":"1.0.2","github":["bklaric","purescript-undefined"],"versionTag":"v1.0.2","moduleMap":{},"compilerVersion":"0.12.2"}"""
