module Test.Registry.App.Effect.DocsStorage (spec) where

import Registry.App.Prelude

import Control.Parallel (parSequence)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.Effect.DocsStorage (DOCS_STORAGE)
import Registry.App.Effect.DocsStorage as DocsStorage
import Registry.Docgen.Codec as Docgen.Codec
import Registry.Docgen.Docs (DocPackage(..), SourceArtifact(..), schemaVersion)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Tmp as Tmp
import Registry.License as License
import Registry.PackageName (PackageName)
import Registry.Test.Assert as Assert
import Registry.Test.Fixtures (defaultHash, defaultLocation)
import Registry.Test.Utils as Utils
import Registry.Version (Version)
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "supports the complete replaceable documentation lifecycle" do
    Aff.bracket Tmp.mkTmpDir FS.Extra.remove \tmp -> do
      let name = Utils.unsafePackageName "example"
      let version = Utils.unsafeVersion "1.0.0"
      let path = Path.concat [ tmp, DocsStorage.formatDocsPath name version ]

      runFs tmp (DocsStorage.exists name version) >>= (_ `Assert.shouldEqual` false)
      runFs tmp $ DocsStorage.upload docs
      runFs tmp (DocsStorage.exists name version) >>= (_ `Assert.shouldEqual` true)
      liftEffect (FS.Sync.exists path) >>= (_ `Assert.shouldEqual` true)

      stored <- runFs tmp $ DocsStorage.download name version
      CJ.encode Docgen.Codec.docPackage stored `Assert.shouldEqual` CJ.encode Docgen.Codec.docPackage docs

      duplicate <- Aff.attempt $ runFs tmp $ DocsStorage.upload docs
      case duplicate of
        Left error -> String.contains (String.Pattern "already exists") (Aff.message error) `Assert.shouldEqual` true
        Right _ -> Assert.fail "Immutable upload unexpectedly replaced documentation"

      writeJsonFile Docgen.Codec.docPackage path mismatched
      mismatch <- Aff.attempt $ runFs tmp $ DocsStorage.download name version
      case mismatch of
        Left error -> String.contains (String.Pattern "identifies itself as other@1.0.0") (Aff.message error) `Assert.shouldEqual` true
        Right _ -> Assert.fail "Download unexpectedly accepted mismatched artifact identity"

      runFs tmp $ DocsStorage.replace replacement
      replaced <- runFs tmp $ DocsStorage.download name version
      case replaced of
        DocPackage { description: Just "Replacement" } -> pure unit
        _ -> Assert.fail "Replacement documentation was not stored"

      runFs tmp $ DocsStorage.delete name version
      runFs tmp (DocsStorage.exists name version) >>= (_ `Assert.shouldEqual` false)
      runFs tmp $ DocsStorage.delete name version

  Spec.it "allows only one concurrent immutable upload" do
    Aff.bracket Tmp.mkTmpDir FS.Extra.remove \tmp -> do
      results <- parSequence
        [ Aff.attempt $ runFs tmp $ DocsStorage.upload docs
        , Aff.attempt $ runFs tmp $ DocsStorage.upload replacement
        ]
      Array.length (Array.mapMaybe hush results) `Assert.shouldEqual` 1
      let failures = Array.mapMaybe (either Just (const Nothing)) results
      case failures of
        [ error ] -> String.contains (String.Pattern "already exists") (Aff.message error) `Assert.shouldEqual` true
        _ -> Assert.fail "Expected exactly one immutable upload to fail"

      stored <- runFs tmp $ DocsStorage.download packageName packageVersion
      let encoded = CJ.encode Docgen.Codec.docPackage stored
      Array.elem encoded [ CJ.encode Docgen.Codec.docPackage docs, CJ.encode Docgen.Codec.docPackage replacement ] `Assert.shouldEqual` true

runFs
  :: forall a
   . FilePath
  -> Run (DOCS_STORAGE + EXCEPT String + AFF + EFFECT + ()) a
  -> Aff a
runFs root =
  DocsStorage.interpret (DocsStorage.handleFs root)
    >>> Except.catch (\error -> Run.liftAff $ Aff.throwError $ Aff.error error)
    >>> Run.runBaseAff'

docs :: DocPackage
docs = DocPackage
  { schemaVersion
  , compilerVersion: compilerVersion
  , sourceArtifact: SourceArtifact { bytes: 42.0, hash: defaultHash }
  , dependencies: Map.empty
  , description: Nothing
  , license: Utils.fromRight "license" $ License.parse "BSD-3-Clause"
  , location: defaultLocation
  , locationRef: Nothing
  , modules: []
  , name: packageName
  , readme: Nothing
  , resolvedDependencies: Map.empty
  , resolvedModulePackages: Map.empty
  , version: packageVersion
  }

replacement :: DocPackage
replacement = case docs of
  DocPackage package -> DocPackage package { description = Just "Replacement" }

mismatched :: DocPackage
mismatched = case docs of
  DocPackage package -> DocPackage package { name = Utils.unsafePackageName "other" }

packageName :: PackageName
packageName = Utils.unsafePackageName "example"

packageVersion :: Version
packageVersion = Utils.unsafeVersion "1.0.0"

compilerVersion :: Version
compilerVersion = Utils.unsafeVersion "0.15.15"
