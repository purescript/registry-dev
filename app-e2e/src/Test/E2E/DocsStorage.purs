module Test.E2E.DocsStorage (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import JSON as JSON
import Registry.App.Effect.DocsStorage (DOCS_STORAGE)
import Registry.App.Effect.DocsStorage as DocsStorage
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log as Log
import Registry.Docgen.Codec as Docgen.Codec
import Registry.Docgen.Docs (DocPackage(..), SourceArtifact(..), schemaVersion)
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
import Test.E2E.Support.Env (E2E, E2ESpec)
import Test.E2E.Support.WireMock as WireMock
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.it "supports the complete documentation lifecycle through S3" do
    runDocsStorage (DocsStorage.exists packageName packageVersion) >>= (_ `Assert.shouldEqual` false)

    runDocsStorage $ DocsStorage.upload docs
    runDocsStorage (DocsStorage.exists packageName packageVersion) >>= (_ `Assert.shouldEqual` true)

    stored <- runDocsStorage $ DocsStorage.download packageName packageVersion
    CJ.encode Docgen.Codec.docPackage stored `Assert.shouldEqual` CJ.encode Docgen.Codec.docPackage docs

    duplicate <- runDocsStorageResult $ DocsStorage.upload docs
    case duplicate of
      Left error -> String.contains (String.Pattern "already exists") error `Assert.shouldEqual` true
      Right _ -> Assert.fail "Immutable upload unexpectedly replaced documentation"

    runDocsStorage $ DocsStorage.replace replacement
    replaced <- runDocsStorage $ DocsStorage.download packageName packageVersion
    CJ.encode Docgen.Codec.docPackage replaced `Assert.shouldEqual` CJ.encode Docgen.Codec.docPackage replacement

    runDocsStorage $ DocsStorage.delete packageName packageVersion
    runDocsStorage (DocsStorage.exists packageName packageVersion) >>= (_ `Assert.shouldEqual` false)
    runDocsStorage $ DocsStorage.delete packageName packageVersion

    requests <- WireMock.getStorageRequests
    let objectRequests = WireMock.filterByUrlContaining ("/" <> DocsStorage.formatDocsPath packageName packageVersion) requests
    let putRequests = WireMock.filterByMethod "PUT" objectRequests
    let getRequests = WireMock.filterByMethod "GET" objectRequests
    let deleteRequests = WireMock.filterByMethod "DELETE" objectRequests
    Array.length putRequests `Assert.shouldEqual` 2
    Array.length getRequests `Assert.shouldEqual` 2
    Array.length deleteRequests `Assert.shouldEqual` 2
    let putBodies = map decodeBody putRequests
    Array.any (_ == CJ.encode Docgen.Codec.docPackage docs) putBodies `Assert.shouldEqual` true
    Array.any (_ == CJ.encode Docgen.Codec.docPackage replacement) putBodies `Assert.shouldEqual` true

runDocsStorage
  :: forall a
   . Run (DOCS_STORAGE + Env.RESOURCE_ENV + Log.LOG + EXCEPT String + AFF + EFFECT + ()) a
  -> E2E a
runDocsStorage operation = do
  result <- runDocsStorageResult operation
  case result of
    Left error -> liftAff $ Aff.throwError $ Aff.error error
    Right value -> pure value

runDocsStorageResult
  :: forall a
   . Run (DOCS_STORAGE + Env.RESOURCE_ENV + Log.LOG + EXCEPT String + AFF + EFFECT + ()) a
  -> E2E (Either String a)
runDocsStorageResult operation = do
  resourceEnv <- Env.lookupResourceEnv
  key <- Env.lookupRequired Env.spacesKey
  secret <- Env.lookupRequired Env.spacesSecret
  bucket <- Env.lookupWithDefault Env.docsBucket "purescript-registry-docs"
  liftAff $ operation
    # Except.runExcept
    # DocsStorage.interpret (DocsStorage.handleS3 { bucket, s3: { key, secret } })
    # Env.runResourceEnv resourceEnv
    # Log.interpret (\(Log.Log _ _ next) -> pure next)
    # Run.runBaseAff'

decodeBody :: WireMock.WireMockRequest -> JSON
decodeBody request =
  Utils.fromRight "S3 PUT request body was not JSON"
    $ JSON.parse
    $ Utils.fromJust "S3 PUT request did not contain a body" request.body

docs :: DocPackage
docs = DocPackage
  { schemaVersion
  , compilerVersion
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

packageName :: PackageName
packageName = Utils.unsafePackageName "docs-storage-test"

packageVersion :: Version
packageVersion = Utils.unsafeVersion "1.0.0"

compilerVersion :: Version
compilerVersion = Utils.unsafeVersion "0.15.15"
