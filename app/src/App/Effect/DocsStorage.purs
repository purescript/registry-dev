-- | Storage for canonical package documentation artifacts. Unlike package
-- | tarballs, docs are derived data and can be replaced after generator fixes.
module Registry.App.Effect.DocsStorage
  ( DOCS_STORAGE
  , DocsStorage(..)
  , S3Env
  , _docsStorage
  , delete
  , download
  , exists
  , formatDocsPath
  , handleFs
  , handleReadOnly
  , handleS3
  , interpret
  , replace
  , upload
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.Array as Array
import Effect.Aff as Aff
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Docgen.Codec as Docgen.Codec
import Registry.Docgen.Docs (DocPackage(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.S3 as S3
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Unsafe.Coerce (unsafeCoerce)

data DocsStorage a
  = Upload DocPackage (Either String Unit -> a)
  | Replace DocPackage (Either String Unit -> a)
  | Download PackageName Version (Either String DocPackage -> a)
  | Exists PackageName Version (Either String Boolean -> a)
  | Delete PackageName Version (Either String Unit -> a)

derive instance Functor DocsStorage

type DOCS_STORAGE r = (docsStorage :: DocsStorage | r)

_docsStorage :: Proxy "docsStorage"
_docsStorage = Proxy

upload :: forall r. DocPackage -> Run (DOCS_STORAGE + EXCEPT String + r) Unit
upload docs = Except.rethrow =<< Run.lift _docsStorage (Upload docs identity)

replace :: forall r. DocPackage -> Run (DOCS_STORAGE + EXCEPT String + r) Unit
replace docs = Except.rethrow =<< Run.lift _docsStorage (Replace docs identity)

download :: forall r. PackageName -> Version -> Run (DOCS_STORAGE + EXCEPT String + r) DocPackage
download name version = Except.rethrow =<< Run.lift _docsStorage (Download name version identity)

exists :: forall r. PackageName -> Version -> Run (DOCS_STORAGE + EXCEPT String + r) Boolean
exists name version = Except.rethrow =<< Run.lift _docsStorage (Exists name version identity)

delete :: forall r. PackageName -> Version -> Run (DOCS_STORAGE + EXCEPT String + r) Unit
delete name version = Except.rethrow =<< Run.lift _docsStorage (Delete name version identity)

interpret :: forall r a. (DocsStorage ~> Run r) -> Run (DOCS_STORAGE + r) a -> Run r a
interpret handler = Run.interpret (Run.on _docsStorage handler Run.send)

formatDocsPath :: PackageName -> Version -> String
formatDocsPath name version = PackageName.print name <> "/" <> Version.print version <> ".json"

handleFs :: forall r a. FilePath -> DocsStorage a -> Run (AFF + EFFECT + r) a
handleFs root = case _ of
  Upload docs reply -> do
    let { name, version } = packageIdentity docs
    let path = Path.concat [ root, formatDocsPath name version ]
    result <- writeDocsFile FS.Aff.link path docs
    pure $ reply case result of
      Left error
        | nodeErrorCode error == "EEXIST" ->
            Left $ "Documentation for " <> formatPackageVersion name version <> " already exists."
      other -> lmap Aff.message other

  Replace docs reply -> do
    let { name, version } = packageIdentity docs
    result <- writeDocsFile FS.Aff.rename (Path.concat [ root, formatDocsPath name version ]) docs
    pure $ reply $ lmap Aff.message result

  Download name version reply -> do
    let path = Path.concat [ root, formatDocsPath name version ]
    result <- Run.liftAff $ readJsonFile Docgen.Codec.docPackage path
    pure $ reply $ result
      # lmap (\error -> "Could not read documentation for " <> formatPackageVersion name version <> ": " <> error)
      >>= validateIdentity name version

  Exists name version reply -> do
    present <- Run.liftEffect $ FS.Sync.exists $ Path.concat [ root, formatDocsPath name version ]
    pure $ reply $ Right present

  Delete name version reply -> do
    let path = Path.concat [ root, formatDocsPath name version ]
    present <- Run.liftEffect $ FS.Sync.exists path
    result <- if present then Run.liftAff $ Aff.attempt $ FS.Extra.remove path else pure $ Right unit
    pure $ reply $ lmap (\error -> "Could not delete documentation for " <> formatPackageVersion name version <> ": " <> Aff.message error) result
  where
  writeDocsFile install path docs = Run.liftAff $ Aff.attempt do
    FS.Extra.ensureDirectory $ Path.dirname path
    tempDir <- FS.Aff.mkdtemp $ Path.concat [ Path.dirname path, ".docs-" ]
    Aff.finally (FS.Extra.remove tempDir) do
      let tempPath = Path.concat [ tempDir, Path.basename path ]
      writeJsonFile Docgen.Codec.docPackage tempPath docs
      install tempPath path

  nodeErrorCode :: Aff.Error -> String
  nodeErrorCode error = (unsafeCoerce error :: { code :: String }).code

type S3Env =
  { bucket :: String
  , s3 :: S3.SpaceKey
  }

handleS3 :: forall r a. S3Env -> DocsStorage a -> Run (RESOURCE_ENV + LOG + AFF + EFFECT + r) a
handleS3 env = case _ of
  Upload docs reply -> map (map reply) Except.runExcept do
    let { name, version } = packageIdentity docs
    s3 <- connectS3 env
    -- Spaces does not support conditional PutObject requests, so this
    -- preflight check is best-effort rather than an atomic create.
    whenM (objectExists s3 name version) do
      Except.throw $ "Documentation for " <> formatPackageVersion name version <> " already exists."
    putDocs s3 docs

  Replace docs reply -> map (map reply) Except.runExcept do
    s3 <- connectS3 env
    putDocs s3 docs

  Download name version reply -> map (map reply) Except.runExcept do
    s3 <- connectS3 env
    let key = formatDocsPath name version
    result <- Run.liftAff $ withRetryOnTimeout $ Aff.attempt $ S3.getObject s3 { key }
    buffer <- handleS3Result ("download documentation for " <> formatPackageVersion name version) result
    contents <- Run.liftEffect $ Buffer.toString UTF8 buffer
    docs <- Except.rethrow $ lmap DecodeError.print $ parseJson Docgen.Codec.docPackage contents
    Except.rethrow $ validateIdentity name version docs

  Exists name version reply -> map (map reply) Except.runExcept do
    s3 <- connectS3 env
    objectExists s3 name version

  Delete name version reply -> map (map reply) Except.runExcept do
    s3 <- connectS3 env
    let key = formatDocsPath name version
    result <- Run.liftAff $ withRetryOnTimeout $ Aff.attempt $ S3.deleteObject s3 { key }
    void $ handleS3Result ("delete documentation for " <> formatPackageVersion name version) result

handleReadOnly :: forall r a. S3Env -> DocsStorage a -> Run (RESOURCE_ENV + LOG + AFF + EFFECT + r) a
handleReadOnly env operation = case operation of
  Upload docs reply -> do
    let { name, version } = packageIdentity docs
    Log.warn $ "Skipping documentation upload for " <> formatPackageVersion name version <> " in read-only mode."
    pure $ reply $ Right unit
  Replace docs reply -> do
    let { name, version } = packageIdentity docs
    Log.warn $ "Skipping documentation replacement for " <> formatPackageVersion name version <> " in read-only mode."
    pure $ reply $ Right unit
  Delete name version reply -> do
    Log.warn $ "Skipping documentation deletion for " <> formatPackageVersion name version <> " in read-only mode."
    pure $ reply $ Right unit
  read -> handleS3 env read

packageIdentity :: DocPackage -> { name :: PackageName, version :: Version }
packageIdentity (DocPackage { name, version }) = { name, version }

validateIdentity :: PackageName -> Version -> DocPackage -> Either String DocPackage
validateIdentity expectedName expectedVersion docs = do
  let actual = packageIdentity docs
  if actual.name == expectedName && actual.version == expectedVersion then
    Right docs
  else
    Left $ "Stored documentation at " <> formatDocsPath expectedName expectedVersion
      <> " identifies itself as "
      <> formatPackageVersion actual.name actual.version
      <> "."

connectS3 :: forall r. S3Env -> Run (RESOURCE_ENV + LOG + EXCEPT String + AFF + r) S3.Space
connectS3 env = do
  { s3BucketUrl } <- Env.askResourceEnv
  Log.debug $ "Connecting to documentation bucket " <> env.bucket <> "."
  result <- Run.liftAff $ withRetryOnTimeout $ Aff.attempt $ S3.connect env.s3 s3BucketUrl env.bucket
  handleS3Result "connect to the documentation storage backend" result

objectExists :: forall r. S3.Space -> PackageName -> Version -> Run (LOG + EXCEPT String + AFF + r) Boolean
objectExists s3 name version = do
  let key = formatDocsPath name version
  result <- Run.liftAff $ withRetryOnTimeout $ Aff.attempt $ S3.listObjects s3 { prefix: key }
  objects <- handleS3Result ("check documentation for " <> formatPackageVersion name version) result
  pure $ Array.any (_.key >>> eq key) objects

putDocs :: forall r. S3.Space -> DocPackage -> Run (LOG + EXCEPT String + AFF + EFFECT + r) Unit
putDocs s3 docs = do
  let { name, version } = packageIdentity docs
  let key = formatDocsPath name version
  buffer <- Run.liftEffect $ Buffer.fromString (stringifyJson Docgen.Codec.docPackage docs) UTF8
  result <- Run.liftAff $ withRetryOnTimeout $ Aff.attempt $ S3.putObject s3 { key, body: buffer, acl: S3.PublicRead }
  void $ handleS3Result ("upload documentation for " <> formatPackageVersion name version) result

handleS3Result :: forall value r. String -> RetryResult Aff.Error value -> Run (LOG + EXCEPT String + r) value
handleS3Result operation = case _ of
  Cancelled -> do
    Log.error $ "Timed out while attempting to " <> operation <> "."
    Except.throw $ "Could not " <> operation <> "."
  Failed error -> do
    Log.error $ "Failed to " <> operation <> ": " <> Aff.message error
    Except.throw $ "Could not " <> operation <> "."
  Succeeded value -> pure value
