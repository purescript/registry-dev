module Registry.App.Legacy.Manifest where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.Either as Either
import Data.Exists as Exists
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.These (These(..))
import Data.These as These
import Data.Variant as Variant
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Library.Execa as Execa
import Node.Path as Path
import Registry.App.CLI.Licensee as Licensee
import Registry.App.Effect.Cache (class FsEncodable, class MemoryEncodable, Cache, FsEncoding(..), MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.LenientRange as LenientRange
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Legacy.Types (LegacyPackageSet(..), LegacyPackageSetEntry, LegacyPackageSetUnion, RawPackageName(..), RawVersion(..), RawVersionRange(..), legacyPackageSetCodec, legacyPackageSetUnionCodec, rawPackageNameMapCodec, rawVersionCodec, rawVersionRangeCodec)
import Registry.Foreign.Octokit (Address, GitHubError)
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tmp as Tmp
import Registry.License as License
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Run.Except as Run.Except
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

type LegacyManifest =
  { license :: License
  , description :: Maybe String
  , dependencies :: Map PackageName Range
  }

toManifest :: PackageName -> Version -> Location -> LegacyManifest -> Manifest
toManifest name version location { license, description, dependencies } = do
  let files = Nothing
  let owners = Nothing
  Manifest { name, version, location, license, description, dependencies, files, owners }

-- | Attempt to retrieve a license, description, and set of dependencies from a
-- | PureScript repo that does not have a Registry-supported manifest, but does
-- | have a spago.dhall or bower.json manifest.
fetchLegacyManifest
  :: forall r
   . PackageName
  -> Address
  -> RawVersion
  -> Run (GITHUB + LEGACY_CACHE + LOG + EXCEPT String + AFF + EFFECT + r) (Either LegacyManifestValidationError LegacyManifest)
fetchLegacyManifest name address ref = Run.Except.runExceptAt _legacyManifestError do
  legacyPackageSets <- fetchLegacyPackageSets >>= case _ of
    Left error -> do
      Log.error $ "Failed error when to fetch legacy package sets: " <> Octokit.printGitHubError error
      Except.throw "Could not retrieve legacy package sets; aborting to avoid producing incorrect legacy manifest depedency bounds."
    Right union -> pure union

  manifests <- Run.Except.rethrowAt _legacyManifestError =<< fetchLegacyManifestFiles address ref

  dependencies <- do
    let
      -- Package sets will produce exact versions for dependencies, ie. a
      -- dependency on "strings: 4.1.2" will produce the version range
      -- "strings: >=4.1.2 <4.1.3", which while technically correct is almost
      -- certainly not what the user wants. We instead treat these ranges as
      -- caret ranges, so "strings: 4.1.2" becomes "strings: >=4.1.2 <5.0.0".
      fixedToRange :: RawVersion -> RawVersionRange
      fixedToRange (RawVersion fixed) = do
        let parsedVersion = LenientVersion.parse fixed
        let bump version = Version.print (Version.bumpHighest version)
        let printRange version = Array.fold [ ">=", fixed, " <", bump version ]
        RawVersionRange $ Either.either (const fixed) (printRange <<< LenientVersion.version) parsedVersion

      minMaxToRange { min: smallest, max: highest } =
        { min: fixedToRange smallest, max: fixedToRange highest }

      maybePackageSetDeps :: Maybe (Map PackageName Range)
      maybePackageSetDeps =
        Map.lookup name legacyPackageSets >>= Map.lookup ref >>= \deps -> do
          let converted = mapKeys (RawPackageName <<< PackageName.print) $ map minMaxToRange deps
          let smallerBounds = validateDependencies (converted <#> _.min)
          let higherBounds = validateDependencies (converted <#> _.max)
          hush $ Map.unionWith Range.union <$> smallerBounds <*> higherBounds

      validateSpagoDeps :: SpagoDhallJson -> Either LegacyManifestValidationError (Map PackageName Range)
      validateSpagoDeps (SpagoDhallJson { dependencies, packages }) = do
        let
          convert = do
            let findPackage p = Map.lookup p packages
            let foldFn deps p = maybe deps (\{ version } -> Map.insert p (fixedToRange version) deps) (findPackage p)
            Array.foldl foldFn Map.empty

        validateDependencies (convert dependencies)

      validateBowerDeps :: Bowerfile -> Either LegacyManifestValidationError (Map PackageName Range)
      validateBowerDeps (Bowerfile { dependencies }) = do
        let
          -- We remove dependencies that don't begin with 'purescript-', as these
          -- indicate JavaScript dependencies.
          convert = Map.mapMaybeWithKey \(RawPackageName p) range -> do
            _ <- String.stripPrefix (String.Pattern "purescript-") p
            pure range

        validateDependencies (convert dependencies)

      unionManifests = do
        case manifests of
          This bower -> validateBowerDeps bower
          That spago -> validateSpagoDeps spago
          Both bower spago ->
            case validateBowerDeps bower, validateSpagoDeps spago of
              Left bowerError, Left _ -> Left bowerError
              Right bowerDeps, Left _ -> Right bowerDeps
              Left _, Right spagoDeps -> Right spagoDeps
              Right bowerDeps, Right spagoDeps -> Right do
                bowerDeps # mapWithIndex \package range ->
                  case Map.lookup package spagoDeps of
                    Nothing -> range
                    Just spagoRange -> Range.union range spagoRange

      unionPackageSets = case maybePackageSetDeps, unionManifests of
        Nothing, Left manifestError -> Left manifestError
        Nothing, Right manifestDeps -> Right manifestDeps
        Just packageSetDeps, Left _ -> Right packageSetDeps
        Just packageSetDeps, Right manifestDeps -> Right do
          packageSetDeps # mapWithIndex \package range ->
            case Map.lookup package manifestDeps of
              Nothing -> range
              Just manifestRange -> Range.union range manifestRange

    Run.Except.rethrowAt _legacyManifestError unionPackageSets

  license <- do
    let unBower (Bowerfile { license }) = Array.mapMaybe NonEmptyString.fromString license
    let unSpago (SpagoDhallJson { license }) = Array.catMaybes [ license ]
    let manifestLicenses = These.these unBower unSpago (\bower spago -> unBower bower <> unSpago spago) manifests
    detectedLicenses <- detectLicenses address ref
    let licenses = Array.nub $ Array.concat [ detectedLicenses, manifestLicenses ]
    Run.Except.rethrowAt _legacyManifestError $ validateLicense licenses

  let
    description = do
      Bowerfile fields <- These.theseLeft manifests
      fields.description

  pure { license, dependencies, description }

_legacyManifestError :: Proxy "legacyManifestError"
_legacyManifestError = Proxy

type LegacyManifestValidationError = { error :: LegacyManifestError, reason :: String }

data LegacyManifestError
  = NoManifests
  | MissingLicense
  | InvalidLicense (Array String)
  | InvalidDependencies (Array { name :: String, range :: String, error :: String })

legacyManifestErrorCodec :: JsonCodec LegacyManifestError
legacyManifestErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { noManifests: Left unit
  , missingLicense: Left unit
  , invalidLicense: Right (CA.array CA.string)
  , invalidDependencies: Right (CA.array dependencyCodec)
  }
  where
  dependencyCodec = CA.Record.object "Dependency"
    { name: CA.string
    , range: CA.string
    , error: CA.string
    }

  toVariant = case _ of
    NoManifests -> Variant.inj (Proxy :: _ "noManifests") unit
    MissingLicense -> Variant.inj (Proxy :: _ "missingLicense") unit
    InvalidLicense inner -> Variant.inj (Proxy :: _ "invalidLicense") inner
    InvalidDependencies inner -> Variant.inj (Proxy :: _ "invalidDependencies") inner

  fromVariant = Variant.match
    { noManifests: \_ -> NoManifests
    , missingLicense: \_ -> MissingLicense
    , invalidLicense: InvalidLicense
    , invalidDependencies: InvalidDependencies
    }

printLegacyManifestError :: LegacyManifestError -> String
printLegacyManifestError = case _ of
  NoManifests -> "NoManifests"
  MissingLicense -> "MissingLicense"
  InvalidLicense licenses -> "InvalidLicense (" <> String.joinWith ", " licenses <> ")"
  InvalidDependencies errors -> "InvalidDependencies (" <> String.joinWith ", " (map printDepError errors) <> ")"
  where
  printDepError { name, range, error } = "[{ " <> name <> ": " <> range <> "}, " <> error <> "]"

fetchLegacyManifestFiles
  :: forall r
   . Address
  -> RawVersion
  -> Run (GITHUB + LOG + AFF + EFFECT + r) (Either LegacyManifestValidationError (These Bowerfile SpagoDhallJson))
fetchLegacyManifestFiles address ref = do
  eitherBower <- fetchBowerfile address ref
  void $ flip ltraverse eitherBower \error ->
    Log.debug $ "Failed to fetch bowerfile: " <> Octokit.printGitHubError error
  eitherSpago <- fetchSpagoDhallJson address ref
  void $ flip ltraverse eitherSpago \error ->
    Log.debug $ "Failed to fetch spago.dhall: " <> Octokit.printGitHubError error
  pure $ case eitherBower, eitherSpago of
    Left _, Left _ -> Left { error: NoManifests, reason: "No bower.json or spago.dhall files available." }
    Right bower, Left _ -> Right $ This bower
    Left _, Right spago -> Right $ That spago
    Right bower, Right spago -> Right $ Both bower spago

detectLicenses
  :: forall r
   . Address
  -> RawVersion
  -> Run (GITHUB + LOG + AFF + r) (Array NonEmptyString)
detectLicenses address ref = do
  packageJsonFile <- GitHub.getContent address ref "package.json"
  licenseFile <- GitHub.getContent address ref "LICENSE"
  let packageJsonInput = { name: "package.json", contents: _ } <$> hush packageJsonFile
  let licenseInput = { name: "LICENSE", contents: _ } <$> hush licenseFile
  Run.liftAff (Licensee.detectFiles (Array.catMaybes [ packageJsonInput, licenseInput ])) >>= case _ of
    Left err -> Log.warn ("Licensee decoding error, ignoring: " <> err) $> []
    Right licenses -> pure $ Array.mapMaybe NonEmptyString.fromString licenses

validateLicense :: Array NonEmptyString -> Either LegacyManifestValidationError License
validateLicense licenses = do
  let
    rewrite = case _ of
      "Apache 2" -> "Apache-2.0"
      "Apache-2" -> "Apache-2.0"
      "Apache 2.0" -> "Apache-2.0"
      "BSD" -> "BSD-3-Clause"
      "BSD3" -> "BSD-3-Clause"
      "BSD-3" -> "BSD-3-Clause"
      "3-Clause BSD" -> "BSD-3-Clause"
      other -> other

    parsedLicenses =
      map (License.parse <<< rewrite)
        $ Array.filter (_ /= "LICENSE")
        $ map NonEmptyString.toString licenses

  case partitionEithers parsedLicenses of
    { fail: [], success: [] } -> Left { error: MissingLicense, reason: "No licenses found." }
    { fail: [], success } -> Right $ License.joinWith License.And success
    { fail } -> Left { error: InvalidLicense fail, reason: "Licenses found, but not valid SPDX identifiers." }

validateDependencies :: Map RawPackageName RawVersionRange -> Either LegacyManifestValidationError (Map PackageName Range)
validateDependencies dependencies = do
  let
    foldFn (RawPackageName name) acc (RawVersionRange range) = do
      let failWith = { name, range, error: _ }
      case PackageName.parse (stripPureScriptPrefix name), LenientRange.parse range of
        Left nameErr, Left rangeErr ->
          acc { no = Array.cons (failWith (nameErr <> rangeErr)) acc.no }
        Left nameErr, Right _ ->
          acc { no = Array.cons (failWith nameErr) acc.no }
        Right _, Left rangeErr ->
          acc { no = Array.cons (failWith rangeErr) acc.no }
        Right parsedName, Right parsedRange ->
          acc { yes = Array.cons (Tuple parsedName (LenientRange.range parsedRange)) acc.yes }

    parsedDependencies =
      foldlWithIndex foldFn { no: [], yes: [] } dependencies

  case parsedDependencies of
    { no: [], yes } -> pure $ Map.fromFoldable yes
    { no } -> Left { error: InvalidDependencies no, reason: "Version specifies invalid dependencies." }

-- | The result of calling 'dhall-to-json' on a 'spago.dhall' file and
-- | accompanying 'packages.dhall' file.
newtype SpagoDhallJson = SpagoDhallJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  , packages :: Map RawPackageName { version :: RawVersion }
  }

derive instance Newtype SpagoDhallJson _

spagoDhallJsonCodec :: JsonCodec SpagoDhallJson
spagoDhallJsonCodec = Profunctor.dimap toRep fromRep $ CA.Record.object "SpagoDhallJson"
  { license: CA.Record.optional CA.Common.nonEmptyString
  , dependencies: CA.Record.optional (CA.array (Profunctor.wrapIso RawPackageName CA.string))
  , packages: CA.Record.optional packageVersionMapCodec
  }
  where
  packageVersionMapCodec :: JsonCodec (Map RawPackageName { version :: RawVersion })
  packageVersionMapCodec = rawPackageNameMapCodec $ CA.Record.object "VersionObject" { version: rawVersionCodec }

  toRep (SpagoDhallJson fields) = fields
    { dependencies = if Array.null fields.dependencies then Nothing else Just fields.dependencies
    , packages = if Map.isEmpty fields.packages then Nothing else Just fields.packages
    }

  fromRep fields = SpagoDhallJson $ fields
    { dependencies = fromMaybe [] fields.dependencies
    , packages = fromMaybe Map.empty fields.packages
    }

-- | Attempt to construct a SpagoDhallJson file from a spago.dhall and
-- | packages.dhall file located in a remote repository at the given ref.
fetchSpagoDhallJson
  :: forall r
   . Address
  -> RawVersion
  -> Run (GITHUB + LOG + AFF + EFFECT + r) (Either GitHubError SpagoDhallJson)
fetchSpagoDhallJson address ref = Run.Except.runExceptAt _spagoDhallError do
  let getContent file = GitHub.getContent address ref file >>= Run.Except.rethrowAt _spagoDhallError
  spagoDhall <- getContent "spago.dhall"
  packagesDhall <- getContent "packages.dhall"
  tmp <- Tmp.mkTmpDir
  Run.liftAff (Aff.attempt (FS.Aff.writeTextFile UTF8 (Path.concat [ tmp, "packages.dhall" ]) packagesDhall)) >>= case _ of
    Left error -> do
      Log.error $ "Failed to write packages.dhall file to tmp: " <> Aff.message error
      Run.Except.throwAt _spagoDhallError $ Octokit.UnexpectedError "Could not write packages.dhall file."
    Right _ -> pure unit
  Log.debug "Converting spago.dhall and packages.dhall to json."
  dhallJson <- Run.liftAff $ dhallToJson { dhall: spagoDhall, cwd: Just tmp }
  Run.Except.rethrowAt _spagoDhallError $ case dhallJson of
    Left err -> Left $ Octokit.DecodeError err
    Right json -> case CA.decode spagoDhallJsonCodec json of
      Left err -> Left $ Octokit.DecodeError $ CA.printJsonDecodeError err
      Right value -> pure value
  where
  _spagoDhallError :: Proxy "spagoDhallError"
  _spagoDhallError = Proxy

  -- | Convert a string representing a Dhall expression into JSON using the
  -- | `dhall-to-json` CLI.
  dhallToJson :: { dhall :: String, cwd :: Maybe FilePath } -> Aff (Either String Json)
  dhallToJson { dhall, cwd } = do
    let cmd = "dhall-to-json"
    let args = []
    process <- Execa.execa cmd args (_ { cwd = cwd })
    process.stdin.writeUtf8End dhall
    result <- process.result
    pure case result of
      Right { stdout } -> lmap CA.printJsonDecodeError $ parseJson CA.json stdout
      Left { stderr } -> Left stderr

newtype Bowerfile = Bowerfile
  { description :: Maybe String
  , dependencies :: Map RawPackageName RawVersionRange
  , license :: Array String
  }

derive instance Newtype Bowerfile _
derive newtype instance Eq Bowerfile

bowerfileCodec :: JsonCodec Bowerfile
bowerfileCodec = Profunctor.dimap toRep fromRep $ CA.Record.object "Bowerfile"
  { description: CA.Record.optional CA.string
  , dependencies: CA.Record.optional dependenciesCodec
  , license: CA.Record.optional licenseCodec
  }
  where
  toRep (Bowerfile fields) = fields { dependencies = Just fields.dependencies, license = Just fields.license }
  fromRep fields = Bowerfile $ fields { dependencies = fromMaybe Map.empty fields.dependencies, license = fromMaybe [] fields.license }

  dependenciesCodec :: JsonCodec (Map RawPackageName RawVersionRange)
  dependenciesCodec = rawPackageNameMapCodec rawVersionRangeCodec

  licenseCodec :: JsonCodec (Array String)
  licenseCodec = CA.codec' decode encode
    where
    decode json = CA.decode (CA.array CA.string) json <|> map Array.singleton (CA.decode CA.string json)
    encode = CA.encode (CA.array CA.string)

-- | Attempt to construct a Bowerfile from a bower.json file located in a
-- | remote repository at the given ref.
fetchBowerfile :: forall r. Address -> RawVersion -> Run (GITHUB + r) (Either GitHubError Bowerfile)
fetchBowerfile address ref = GitHub.getJsonFile address ref bowerfileCodec "bower.json"

-- | Attempt to fetch all package sets from the package-sets repo and union them
-- | into a map, where keys are packages in the sets and values are a map of
-- | the package version to its dependencies in the set at that version.
fetchLegacyPackageSets :: forall r. Run (GITHUB + LEGACY_CACHE + LOG + EFFECT + r) (Either GitHubError LegacyPackageSetUnion)
fetchLegacyPackageSets = Run.Except.runExceptAt _legacyPackageSetsError do
  allTags <- do
    tagsResult <- GitHub.listTags Legacy.PackageSet.legacyPackageSetsRepo
    Run.Except.rethrowAt _legacyPackageSetsError tagsResult

  let
    tags :: Array String
    tags = Legacy.PackageSet.filterLegacyPackageSets allTags

    convertPackageSet :: LegacyPackageSet -> SemigroupMap PackageName (SemigroupMap RawVersion (SemigroupMap PackageName { min :: Min RawVersion, max :: Max RawVersion }))
    convertPackageSet (LegacyPackageSet packages) = SemigroupMap $ map (convertEntry packages) packages

    convertEntry :: Map PackageName LegacyPackageSetEntry -> LegacyPackageSetEntry -> SemigroupMap RawVersion (SemigroupMap PackageName { min :: Min RawVersion, max :: Max RawVersion })
    convertEntry entries { dependencies, version } = do
      let
        -- Package sets are only valid if all packages in the set have dependencies that are also in
        -- the set, so this (should) be safe.
        resolveDependencyVersion dep =
          let
            v = unsafeFromJust (_.version <$> Map.lookup dep entries)
          in
            { min: Min v, max: Max v }
        resolveDependencyVersions =
          Array.foldl (\m name -> Map.insert name (resolveDependencyVersion name) m) Map.empty
      SemigroupMap $ Map.singleton version $ SemigroupMap $ resolveDependencyVersions dependencies

  Log.debug "Merging legacy package sets into a union."
  tagsHash <- Sha256.hashString (String.joinWith " " tags)

  -- It's important that we cache the end result of unioning all package sets
  -- because the package sets are quite large and it's expensive to read them
  -- all into memory and fold over them.
  Cache.get _legacyCache (LegacyUnion tagsHash) >>= case _ of
    Nothing -> do
      Log.debug $ "Cache miss for legacy package set union, rebuilding..."

      legacySetResults <- for tags \refStr -> do
        let ref = RawVersion refStr
        cached <- Cache.get _legacyCache (LegacySet ref) >>= case _ of
          Nothing -> do
            Log.debug $ "Cache miss for legacy package set " <> refStr <> ", refetching..."
            result <- GitHub.getJsonFile Legacy.PackageSet.legacyPackageSetsRepo ref legacyPackageSetCodec "packages.json"
            Cache.put _legacyCache (LegacySet ref) result
            pure result
          Just value ->
            pure value
        pure $ lmap (Tuple ref) cached

      let results = partitionEithers legacySetResults
      when (not (Array.null results.fail)) do
        Log.warn $ "Failed to retrieve package sets for some tags: " <> String.joinWith ", " (map (fst >>> un RawVersion) results.fail)

      let
        convertedSets :: Array (SemigroupMap PackageName (SemigroupMap RawVersion (SemigroupMap PackageName { min :: Min RawVersion, max :: Max RawVersion })))
        convertedSets = map convertPackageSet results.success

        merged :: LegacyPackageSetUnion
        merged = coerce $ fold convertedSets

      Cache.put _legacyCache (LegacyUnion tagsHash) merged
      pure merged

    Just value ->
      pure value
  where
  _legacyPackageSetsError :: Proxy "legacyPackageSetsError"
  _legacyPackageSetsError = Proxy

-- | A key type for the legacy cache.
data LegacyCache (c :: Type -> Type -> Type) a
  = LegacySet RawVersion (c (Either GitHubError LegacyPackageSet) a)
  | LegacyUnion Sha256 (c LegacyPackageSetUnion a)

instance Functor2 c => Functor (LegacyCache c) where
  map k = case _ of
    LegacySet ref a -> LegacySet ref (map2 k a)
    LegacyUnion tagsHash a -> LegacyUnion tagsHash (map2 k a)

instance MemoryEncodable LegacyCache where
  encodeMemory = case _ of
    LegacySet (RawVersion ref) next ->
      Exists.mkExists $ Key ("LegacySet__" <> ref) next
    LegacyUnion hash next ->
      Exists.mkExists $ Key ("LegacyUnion__" <> Sha256.print hash) next

instance FsEncodable LegacyCache where
  encodeFs = case _ of
    LegacySet (RawVersion ref) next ->
      Exists.mkExists $ AsJson ("LegacySet__" <> ref) (CA.Common.either Octokit.githubErrorCodec legacyPackageSetCodec) next
    LegacyUnion hash next ->
      Exists.mkExists $ AsJson ("LegacyUnion" <> Sha256.print hash) legacyPackageSetUnionCodec next

type LEGACY_CACHE r = (legacyCache :: Cache LegacyCache | r)

_legacyCache :: Proxy "legacyCache"
_legacyCache = Proxy
