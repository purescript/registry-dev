module Registry.Legacy.PackageSet
  ( ConvertedLegacyPackageSet
  , LatestCompatibleSets
  , LegacyPackageSet(..)
  , LegacyPackageSetEntry
  , PscTag(..)
  , filterLegacyPackageSets
  , fromPackageSet
  , legacyPackageSetsRepo
  , mirrorLegacySet
  , parsePscTag
  , printDhall
  , printPscTag
  , legacyPackageSetCodec
  , legacyPackageSetEntryCodec
  , pscTagCodec
  , latestCompatibleSetsCodec
  ) where

import Registry.App.Prelude

import Control.Monad.Error.Class as Error
import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Compactable (separate)
import Data.DateTime (Date, DateTime(..))
import Data.DateTime as DateTime
import Data.Filterable (filterMap)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as Format.DateTime
import Data.Formatter.DateTime as Formatter.DateTime
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import Foreign.Git as Git
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Parsing as Parsing
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Registry.App.Json as Json
import Registry.App.Monad (class MonadRegistry, GitHubEnv)
import Registry.Effect.Log as Log
import Registry.Effect.Notify as Notify
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version

legacyPackageSetsRepo :: GitHub.Address
legacyPackageSetsRepo = { owner: "purescript", repo: "package-sets" }

-- | The format of a legacy packages.json package set file
newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet

legacyPackageSetCodec :: JsonCodec LegacyPackageSet
legacyPackageSetCodec =
  Profunctor.wrapIso LegacyPackageSet
    $ Internal.Codec.packageMap legacyPackageSetEntryCodec

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: RawVersion
  }

legacyPackageSetEntryCodec :: JsonCodec LegacyPackageSetEntry
legacyPackageSetEntryCodec = Json.object "LegacyPackageSetEntry"
  { dependencies: CA.array PackageName.codec
  , repo: CA.string
  , version: Profunctor.wrapIso RawVersion CA.string
  }

type ConvertedLegacyPackageSet =
  { tag :: PscTag
  , upstream :: Version
  , packageSet :: LegacyPackageSet
  }

-- | A package set tag for the legacy package sets.
newtype PscTag = PscTag { compiler :: Version, date :: Date }

derive instance Newtype PscTag _
derive instance Eq PscTag
derive instance Ord PscTag

pscTagCodec :: JsonCodec PscTag
pscTagCodec = CA.codec' decode encode
  where
  decode json = do
    tagStr <- CA.decode CA.string json
    lmap (CA.Named "PscTag" <<< CA.TypeMismatch) (parsePscTag tagStr)

  encode =
    CA.encode CA.string <<< printPscTag

pscDateFormat :: List FormatterCommand
pscDateFormat = YearFull : MonthTwoDigits : DayOfMonthTwoDigits : Nil

parsePscTag :: String -> Either String PscTag
parsePscTag = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser do
  _ <- Parsing.String.string "psc-"
  version <- parseVersion =<< charsUntilHyphen
  date <- Parsing.String.rest
  case Format.DateTime.unformat pscDateFormat date of
    Left err ->
      Parsing.fail $ "Expected an 8-digit date such as '20220101': " <> err
    Right parsedDate ->
      pure $ PscTag { compiler: version, date: DateTime.date parsedDate }
  where
  parseVersion =
    Error.liftEither <<< flip Parsing.runParser Version.parser

  charsUntilHyphen =
    map String.CodeUnits.fromCharArray
      $ map fst
      $ Parsing.Combinators.Array.manyTill_ Parsing.String.anyChar
      $ Parsing.String.char '-'

printPscTag :: PscTag -> String
printPscTag (PscTag { compiler, date }) =
  Array.fold
    [ "psc-"
    , Version.print compiler
    , "-"
    , Format.DateTime.format pscDateFormat (DateTime date bottom)
    ]

fromPackageSet :: ManifestIndex -> Map PackageName Metadata -> PackageSet -> Either String ConvertedLegacyPackageSet
fromPackageSet index metadataMap (PackageSet { compiler, packages, published, version }) = do
  converted <- case separate $ mapWithIndex convertPackage packages of
    { left, right } | Map.isEmpty left -> Right right
    { left } -> do
      let toValues = Array.fromFoldable <<< Map.values
      Left $ String.joinWith "\n" $ Array.cons "Failed to convert packages:" $ toValues left

  let converted' = Map.insert (unsafeFromRight (PackageName.parse "metadata")) metadataPackage converted
  pure { tag, upstream: version, packageSet: LegacyPackageSet converted' }
  where
  tag :: PscTag
  tag = PscTag { compiler, date: published }

  -- Legacy package sets determine their compiler version by the version of
  -- the 'metadata' package.
  metadataPackage :: LegacyPackageSetEntry
  metadataPackage =
    { repo: "https://github.com/purescript/purescript-metadata.git"
    , version: RawVersion ("v" <> Version.print compiler)
    , dependencies: []
    }

  convertPackage :: PackageName -> Version -> Either String LegacyPackageSetEntry
  convertPackage packageName packageVersion = do
    versions <- note noIndexPackageError $ Map.lookup packageName $ ManifestIndex.toMap index
    Manifest manifest <- note noIndexVersionError $ Map.lookup packageVersion versions

    Metadata metadata <- note noMetadataPackageError $ Map.lookup packageName metadataMap
    { ref } <- note noMetadataVersionError $ Map.lookup packageVersion metadata.published

    repo <- case metadata.location of
      GitHub { owner, repo, subdir: Nothing } -> Right $ "https://github.com/" <> owner <> "/" <> repo <> ".git"
      Git { url, subdir: Nothing } -> Right url
      GitHub _ -> Left usesSubdirError
      Git _ -> Left usesSubdirError

    pure
      { version: RawVersion ref
      , dependencies: Array.fromFoldable $ Map.keys $ manifest.dependencies
      , repo
      }
    where
    nameStr = PackageName.print packageName
    versionStr = Version.print packageVersion
    noIndexPackageError = "No registry index entry found for " <> nameStr
    noIndexVersionError = "Found registry index entry for " <> nameStr <> " but none for version " <> versionStr
    noMetadataPackageError = "No metadata entry found for " <> nameStr
    noMetadataVersionError = "Found metadata entry for " <> nameStr <> " but no published version for " <> versionStr
    usesSubdirError = "Package " <> nameStr <> " uses the 'subdir' key, which is not supported for legacy package sets."

printDhall :: LegacyPackageSet -> String
printDhall (LegacyPackageSet entries) = do
  let format = Dodo.twoSpaces { pageWidth = 80 }
  let inputs = Map.toUnfoldable entries
  Dodo.print Dodo.plainText format (printPackageSet inputs)
  where
  printPackageSet :: forall a. Array (Tuple PackageName LegacyPackageSetEntry) -> Dodo.Doc a
  printPackageSet packages =
    Dodo.Common.pursCurlies
      $ Dodo.foldWithSeparator Dodo.Common.leadingComma
      $ map printPackage packages

  quoteReserved :: String -> String
  quoteReserved input = do
    -- We can't use any reserved keywords:
    -- https://docs.dhall-lang.org/references/Built-in-types.html#built-in-types-functions-and-operators
    if input `Array.elem` [ "assert", "let", "using", "missing", "merge", "if", "then", "else" ] then
      Array.fold [ "`", input, "`" ]
    else
      input

  printPackage :: forall a. Tuple PackageName LegacyPackageSetEntry -> Dodo.Doc a
  printPackage (Tuple name entry) =
    Array.fold
      [ Dodo.text (quoteReserved (PackageName.print name))
      , Dodo.text " ="
      , Dodo.break
      , Dodo.indent (printEntry entry)
      ]

  printEntry :: forall a. LegacyPackageSetEntry -> Dodo.Doc a
  printEntry entry =
    Dodo.Common.pursCurlies $ Dodo.foldWithSeparator Dodo.Common.leadingComma
      [ Dodo.text "dependencies =" <> breakAlign (printDependencies entry.dependencies)
      , Dodo.text "repo =" <> breakAlign (quoteString entry.repo)
      , Dodo.text "version =" <> breakAlign (printVersion entry.version)
      ]

  breakAlign :: forall a. Dodo.Doc a -> Dodo.Doc a
  breakAlign doc =
    Dodo.flexGroup (Dodo.spaceBreak <> Dodo.align 2 doc)

  printDependencies :: forall a. Array PackageName -> Dodo.Doc a
  printDependencies dependencies =
    if Array.null dependencies then
      Dodo.text "[] : List Text"
    else
      Dodo.Common.pursSquares
        $ Dodo.foldWithSeparator Dodo.Common.leadingComma
        $ map (quoteString <<< PackageName.print) dependencies

  printVersion :: forall a. RawVersion -> Dodo.Doc a
  printVersion (RawVersion version) = quoteString version

  quoteString :: forall a. String -> Dodo.Doc a
  quoteString = Dodo.enclose (Dodo.text "\"") (Dodo.text "\"") <<< Dodo.text

type LatestCompatibleSets = Map Version PscTag

latestCompatibleSetsCodec :: JsonCodec LatestCompatibleSets
latestCompatibleSetsCodec = Internal.Codec.versionMap pscTagCodec

-- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/release.sh
-- https://github.com/purescript/package-sets/blob/psc-0.15.4-20220829/update-latest-compatible-sets.sh
mirrorLegacySet :: forall m r. MonadRegistry m => MonadAsk (GitHubEnv r) m => ConvertedLegacyPackageSet -> m Unit
mirrorLegacySet { tag, packageSet, upstream } = do
  tmp <- liftEffect Tmp.mkTmpDir

  { octokit, cache } <- ask

  packageSetsTags <- liftAff (Except.runExceptT (GitHub.listTags octokit cache legacyPackageSetsRepo)) >>= case _ of
    Left error -> do
      let formatted = GitHub.printGitHubError error
      Notify.die $ "Could not fetch tags for the package-sets repo: " <> formatted
    Right tags -> pure $ Set.fromFoldable $ map _.name tags

  let printedTag = printPscTag tag

  when (Set.member printedTag packageSetsTags) do
    Notify.die $ "Package set tag " <> printedTag <> " already exists, aborting..."

  let packageSetsUrl = "https://github.com/" <> legacyPackageSetsRepo.owner <> "/" <> legacyPackageSetsRepo.repo <> ".git"
  liftAff (Except.runExceptT (Git.runGit [ "clone", packageSetsUrl, "--depth", "1" ] (Just tmp))) >>= case _ of
    Left error -> Notify.die error
    Right _ -> pure unit

  -- We need to write three files to the package sets repository:
  --
  -- * latest-compatible-sets.json
  --   stores a mapping of compiler versions to their highest compatible tag
  --
  -- * packages.json
  --   stores the JSON representation of the latest package set
  --
  -- * src/packages.dhall
  --   stores the Dhall representation of the latest package set

  let packageSetsPath = Path.concat [ tmp, legacyPackageSetsRepo.repo ]
  let latestSetsPath = Path.concat [ packageSetsPath, "latest-compatible-sets.json" ]
  latestCompatibleSets <- do
    latestSets <- liftAff (Json.readJsonFile latestCompatibleSetsCodec latestSetsPath) >>= case _ of
      Left err -> Notify.die $ "Failed to read latest-compatible-sets: " <> err
      Right parsed -> pure parsed
    let key = (un PscTag tag).compiler
    case Map.lookup key latestSets of
      Just existingTag | existingTag >= tag -> do
        Log.info "Not updating latest-compatible sets because this tag (or a higher one) already exists."
        pure latestSets
      _ ->
        pure $ Map.insert key tag latestSets

  let
    packagesDhallPath = Path.concat [ packageSetsPath, "src", "packages.dhall" ]
    packagesJsonPath = Path.concat [ packageSetsPath, "packages.json" ]
    commitFiles =
      [ Tuple packagesDhallPath (printDhall packageSet)
      , Tuple packagesJsonPath (Json.printJson legacyPackageSetCodec packageSet)
      , Tuple latestSetsPath (Json.printJson latestCompatibleSetsCodec latestCompatibleSets)
      ]

  let
    -- We push the stable tag (ie. just a compiler version) if one does not yet
    -- exist. We always push the full tag.
    printedCompiler = Version.print (un PscTag tag).compiler
    tagsToPush = Array.catMaybes
      [ if Set.member printedCompiler packageSetsTags then Nothing else Just printedCompiler
      , Just printedTag
      ]

  result <- liftAff $ Except.runExceptT do
    GitHub.GitHubToken token <- Git.configurePacchettiBotti (Just packageSetsPath)
    for_ commitFiles \(Tuple path contents) -> do
      liftAff $ FS.Aff.writeTextFile UTF8 path (contents <> "\n")
      Git.runGit_ [ "add", path ] (Just packageSetsPath)
    let commitMessage = "Update to the " <> Version.print upstream <> " package set."
    Git.runGit_ [ "commit", "-m", commitMessage ] (Just packageSetsPath)
    let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> legacyPackageSetsRepo.owner <> "/" <> legacyPackageSetsRepo.repo <> ".git"
    Git.runGit_ [ "push", origin, "master" ] (Just packageSetsPath)
    for_ tagsToPush \pushTag -> do
      Git.runGit_ [ "tag", pushTag ] (Just packageSetsPath)
      Git.runGit_ [ "push", origin, pushTag ] (Just packageSetsPath)

  case result of
    Left error -> Notify.die $ "Package set mirroring failed: " <> error
    Right _ -> pure unit

filterLegacyPackageSets :: Array GitHub.Tag -> Array String
filterLegacyPackageSets tags = do
  let
    -- Package sets after this date are published by the registry, and are
    -- therefore not legacy package sets.
    lastLegacyDate = unsafeFromRight $ Formatter.DateTime.unformat Internal.Format.iso8601Date "2022-09-01"
    legacyTag { name } = case parsePscTag name of
      Right (PscTag { date }) | date <= (DateTime.date lastLegacyDate) -> Just name
      _ -> Nothing

  filterMap legacyTag tags
