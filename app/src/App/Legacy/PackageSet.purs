module Registry.App.Legacy.PackageSet
  ( ConvertedLegacyPackageSet
  , LatestCompatibleSets
  , PscTag(..)
  , filterLegacyPackageSets
  , convertPackageSet
  , legacyPackageSetsRepo
  , parsePscTag
  , printDhall
  , printPscTag
  , pscTagCodec
  , latestCompatibleSetsCodec
  ) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class as Error
import Data.Array as Array
import Data.Codec as Codec
import Data.Codec.JSON as CJ
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
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import Parsing as Parsing
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Registry.App.Legacy.Types (LegacyPackageSet(..), LegacyPackageSetEntry, RawVersion(..))
import Registry.Foreign.Octokit (Address, Tag)
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version

legacyPackageSetsRepo :: Address
legacyPackageSetsRepo = { owner: "purescript", repo: "package-sets" }

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

pscTagCodec :: CJ.Codec PscTag
pscTagCodec = CJ.named "PscTag" $ Codec.codec' decode encode
  where
  decode json = do
    tagStr <- Codec.decode CJ.string json
    except $ lmap CJ.DecodeError.basic $ parsePscTag tagStr

  encode =
    CJ.encode CJ.string <<< printPscTag

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

convertPackageSet :: ManifestIndex -> PackageSet -> Either String ConvertedLegacyPackageSet
convertPackageSet index (PackageSet { compiler, packages, published, version }) = do
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

    repo <- case manifest.location of
      GitHub { owner, repo, subdir: Nothing } -> Right $ "https://github.com/" <> owner <> "/" <> repo <> ".git"
      Git { url, subdir: Nothing } -> Right url
      GitHub _ -> Left usesSubdirError
      Git _ -> Left usesSubdirError

    pure
      { version: RawVersion manifest.ref
      , dependencies: Array.fromFoldable $ Map.keys $ manifest.dependencies
      , repo
      }
    where
    nameStr = PackageName.print packageName
    versionStr = Version.print packageVersion
    noIndexPackageError = "No registry index entry found for " <> nameStr
    noIndexVersionError = "Found registry index entry for " <> nameStr <> " but none for version " <> versionStr
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

latestCompatibleSetsCodec :: CJ.Codec LatestCompatibleSets
latestCompatibleSetsCodec = Internal.Codec.versionMap pscTagCodec

-- | Filter the package sets to only those published before the registry was
-- | launched.
filterLegacyPackageSets :: Array Tag -> Array String
filterLegacyPackageSets tags = do
  let
    -- Package sets after this date are published by the registry, and are
    -- therefore not legacy package sets.
    lastLegacyDate = unsafeFromRight $ Formatter.DateTime.unformat Internal.Format.iso8601Date "2022-09-01"
    legacyTag { name } = case parsePscTag name of
      Right (PscTag { date }) | date <= (DateTime.date lastLegacyDate) -> Just name
      _ -> Nothing

  filterMap legacyTag tags
