module Registry.Legacy.PackageSet where

import Registry.Prelude

import Data.Array as Array
import Data.Compactable (separate)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map as Map
import Data.String as String
import Dodo as Dodo
import Dodo.Common as Dodo.Common
import Registry.Index (RegistryIndex)
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location(..), Manifest(..), Metadata, PackageSet(..))
import Registry.Version (Version)
import Registry.Version as Version

newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet
derive newtype instance Show LegacyPackageSet

instance RegistryJson LegacyPackageSet where
  encode (LegacyPackageSet plan) = Json.encode plan
  decode = map LegacyPackageSet <<< Json.decode

type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: RawVersion
  }

type ConvertedLegacyPackageSet =
  { name :: String
  , upstream :: Version
  , packageSet :: LegacyPackageSet
  }

fromPackageSet :: RegistryIndex -> Map PackageName Metadata -> PackageSet -> Either String ConvertedLegacyPackageSet
fromPackageSet index metadata (PackageSet { compiler, packages, published, version }) = do
  converted <- case separate $ mapWithIndex convertPackage packages of
    { left, right } | Map.isEmpty left -> Right right
    { left } -> do
      let toValues = Array.fromFoldable <<< Map.values
      Left $ String.joinWith "\n" $ Array.cons "Failed to convert packages:" $ toValues left

  let converted' = Map.insert (unsafeFromRight (PackageName.parse "metadata")) metadataPackage converted
  pure { name, upstream: version, packageSet: LegacyPackageSet converted' }
  where
  name :: String
  name = do
    let dateFormat = List.fromFoldable [ YearFull, MonthTwoDigits, DayOfMonthTwoDigits ]
    "psc-" <> Version.printVersion compiler <> "-" <> Formatter.DateTime.format dateFormat published

  -- Legacy package sets determine their compiler version by the version of
  -- the 'metadata' package.
  metadataPackage :: LegacyPackageSetEntry
  metadataPackage =
    { repo: "https://github.com/purescript/purescript-metadata.git"
    , version: RawVersion ("v" <> Version.printVersion compiler)
    , dependencies: []
    }

  convertPackage :: PackageName -> Version -> Either String LegacyPackageSetEntry
  convertPackage packageName packageVersion = do
    versions <- note noIndexPackageError $ Map.lookup packageName index
    Manifest manifest <- note noIndexVersionError $ Map.lookup packageVersion versions

    metadataEntries <- note noMetadataPackageError $ Map.lookup packageName metadata
    metadataEntry <- note noMetadataVersionError $ Map.lookup packageVersion metadataEntries.published

    repo <- case metadataEntries.location of
      GitHub { owner, repo, subdir: Nothing } -> Right $ "https://github.com/" <> owner <> "/" <> repo <> ".git"
      Git { gitUrl, subdir: Nothing } -> Right gitUrl
      GitHub _ -> Left usesSubdirError
      Git _ -> Left usesSubdirError

    pure
      { version: RawVersion metadataEntry.ref
      , dependencies: Array.fromFoldable $ Map.keys $ manifest.dependencies
      , repo
      }
    where
    nameStr = PackageName.print packageName
    versionStr = Version.printVersion packageVersion
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
    Dodo.Common.jsCurlies
      $ Dodo.foldWithSeparator Dodo.Common.leadingComma
      $ map printPackage packages

  printPackage :: forall a. Tuple PackageName LegacyPackageSetEntry -> Dodo.Doc a
  printPackage (Tuple name entry) =
    Array.fold
      [ Dodo.text do
          let nameStr = PackageName.print name
          if nameStr == "assert" then "`assert`" else nameStr
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
    Dodo.flexGroup (Dodo.spaceBreak <> Dodo.align 4 doc)

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
