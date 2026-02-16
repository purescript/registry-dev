-- | Implementation of the `License` data type from the registry spec.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#license
-- |
-- | WARNING:
-- | This module relies on the 'spdx-expression-parse' NPM library, which you
-- | must install if you are using parsing code from this module. Please see the
-- | package.json file for exact versions.
module Registry.License
  ( License
  , SPDXConjunction(..)
  , canonicalizeDetected
  , codec
  , extractIds
  , joinWith
  , parse
  , print
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (Except, except)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import JSON (JSON)

-- | An SPDX license identifier such as 'MIT' or 'Apache-2.0'.
newtype License = License LicenseTree

derive newtype instance Eq License

-- | A codec for encoding and decoding a `License` as JSON
codec :: CJ.Codec License
codec = CJ.named "License" $ Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError License
  decode = except <<< lmap CJ.DecodeError.basic <<< parse <=< Codec.decode CJ.string

  encode :: License -> JSON
  encode = print >>> CJ.encode CJ.string

data LicenseTree
  = Leaf CanonicalLicenseLeaf
  | Branch SPDXConjunction LicenseTree LicenseTree

derive instance Eq LicenseTree

type CanonicalLicenseLeaf =
  { identifier :: String
  , exception :: Maybe String
  }

type ParsedLicenseLeaf =
  { identifier :: String
  , plus :: Boolean
  , exception :: Maybe String
  }

data ParsedLicenseTree
  = ParsedLeaf ParsedLicenseLeaf
  | ParsedBranch SPDXConjunction ParsedLicenseTree ParsedLicenseTree

derive instance Eq ParsedLicenseTree

foreign import parseLicenseTreeImpl
  :: forall r
   . Fn5
       (String -> r)
       (String -> Boolean -> String -> r)
       (r -> r -> r)
       (r -> r -> r)
       String
       r

foreign import currentIds :: Array String
foreign import deprecatedIds :: Array String

-- | Parse a string as a SPDX license identifier.
parse :: String -> Either String License
parse input = do
  parsedTree <- parseExpressionTree input
  canonicalTree <- canonicalizeParsedTree canonicalizeStrictLeaf parsedTree
  pure $ License canonicalTree

-- | Canonicalize SPDX IDs detected from external tooling. This is lenient for
-- | deprecated SPDX IDs where the canonical replacement is unambiguous.
canonicalizeDetected :: String -> Either String String
canonicalizeDetected input = do
  parsedTree <- parseExpressionTree input
  canonicalTree <- canonicalizeParsedTree canonicalizeDetectedLeaf parsedTree
  pure $ renderCanonicalTree canonicalTree

parseExpressionTree :: String -> Either String ParsedLicenseTree
parseExpressionTree =
  runFn5 parseLicenseTreeImpl Left onLeaf onAnd onOr
  where
  onLeaf :: String -> Boolean -> String -> Either String ParsedLicenseTree
  onLeaf identifier plus exception = Right $ ParsedLeaf
    { identifier
    , plus
    , exception: if String.null exception then Nothing else Just exception
    }

  onAnd :: Either String ParsedLicenseTree -> Either String ParsedLicenseTree -> Either String ParsedLicenseTree
  onAnd left right = ParsedBranch And <$> left <*> right

  onOr :: Either String ParsedLicenseTree -> Either String ParsedLicenseTree -> Either String ParsedLicenseTree
  onOr left right = ParsedBranch Or <$> left <*> right

canonicalizeParsedTree
  :: (ParsedLicenseLeaf -> Either String CanonicalLicenseLeaf)
  -> ParsedLicenseTree
  -> Either String LicenseTree
canonicalizeParsedTree canonicalizeLeaf = case _ of
  ParsedLeaf leaf ->
    map Leaf $ canonicalizeLeaf leaf
  ParsedBranch conjunction left right ->
    Branch conjunction
      <$> canonicalizeParsedTree canonicalizeLeaf left
      <*> canonicalizeParsedTree canonicalizeLeaf right

canonicalizeStrictLeaf :: ParsedLicenseLeaf -> Either String CanonicalLicenseLeaf
canonicalizeStrictLeaf rawLeaf = do
  canonicalLeaf <- canonicalizeDetectedLeaf rawLeaf
  if printParsedLeaf rawLeaf == printCanonicalLeaf canonicalLeaf then
    Right canonicalLeaf
  else
    Left $ Array.fold
      [ "Non-canonical SPDX identifier '"
      , printParsedLeaf rawLeaf
      , "'. Use '"
      , printCanonicalLeaf canonicalLeaf
      , "'"
      ]

canonicalizeDetectedLeaf :: ParsedLicenseLeaf -> Either String CanonicalLicenseLeaf
canonicalizeDetectedLeaf { identifier, plus, exception } = do
  if isCurrent identifier then do
    canonicalIdentifier <- if plus then canonicalizePlusIdentifier identifier else Right identifier
    ensureCurrentIdentifier canonicalIdentifier identifier
    pure { identifier: canonicalIdentifier, exception }
  else if isDeprecated identifier then do
    canonicalIdentifier <- canonicalizeDeprecatedIdentifier { identifier, plus }
    pure { identifier: canonicalIdentifier, exception }
  else do
    Left $ "SPDX identifier '" <> identifier <> "' is not recognized in the current SPDX license list"

canonicalizeVersionedIdentifier :: { base :: String, plus :: Boolean } -> String
canonicalizeVersionedIdentifier { base, plus } = if plus then base <> "-or-later" else base <> "-only"

canonicalizeDeprecatedIdentifier :: { identifier :: String, plus :: Boolean } -> Either String String
canonicalizeDeprecatedIdentifier { identifier, plus } = do
  let canonicalVersioned = canonicalizeVersionedIdentifier { base: identifier, plus }
  if plus && isCurrent canonicalVersioned then
    Right canonicalVersioned
  else if Set.member identifier ambiguousDeprecatedIdentifiers then do
    Left $ "Deprecated SPDX identifier '" <> identifier <> "' does not have an unambiguous canonical replacement"
  else if isCurrent canonicalVersioned then
    Right canonicalVersioned
  else case Map.lookup identifier deprecatedIdentifierRenames of
    Just replacement -> do
      if plus then
        Left $ "Deprecated SPDX identifier '" <> identifier <> "+' does not have an unambiguous canonical replacement"
      else do
        ensureCurrentIdentifier replacement identifier
        Right replacement
    Nothing ->
      Left $ "Deprecated SPDX identifier '" <> identifier <> "' does not have an unambiguous canonical replacement"

canonicalizePlusIdentifier :: String -> Either String String
canonicalizePlusIdentifier identifier = do
  let fallback = identifier <> "-or-later"
  let fromOnly = String.stripSuffix (Pattern "-only") identifier <#> (_ <> "-or-later")

  case fromOnly of
    Just candidate ->
      if isCurrent candidate then
        Right candidate
      else if isCurrent fallback then
        Right fallback
      else
        Left $ "Cannot canonicalize '+' for SPDX identifier '" <> identifier <> "'"
    Nothing ->
      if isCurrent fallback then
        Right fallback
      else
        Left $ "Cannot canonicalize '+' for SPDX identifier '" <> identifier <> "'"

ensureCurrentIdentifier :: String -> String -> Either String Unit
ensureCurrentIdentifier canonicalIdentifier sourceIdentifier = do
  unless (isCurrent canonicalIdentifier) do
    Left $ "SPDX identifier '" <> sourceIdentifier <> "' is not recognized in the current SPDX license list"

isCurrent :: String -> Boolean
isCurrent identifier =
  Set.member identifier spdxIdentifierSets.current
    && not (Set.member identifier spdxIdentifierSets.deprecated)

isDeprecated :: String -> Boolean
isDeprecated identifier = Set.member identifier spdxIdentifierSets.deprecated

spdxIdentifierSets :: { current :: Set.Set String, deprecated :: Set.Set String }
spdxIdentifierSets =
  { current: Set.fromFoldable currentIds
  , deprecated: Set.fromFoldable deprecatedIds
  }

-- Deprecated identifiers that have deterministic, non-versioned renames.
deprecatedIdentifierRenames :: Map.Map String String
deprecatedIdentifierRenames = Map.fromFoldable
  [ Tuple "BSD-2-Clause-NetBSD" "BSD-2-Clause"
  , Tuple "StandardML-NJ" "SMLNJ"
  , Tuple "bzip2-1.0.5" "bzip2-1.0.6"
  ]

ambiguousDeprecatedIdentifiers :: Set.Set String
ambiguousDeprecatedIdentifiers = Set.fromFoldable
  [ "BSD-2-Clause-FreeBSD"
  , "GFDL-1.1"
  , "GFDL-1.2"
  , "GFDL-1.3"
  , "Net-SNMP"
  , "Nunit"
  ]

-- | Print an SPDX license identifier as a string.
print :: License -> String
print (License tree) = renderCanonicalTree tree

renderCanonicalTree :: LicenseTree -> String
renderCanonicalTree = go 0
  where
  go :: Int -> LicenseTree -> String
  go parentPrecedence = case _ of
    Leaf leaf ->
      printCanonicalLeaf leaf
    Branch conjunction left right ->
      if conjunctionPrecedence conjunction < parentPrecedence then
        "(" <> renderBranch conjunction left right <> ")"
      else
        renderBranch conjunction left right

  renderBranch :: SPDXConjunction -> LicenseTree -> LicenseTree -> String
  renderBranch conjunction left right = Array.fold
    [ go (conjunctionPrecedence conjunction) left
    , " "
    , printConjunction conjunction
    , " "
    , go (conjunctionPrecedence conjunction) right
    ]

conjunctionPrecedence :: SPDXConjunction -> Int
conjunctionPrecedence = case _ of
  Or -> 1
  And -> 2

printConjunction :: SPDXConjunction -> String
printConjunction = case _ of
  And -> "AND"
  Or -> "OR"

printParsedLeaf :: ParsedLicenseLeaf -> String
printParsedLeaf { identifier, plus, exception } = case exception of
  Nothing ->
    if plus then identifier <> "+" else identifier
  Just exceptionId ->
    (if plus then identifier <> "+" else identifier) <> " WITH " <> exceptionId

printCanonicalLeaf :: CanonicalLicenseLeaf -> String
printCanonicalLeaf { identifier, exception } = case exception of
  Nothing ->
    identifier
  Just exceptionId ->
    identifier <> " WITH " <> exceptionId

-- | Extract all license identifiers from a SPDX expression.
-- | Returns an array of uppercase license IDs for case-insensitive comparison.
-- | For example, "MIT AND Apache-2.0" returns ["MIT", "APACHE-2.0"].
extractIds :: License -> Array String
extractIds (License tree) = Array.nub (collectIds tree)
  where
  collectIds :: LicenseTree -> Array String
  collectIds = case _ of
    Leaf { identifier } -> [ String.toUpper identifier ]
    Branch _ left right -> collectIds left <> collectIds right

-- | A valid conjunction for SPDX license identifiers. AND means that both
-- | licenses must be satisfied; OR means that at least one license must be
-- | satisfied.
data SPDXConjunction = And | Or

derive instance Eq SPDXConjunction

-- | Join multiple license identifiers together with the given SPDX conjunction
-- | to create a new valid SPDX license identifier.
joinWith :: SPDXConjunction -> NonEmptyArray License -> License
joinWith conjunction licenses = case NonEmptyArray.uncons licenses of
  { head, tail } -> Array.foldl join head tail
  where
  join :: License -> License -> License
  join (License left) (License right) = License $ Branch conjunction left right
