module Test.Registry.ManifestIndex (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Either (Either(..))
import Data.Int as Int
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import JSON as JSON
import Node.Path as Path
import Registry.Internal.Codec as Internal.Codec
import Registry.Location (Location(..))
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Test.Assert as Assert
import Registry.Test.Utils (unsafeManifest)
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips package entry fixture" do
    let parsedContext = ManifestIndex.parseEntry contextEntry
    contextEntry `Assert.shouldEqualRight` map (ManifestIndex.printEntry <<< NonEmptySet.fromFoldable1) parsedContext

  Spec.it "Produces correct entry file paths" do
    let
      entries =
        [ "a" /\ Path.concat [ "1", "a" ]
        , "ab" /\ Path.concat [ "2", "ab" ]
        , "abc" /\ Path.concat [ "3", "a", "abc" ]
        , "abcd" /\ Path.concat [ "ab", "cd", "abcd" ]
        , "abcde" /\ Path.concat [ "ab", "cd", "abcde" ]
        ]

      parse (name /\ path) = do
        let computedPath = ManifestIndex.packageEntryFilePath (Utils.unsafePackageName name)
        if computedPath == path then Right unit else Left (name /\ path /\ computedPath)

      formatError (name /\ expected /\ received) = name <> " should have path " <> expected <> " but got path " <> received

      { fail } = Utils.partitionEithers $ map parse entries

    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some package names did not map to correct directories:"
        , Array.foldMap (append "\n  - " <<< formatError) fail
        ]

  Spec.it "Prefers second manifest in the case of duplicate insertion." do
    let
      manifest1 = unsafeManifest "prelude" "1.0.0" []
      manifest2 = Newtype.over Manifest (_ { description = Just "My prelude description." }) manifest1
      index =
        ManifestIndex.insert manifest1 ManifestIndex.empty
          >>= ManifestIndex.insert manifest2

    case index of
      Left errors ->
        Assert.fail $ formatInsertErrors errors
      Right result -> do
        let name = (un Manifest manifest1).name
        let version = (un Manifest manifest1).version
        case ManifestIndex.lookup name version result of
          Nothing ->
            Assert.fail $ "Missing package: " <> Utils.formatPackageVersion name version
          Just (Manifest { description }) ->
            description `Assert.shouldEqual` (un Manifest manifest2).description

  Spec.it "Topologically sorts manifests" do
    testSorted
      [ unsafeManifest "control" "2.0.0" []
      , unsafeManifest "prelude" "2.0.0" [ Tuple "control" ">=2.0.0 <3.0.0" ]
      , unsafeManifest "prelude" "1.0.0" []
      , unsafeManifest "control" "1.0.0" [ Tuple "prelude" ">=1.0.0 <2.0.0" ]
      , unsafeManifest "transformers" "1.0.0" [ Tuple "control" ">=2.0.0 <3.0.0", Tuple "prelude" ">=1.0.0 <2.0.0" ]
      ]

  Spec.it "Parses tiny index" do
    let
      tinyIndex :: Array Manifest
      tinyIndex = [ unsafeManifest "prelude" "1.0.0" [] ]

    testIndex { satisfied: tinyIndex, unsatisfied: [] }

  Spec.it "Fails to parse non-self-contained index" do
    let
      satisfied :: Array Manifest
      satisfied =
        [ unsafeManifest "prelude" "1.0.0" []
        , unsafeManifest "control" "1.0.0" [ Tuple "prelude" ">=1.0.0 <2.0.0" ]
        -- It is OK for the version bounds to not exist, although we may
        -- choose to make this more strict in the future.
        , unsafeManifest "control" "2.0.0" [ Tuple "prelude" ">=2.0.0 <3.0.0" ]
        ]

      unsatisfied :: Array Manifest
      unsatisfied =
        [ unsafeManifest "control" "3.0.0" [ Tuple "tuples" ">=2.0.0 <3.0.0" ]
        ]

    testIndex { satisfied, unsatisfied }

  Spec.it "Parses cyclical but acceptable index" do
    let
      satisfied :: Array Manifest
      satisfied =
        [ unsafeManifest "prelude" "1.0.0" []
        , unsafeManifest "prelude" "2.0.0" [ Tuple "control" ">=2.0.0 <3.0.0" ]
        , unsafeManifest "control" "1.0.0" [ Tuple "prelude" ">=1.0.0 <2.0.0" ]
        , unsafeManifest "control" "2.0.0" []
        ]

    testIndex { satisfied, unsatisfied: [] }

  Spec.it "Does not parse unacceptable cyclical index" do
    let
      unsatisfied :: Array Manifest
      unsatisfied =
        [ unsafeManifest "prelude" "1.0.0" [ Tuple "control" ">=1.0.0 <2.0.0" ]
        , unsafeManifest "control" "1.0.0" [ Tuple "prelude" ">=1.0.0 <2.0.0" ]
        ]

    testIndex { satisfied: [], unsatisfied }

contextEntry :: String
contextEntry =
  """{"name":"context","version":"0.0.1","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.2","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.3","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
"""

testIndex
  :: forall m
   . MonadThrow Error m
  => { satisfied :: Array Manifest, unsatisfied :: Array Manifest }
  -> m Unit
testIndex { satisfied, unsatisfied } = case ManifestIndex.maximalIndex (Set.fromFoldable (Array.fold [ satisfied, unsatisfied ])) of
  Tuple errors result -> do
    let
      { fail: shouldHaveErrors } =
        Utils.partitionEithers $ unsatisfied <#> \(Manifest m) ->
          case Map.lookup m.name errors >>= Map.lookup m.version of
            Nothing -> Left (Utils.formatPackageVersion m.name m.version)
            Just _ -> Right unit

      { fail: shouldHaveSuccess } =
        Utils.partitionEithers $ satisfied <#> \(Manifest m) ->
          case Map.lookup m.name errors >>= Map.lookup m.version of
            Nothing -> Right unit
            Just missing -> Left $ Array.fold
              [ Utils.formatPackageVersion m.name m.version
              , " could not satisfy "
              , Array.foldMap
                  ( \(Tuple name range) ->
                      PackageName.print name <> " (" <> Range.print range <> ")"
                  )
                  (Map.toUnfoldable missing)
              ]

    unless (Array.null shouldHaveErrors && Array.null shouldHaveSuccess) do
      Assert.fail $ String.joinWith "\n"
        [ Array.foldMap (append "\n  - Should not have satisfied: ") shouldHaveErrors
        , Array.foldMap (append "\n  - Should have satisfied: ") shouldHaveSuccess
        ]

    let expectedSize = Array.length (Array.nub satisfied)
    case List.foldl (+) 0 $ map Map.size $ Map.values $ ManifestIndex.toMap result of
      n | n == expectedSize -> pure unit
      n -> Assert.fail $ Array.fold
        [ "Index should have size "
        , Int.toStringAs Int.decimal expectedSize
        , " but has size "
        , Int.toStringAs Int.decimal n
        , ":\n"
        , formatIndex result
        ]

testSorted :: forall m. MonadThrow Error m => Array Manifest -> m Unit
testSorted input = do
  let sorted = ManifestIndex.topologicalSort ManifestIndex.IgnoreRanges (Set.fromFoldable input)
  unless (input == sorted) do
    Assert.fail $ String.joinWith "\n"
      [ JSON.printIndented $ CJ.encode (CJ.array manifestCodec') input
      , " is not equal to "
      , JSON.printIndented $ CJ.encode (CJ.array manifestCodec') sorted
      ]

formatInsertErrors :: Map PackageName Range -> String
formatInsertErrors errors = String.joinWith "\n"
  [ "Failed to insert. Failed to satisfy:"
  , JSON.printIndented $ CJ.encode (Internal.Codec.packageMap Range.codec) errors
  ]

formatIndex :: ManifestIndex -> String
formatIndex =
  JSON.printIndented
    <<< CJ.encode (Internal.Codec.packageMap (Internal.Codec.versionMap manifestCodec'))
    <<< ManifestIndex.toMap

manifestCodec' :: CJ.Codec Manifest
manifestCodec' = Profunctor.dimap to from $ CJ.named "ManifestRep" $ CJ.Record.object
  { name: PackageName.codec
  , version: Version.codec
  , dependencies: Internal.Codec.packageMap Range.codec
  }
  where
  to (Manifest { name, version, dependencies }) = { name, version, dependencies }
  from { name, version, dependencies } = Manifest
    { name
    , version
    , dependencies
    , license: Utils.unsafeLicense "MIT"
    , location: Git
        { url: "https://github.com/purescript/purescript-" <> PackageName.print name <> ".git"
        , subdir: Nothing
        }
    , description: Nothing
    , owners: Nothing
    , includeFiles: Nothing
    , excludeFiles: Nothing
    }
