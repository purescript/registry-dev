module Test.Registry.Manifest (spec) where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.String as String
import Data.Traversable (for)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.Manifest as Manifest
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec Unit
spec = do
  Spec.it "Round-trips manifest fixtures" do
    let manifestFixturesPath = Path.concat [ "test", "_fixtures", "manifests" ]
    fixtures <- FS.Aff.readdir manifestFixturesPath
    parsed <- for fixtures \fixture -> do
      let path = Path.concat [ manifestFixturesPath, fixture ]
      rawManifest <- String.trim <$> FS.Aff.readTextFile UTF8 path
      pure $ case lmap CA.printJsonDecodeError <<< CA.decode Manifest.codec =<< Argonaut.Parser.jsonParser rawManifest of
        Left error -> Left { fixture, error }
        Right result -> Right { rawManifest, fixture, result }

    let parsedResult = Utils.partitionEithers parsed
    let formatParseError { fixture, error } = fixture <> ": " <> error

    unless (Array.null parsedResult.fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed manifests were not parsed correctly:"
        , Array.foldMap (append "\n  - " <<< formatParseError) parsedResult.fail
        ]

    let
      roundtrip = parsedResult.success <#> \{ rawManifest, fixture, result } -> do
        let printed = Argonaut.stringifyWithIndent 2 $ CA.encode Manifest.codec result
        if rawManifest == printed then Right unit else Left { rawManifest, fixture, printed }

      roundtripResult = Utils.partitionEithers roundtrip

      formatRoundtripError { rawManifest, fixture, printed } =
        String.joinWith "\n"
          [ fixture <> " input does not match output."
          , String.joinWith "\n" [ rawManifest, "/=", printed ]
          ]

    unless (Array.null roundtripResult.fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed manifests did not round-trip:"
        , Array.foldMap (append "\n  - " <<< formatRoundtripError) roundtripResult.fail
        ]
