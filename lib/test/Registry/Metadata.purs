module Test.Registry.Metadata where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Registry.Metadata as Metadata
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec Unit
spec = do
  Spec.it "Round-trips metadata fixtures" do
    let
      fixtures =
        [ Tuple "record-studio" recordStudio
        ]

      parse (Tuple name input) =
        case lmap CA.printJsonDecodeError <<< CA.decode Metadata.codec =<< Argonaut.Parser.jsonParser input of
          Left error -> Left { name, input, error }
          Right result -> Right { name, input, result }

      { fail, success } = Utils.partitionEithers $ map parse fixtures

      formatError { name, error } = name <> ": " <> error

    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed metadata strings were not parsed correctly:"
        , Array.foldMap (append "\n  - " <<< formatError) fail
        ]

    let
      roundtrip = success <#> \{ name, input, result } -> do
        let printed = Argonaut.stringifyWithIndent 2 $ CA.encode Metadata.codec result
        if String.trim input == String.trim printed then Right unit else Left { name, input, printed }

      roundtripResult = Utils.partitionEithers roundtrip

      formatRoundtripError { name, input, printed } =
        String.joinWith "\n"
          [ name <> " input does not match output."
          , String.joinWith "\n" [ input, "/=", printed ]
          ]

    unless (Array.null roundtripResult.fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed manifests did not round-trip:"
        , Array.foldMap (append "\n  - " <<< formatRoundtripError) roundtripResult.fail
        ]

recordStudio :: String
recordStudio =
  """
{
  "location": {
    "githubOwner": "rowtype-yoga",
    "githubRepo": "purescript-record-studio"
  },
  "published": {
    "0.1.0": {
      "bytes": 3438,
      "hash": "sha256-LPRUC8ozZc7VCeRhKa4CtSgAfNqgAoVs2lH+7mYEcTk=",
      "publishedTime": "2021-03-27T10:03:46.000Z",
      "ref": "v0.1.0"
    },
    "0.2.1": {
      "bytes": 3365,
      "hash": "sha256-ySKKKp3rUJa4UmYTZshaOMO3jE+DW7IIqKJsurA2PP8=",
      "publishedTime": "2022-05-15T10:51:57.000Z",
      "ref": "v0.2.1"
    },
    "1.0.0": {
      "bytes": 5155,
      "hash": "sha256-0iMF8Rq88QBGuxTNrh+iuruw8l5boCP6J2JWBpQ4b7w=",
      "publishedTime": "2022-11-03T17:30:28.000Z",
      "ref": "v1.0.0"
    },
    "1.0.1": {
      "bytes": 5635,
      "hash": "sha256-Xm9pwDBHW5zYUEzxfVSgjglIcwRI1gcCOmcpyQ/tqeY=",
      "publishedTime": "2022-11-04T12:21:09.000Z",
      "ref": "v1.0.1"
    }
  },
  "unpublished": {
    "1.0.2": {
      "publishedTime": "2022-11-04T12:21:00.000Z",
      "reason": "Committed credentials.",
      "unpublishedTime": "2022-11-04T12:21:00.000Z"
    }
  }
}"""
