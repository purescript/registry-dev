module Test.Registry.Metadata (spec) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import JSON as JSON
import Registry.Metadata as Metadata
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips metadata fixtures" do
    Assert.shouldRoundTrip "Metadata" Metadata.codec
      [ { label: "record-studio", value: recordStudio }
      ]

  Spec.describe "PublishedMetadata ref field (deprecated)" do
    Spec.it "Decodes metadata without ref field" do
      let json = Utils.fromRight "Failed to parse JSON" $ JSON.parse publishedWithoutRef
      case CJ.decode Metadata.publishedMetadataCodec json of
        Left err -> Assert.fail $ "Failed to decode: " <> CJ.DecodeError.print err
        Right meta -> meta.ref `Assert.shouldEqual` Nothing

    Spec.it "Decodes metadata with ref field" do
      let json = Utils.fromRight "Failed to parse JSON" $ JSON.parse publishedWithRef
      case CJ.decode Metadata.publishedMetadataCodec json of
        Left err -> Assert.fail $ "Failed to decode: " <> CJ.DecodeError.print err
        Right meta -> meta.ref `Assert.shouldEqual` (Just "v1.0.0")

    Spec.it "Encodes Nothing as missing field (no ref in output)" do
      let json = Utils.fromRight "Failed to parse JSON" $ JSON.parse publishedWithoutRef
      case CJ.decode Metadata.publishedMetadataCodec json of
        Left err -> Assert.fail $ "Failed to decode: " <> CJ.DecodeError.print err
        Right meta -> do
          let encoded = CJ.encode Metadata.publishedMetadataCodec meta
          let jsonStr = JSON.printIndented encoded
          -- Nothing should result in the field being omitted
          String.contains (String.Pattern "\"ref\"") jsonStr `Assert.shouldEqual` false

    Spec.it "Encodes Just \"\" as present field" do
      let json = Utils.fromRight "Failed to parse JSON" $ JSON.parse publishedWithRef
      case CJ.decode Metadata.publishedMetadataCodec json of
        Left err -> Assert.fail $ "Failed to decode: " <> CJ.DecodeError.print err
        Right meta -> do
          let withEmptyRef = meta { ref = Just "" }
          let encoded = CJ.encode Metadata.publishedMetadataCodec withEmptyRef
          let jsonStr = JSON.printIndented encoded
          -- Just "" should result in the field being present (with "ref": "" pattern)
          String.contains (String.Pattern "\"ref\": \"\"") jsonStr `Assert.shouldEqual` true

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
      "compilers": [
        "0.13.0"
      ],
      "hash": "sha256-LPRUC8ozZc7VCeRhKa4CtSgAfNqgAoVs2lH+7mYEcTk=",
      "publishedTime": "2021-03-27T10:03:46.000Z"
    },
    "0.2.1": {
      "bytes": 3365,
      "compilers": [
        "0.13.0"
      ],
      "hash": "sha256-ySKKKp3rUJa4UmYTZshaOMO3jE+DW7IIqKJsurA2PP8=",
      "publishedTime": "2022-05-15T10:51:57.000Z"
    },
    "1.0.0": {
      "bytes": 5155,
      "compilers": [
        "0.13.0"
      ],
      "hash": "sha256-0iMF8Rq88QBGuxTNrh+iuruw8l5boCP6J2JWBpQ4b7w=",
      "publishedTime": "2022-11-03T17:30:28.000Z"
    },
    "1.0.1": {
      "bytes": 5635,
      "compilers": [
        "0.13.0",
        "0.13.1"
      ],
      "hash": "sha256-Xm9pwDBHW5zYUEzxfVSgjglIcwRI1gcCOmcpyQ/tqeY=",
      "publishedTime": "2022-11-04T12:21:09.000Z"
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

-- PublishedMetadata without ref field (should decode with ref = Nothing)
publishedWithoutRef :: String
publishedWithoutRef =
  """
{
  "bytes": 3438,
  "compilers": ["0.15.10"],
  "hash": "sha256-LPRUC8ozZc7VCeRhKa4CtSgAfNqgAoVs2lH+7mYEcTk=",
  "publishedTime": "2021-03-27T10:03:46.000Z"
}"""

-- PublishedMetadata with ref field (should decode with ref = Just "v1.0.0")
publishedWithRef :: String
publishedWithRef =
  """
{
  "bytes": 3438,
  "compilers": ["0.15.10"],
  "hash": "sha256-LPRUC8ozZc7VCeRhKa4CtSgAfNqgAoVs2lH+7mYEcTk=",
  "publishedTime": "2021-03-27T10:03:46.000Z",
  "ref": "v1.0.0"
}"""
