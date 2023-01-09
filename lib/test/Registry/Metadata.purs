module Test.Registry.Metadata (spec) where

import Prelude

import Registry.Metadata as Metadata
import Registry.Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips metadata fixtures" do
    Assert.shouldRoundTrip "Metadata" Metadata.codec
      [ { label: "record-studio", value: recordStudio }
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
