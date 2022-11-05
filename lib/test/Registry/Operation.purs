module Test.Registry.Operation (spec) where

import Prelude

import Registry.Operation as Operation
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips package set update fixtures" do
    Assert.shouldRoundTrip "Package Set Update" Operation.packageSetUpdateCodec
      [ { label: "update-minimal", value: minimalPackageSetUpdate }
      , { label: "update-full", value: fullPackageSetUpdate }
      ]

  Spec.it "Round-trips publish fixtures" do
    Assert.shouldRoundTrip "Publish" Operation.publishCodec
      [ { label: "publish-minimal", value: minimalPublish }
      , { label: "publish-full", value: fullPublish }
      ]

  Spec.it "Round-trips authenticated (unpublish / transfer) fixtures" do
    Assert.shouldRoundTrip "Unpublish" Operation.authenticatedCodec
      [ { label: "unpublish", value: unpublish }
      , { label: "transfer", value: transfer }
      ]

minimalPackageSetUpdate :: String
minimalPackageSetUpdate =
  """
{
  "packages": {
    "aff": "9.0.0",
    "argonaut": "10.0.0"
  }
}"""

fullPackageSetUpdate :: String
fullPackageSetUpdate =
  """
{
  "compiler": "0.16.0",
  "packages": {
    "aff": null,
    "argonaut": "10.0.0"
  }
}"""

minimalPublish :: String
minimalPublish =
  """
{
  "compiler": "0.15.6",
  "name": "my-package",
  "ref": "v1.0.0"
}"""

fullPublish :: String
fullPublish =
  """
{
  "compiler": "0.15.6",
  "location": {
    "gitUrl": "https://github.com/purescript/purescript-prelude.git",
    "subdir": "core"
  },
  "name": "my-package",
  "ref": "c23snabhsrib39"
}"""

unpublish :: String
unpublish =
  """
{
  "email": "pacchettibotti@purescript.org",
  "payload": "{ \"name\": \"my-package\", \"version\": \"0.15.6\", \"reason\": \"Committed credentials.\" }",
  "signature": [
    "abc123",
    "abc123",
    "abc123"
  ]
}"""

transfer :: String
transfer =
  """
{
  "email": "pacchettibotti@purescript.org",
  "payload": "{ \"name\": \"my-package\", \"newLocation\": { \"githubOwner\": \"purescript-deprecated\", \"githubRepo\": \"purescript-my-package\" } }",
  "signature": [
    "abc123",
    "abc123",
    "abc123"
  ]
}"""
