module Test.Registry.Operation (spec) where

import Prelude

import Registry.Operation as Operation
import Registry.Test.Assert as Assert
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
  "ref": "v1.0.0",
  "version": "1.0.0"
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
  "ref": "c23snabhsrib39",
  "version": "1.0.0"
}"""

unpublish :: String
unpublish =
  """
{
  "payload": "{ \"name\": \"my-package\", \"version\": \"0.15.6\", \"reason\": \"Committed credentials.\" }",
  "signature": "1f4967eaa5de1076bb2185b818ea4fb7c18cfe83af951ab32c3bcb4a300dfe9b3795daaae1e7a6d5fb9f72c4cec8003f79a452f2dc9da9ec8cfa63b243c80503"
}"""

transfer :: String
transfer =
  """
{
  "payload": "{ \"name\": \"my-package\", \"newLocation\": { \"githubOwner\": \"purescript-deprecated\", \"githubRepo\": \"purescript-my-package\" } }",
  "signature": "1f4967eaa5de1076bb2185b818ea4fb7c18cfe83af951ab32c3bcb4a300dfe9b3795daaae1e7a6d5fb9f72c4cec8003f79a452f2dc9da9ec8cfa63b243c80503"
}"""
