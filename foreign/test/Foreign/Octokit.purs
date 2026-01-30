module Test.Registry.Foreign.Octokit (spec) where

import Prelude

import Data.Either (Either(..))
import Registry.Foreign.Octokit (Base64Content(..), decodeBase64Content)
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "decodeBase64Content" do
    Spec.it "Decodes simple ASCII content" do
      -- "hello world" in base64
      let content = Base64Content "aGVsbG8gd29ybGQ="
      decodeBase64Content content `Assert.shouldEqual` Right "hello world"

    Spec.it "Decodes content with embedded newlines" do
      -- "hello world" split across lines (GitHub API style)
      let content = Base64Content "aGVs\nbG8g\nd29ybGQ="
      decodeBase64Content content `Assert.shouldEqual` Right "hello world"

    Spec.it "Decodes UTF-8 with multi-byte characters" do
      -- "hello — world" (with em-dash) in base64
      let content = Base64Content "aGVsbG8g4oCUIHdvcmxk"
      decodeBase64Content content `Assert.shouldEqual` Right "hello — world"

    Spec.it "Decodes UTF-8 when multi-byte character is split across newline boundary" do
      -- This is the gesso bug: "PureScript — Compatible" where the em-dash's
      -- UTF-8 bytes (E2 80 94) get split by a newline in the base64 encoding.
      -- The base64 "IOGA\nlC" decodes to bytes [20 E1 80] + [94 ...] which is
      -- an incomplete UTF-8 sequence if decoded separately.
      --
      -- Real example from gesso@1.0.1 spago.yaml (truncated):
      -- "...in PureScript — Compatible..."
      let
        content = Base64Content
          "aW4gUHVyZVNjcmlwdCDigJQgQ29tcGF0aWJsZQ=="
      decodeBase64Content content `Assert.shouldEqual` Right "in PureScript — Compatible"

    Spec.it "Decodes the actual gesso content that was failing" do
      -- The exact base64 from gesso's spago.yaml cache that caused the failure.
      -- The newlines split the em-dash's UTF-8 bytes (E2 80 94) such that
      -- line 2 ends with "IOGA" which decodes to bytes [20 E2 80] - an incomplete
      -- UTF-8 sequence. Line 3 starts with "lC" which has the missing byte 94.
      -- The old code decoded each line separately, failing on the incomplete UTF-8.
      let
        content = Base64Content $
          "cGFja2FnZToKICBuYW1lOiBnZXNzbwogIGRlc2NyaXB0aW9uOiAiRWFzaWx5\n"
            <> "IGJ1aWxkIDxjYW52YXM+IGFwcGxpY2F0aW9ucyBpbiBQdXJlU2NyaXB0IOKA\n"
            <> "lCBDb21wYXRpYmxlIHdpdGggSGFsb2dlbiI="
      decodeBase64Content content `Assert.shouldEqual`
        Right "package:\n  name: gesso\n  description: \"Easily build <canvas> applications in PureScript \x2014 Compatible with Halogen\""
