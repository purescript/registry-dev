module Test.Registry.Foreign.JsonRepair (spec) where

import Registry.App.Prelude

import Data.Codec.Argonaut as CA
import Data.Either as Either
import Registry.App.Json (Json, JsonCodec)
import Registry.App.Json as Json
import Registry.Foreign.JsonRepair as JsonRepair
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Valid JSON" do
    let arrayCodec = CA.array CA.int
    parseTest arrayCodec "[1,2,3]" $ Json.encode arrayCodec [ 1, 2, 3 ]

    let objectCodec = Json.object "Test" { name: CA.string }
    parseTest objectCodec """{ "name": "test" }""" $ Json.encode objectCodec { name: "test" }

    let
      complexCodec = Json.object "Complex" { complex: Json.object "Nested" { nested: CA.string, bool: CA.boolean } }
      complexJson = { complex: { nested: "json", bool: true } }

    parseTest complexCodec (Json.stringifyJson complexCodec complexJson) (Json.encode complexCodec complexJson)

  Spec.describe "Fixable JSON" do
    let testObjectCodec = Json.object "Test" { trailing: CA.string }
    parseTest testObjectCodec """{ "trailing": "comma", }""" $ Json.encode testObjectCodec { trailing: "comma" }

    let testArrayCodec = CA.array CA.string
    parseTest testArrayCodec """[ "trailing comma", ]""" $ Json.encode testArrayCodec [ "trailing comma" ]

  Spec.describe "Unfixable JSON" do
    let
      failParse :: forall a. JsonCodec a -> String -> Spec.Spec Unit
      failParse codec str = Spec.it str do
        parseString codec str `Assert.shouldSatisfy` Either.isLeft

    failParse (Json.object "Test" { name: CA.string }) "name: test"
    failParse (Json.object "Test" { key: CA.string }) """{ "horrendously invalid json" }"""

parseString :: forall a. JsonCodec a -> String -> Either String String
parseString codec = map (Json.stringifyJson codec) <<< Json.parseJson codec <<< JsonRepair.tryRepair

parseTest :: forall a. JsonCodec a -> String -> Json -> Spec.Spec Unit
parseTest codec str json = Spec.it str do
  parseString codec str `Assert.shouldContain` Json.stringify json
