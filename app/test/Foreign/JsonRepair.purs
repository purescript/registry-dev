module Test.Foreign.JsonRepair (testJsonRepair) where

import Registry.App.Prelude

import Data.Codec.Argonaut as CA
import Data.Either as Either
import Foreign.JsonRepair as JsonRepair
import Registry.App.Json (Json, JsonCodec)
import Registry.App.Json as Json
import Test.Spec as Spec
import Test.Utils.Assert as Assert

type Spec = Spec.SpecT Aff Unit Identity Unit

testJsonRepair :: Spec
testJsonRepair = do
  Spec.describe "Parses" do
    Spec.describe "Good json" goodJson
    Spec.describe "Bad json" badJson
  Spec.describe "Does not parse" do
    Spec.describe "Horrendous json" horrendousJson

parseString :: forall a. JsonCodec a -> String -> Either String String
parseString codec = map (Json.stringifyJson codec) <<< Json.parseJson codec <<< JsonRepair.tryRepair

parseTest :: forall a. JsonCodec a -> String -> Json -> Spec
parseTest codec str json = Spec.it str do
  parseString codec str `Assert.shouldContain` Json.stringify json

goodJson :: Spec
goodJson = do
  let arrayCodec = CA.array CA.int
  parseTest arrayCodec "[1,2,3]" $ Json.encode arrayCodec [ 1, 2, 3 ]

  let objectCodec = Json.object "Test" { name: CA.string }
  parseTest objectCodec """{ "name": "test" }""" $ Json.encode objectCodec { name: "test" }

  let
    complexCodec = Json.object "Complex" { complex: Json.object "Nested" { nested: CA.string, bool: CA.boolean } }
    complexJson = { complex: { nested: "json", bool: true } }

  parseTest complexCodec (Json.stringifyJson complexCodec complexJson) (Json.encode complexCodec complexJson)

badJson :: Spec
badJson = do
  let testObjectCodec = Json.object "Test" { trailing: CA.string }
  parseTest testObjectCodec """{ "trailing": "comma", }""" $ Json.encode testObjectCodec { trailing: "comma" }

  let testArrayCodec = CA.array CA.string
  parseTest testArrayCodec """[ "trailing comma", ]""" $ Json.encode testArrayCodec [ "trailing comma" ]

horrendousJson :: Spec
horrendousJson = do
  let
    failParse :: forall a. JsonCodec a -> String -> Spec
    failParse codec str = Spec.it str do
      parseString codec str `Assert.shouldSatisfy` Either.isLeft

  failParse (Json.object "Test" { name: CA.string }) "name: test"
  failParse (Json.object "Test" { key: CA.string }) """{ "horrendously invalid json" }"""
