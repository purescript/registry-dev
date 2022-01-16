module Test.Foreign.Jsonic (jsonic) where

import Registry.Prelude

import Data.Either as Either
import Registry.Json as Json
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

type Spec = Spec.SpecT Aff Unit Identity Unit

jsonic :: Spec
jsonic = do
  Spec.describe "Parses" do
    Spec.describe "Good json" goodJson
    Spec.describe "Bad json" badJson
  Spec.describe "Does not parse" do
    Spec.describe "Horrendous json" horrendousJson

parseString :: String -> Either String String
parseString = map Json.stringify <<< Json.parseJson

parseTest :: String -> Json -> Spec
parseTest str json = Spec.it str do
  parseString str `Assert.shouldContain` Json.stringify json

goodJson :: Spec
goodJson = do
  let complexJson = Json.encode { complex: { nested: "json", bool: true } }
  parseTest "[1,2,3]" $ Json.encode [ 1, 2, 3 ]
  parseTest """{ "name": "test" }""" $ Json.encode { name: "test" }
  parseTest (Json.stringify complexJson) complexJson

badJson :: Spec
badJson = do
  parseTest "name: test" $ Json.encode { name: "test" }
  parseTest "{trailing: comma,}" $ Json.encode { trailing: "comma" }
  parseTest """[ "trailing comma", ]""" $ Json.encode [ "trailing comma" ]

horrendousJson :: Spec
horrendousJson = do
  let
    failParse str = Spec.it str do
      parseString str `Assert.shouldSatisfy` Either.isLeft

  failParse """{ "horrendously invalid json" }"""
