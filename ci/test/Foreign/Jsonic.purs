module Test.Foreign.Jsonic (jsonic) where

import Registry.Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Codec.Argonaut (JsonDecodeError, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either as Either
import Foreign.Jsonic as Jsonic
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

parseString :: String -> Either JsonDecodeError String
parseString = map stringify <<< Jsonic.parseJson

parseTest :: String -> Json -> Spec
parseTest str json = Spec.it str do
  parseString str `Assert.shouldContain` stringify json

goodJson :: Spec
goodJson = do
  let
    nameTestCodec = CAR.object "goodJson.test" { name: CA.string }
    complexJsonCodec = CAR.object "goodJson"
      { complex: CAR.object "complex"
          { nested: CA.string
          , bool: CA.boolean
          }
      }
    complexJson = encode complexJsonCodec { complex: { nested: "json", bool: true } }
  parseTest "[1,2,3]" $ encode (CA.array CA.int) [ 1, 2, 3 ]
  parseTest """{ "name": "test" }""" $ encode nameTestCodec { name: "test" }
  parseTest (stringify complexJson) complexJson

badJson :: Spec
badJson = do
  let
    nameTestCodec = CAR.object "badJson.test" { name: CA.string }
    trailingCommaRecCodec = CAR.object "badJson.test" { trailing: CA.string }
  parseTest "name: test" $ encode nameTestCodec { name: "test" }
  parseTest "{trailing: comma,}" $ encode trailingCommaRecCodec { trailing: "comma" }
  parseTest """[ "trailing comma", ]""" $ encode (CA.array CA.string) [ "trailing comma" ]

horrendousJson :: Spec
horrendousJson = do
  let
    failParse str = Spec.it str do
      parseString str `Assert.shouldSatisfy` Either.isLeft

  failParse """{ "horrendously invalid json" }"""
