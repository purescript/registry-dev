module Test.Foreign.Jsonic (jsonic) where

import Registry.Prelude

import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Either (isLeft)
import Foreign.Jsonic (parse)
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

parseS :: String -> Either String String
parseS s = Json.stringify <$> parse s

parseTest :: String -> Json -> Spec
parseTest str json = Spec.it str do
  parseS str `Assert.shouldContain` Json.stringify json

goodJson :: Spec
goodJson = do
  let
    complexJson = Json.encodeJson { complex: { nested: "json", bool: true } }

  parseTest "[1,2,3]" $ Json.encodeJson [ 1, 2, 3 ]
  parseTest "{\"name\": \"test\"}" $ Json.encodeJson { name: "test" }
  parseTest (Json.stringify complexJson) complexJson

badJson :: Spec
badJson = do
  parseTest "name: test" $ Json.encodeJson { name: "test" }
  parseTest "{trailing: comma,}" $ Json.encodeJson { trailing: "comma" }
  parseTest "[\"trailing comma\",]" $ Json.encodeJson [ "trailing comma" ]

horrendousJson :: Spec
horrendousJson = do
  let
    failParse str = Spec.it str do
      parseS str `Assert.shouldSatisfy` isLeft

  failParse "{ \"horrendously invalid json\" }"
