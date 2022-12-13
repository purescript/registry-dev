module Test.Registry.Foreign.JsonRepair (spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Either (Either)
import Data.Either as Either
import Registry.Foreign.JsonRepair as JsonRepair
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Valid JSON" do
    let arrayCodec = CA.array CA.int
    parseTest arrayCodec "[1,2,3]" $ CA.encode arrayCodec [ 1, 2, 3 ]

    let objectCodec = CA.Record.object "Test" { name: CA.string }
    parseTest objectCodec """{ "name": "test" }""" $ CA.encode objectCodec { name: "test" }

    let
      complexCodec = CA.Record.object "Complex" { complex: CA.Record.object "Nested" { nested: CA.string, bool: CA.boolean } }
      complexJson = { complex: { nested: "json", bool: true } }

    parseTest complexCodec (Argonaut.stringify $ CA.encode complexCodec complexJson) (CA.encode complexCodec complexJson)

  Spec.describe "Fixable JSON" do
    let testObjectCodec = CA.Record.object "Test" { trailing: CA.string }
    parseTest testObjectCodec """{ "trailing": "comma", }""" $ CA.encode testObjectCodec { trailing: "comma" }

    let testArrayCodec = CA.array CA.string
    parseTest testArrayCodec """[ "trailing comma", ]""" $ CA.encode testArrayCodec [ "trailing comma" ]

  Spec.describe "Unfixable JSON" do
    let
      failParse :: forall a. JsonCodec a -> String -> Spec.Spec Unit
      failParse codec str = Spec.it str do
        parseString codec str `Assert.shouldSatisfy` Either.isLeft

    failParse (CA.Record.object "Test" { name: CA.string }) "name: test"
    failParse (CA.Record.object "Test" { key: CA.string }) """{ "horrendously invalid json" }"""

parseString :: forall a. JsonCodec a -> String -> Either String String
parseString codec input = do
  parsed <- Argonaut.Parser.jsonParser (JsonRepair.tryRepair input)
  decoded <- lmap CA.printJsonDecodeError $ CA.decode codec parsed
  pure $ Argonaut.stringify $ CA.encode codec decoded

parseTest :: forall a. JsonCodec a -> String -> Json -> Spec.Spec Unit
parseTest codec str json = Spec.it str do
  parseString codec str `Assert.shouldContain` Argonaut.stringify json
