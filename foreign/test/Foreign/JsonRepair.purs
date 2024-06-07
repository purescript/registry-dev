module Test.Registry.Foreign.JsonRepair (spec) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Bifunctor (lmap)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Either (Either)
import Data.Either as Either
import JSON (JSON)
import JSON as JSON
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Valid JSON" do
    let arrayCodec = CJ.array CJ.int
    parseTest arrayCodec "[1,2,3]" $ CJ.encode arrayCodec [ 1, 2, 3 ]

    let objectCodec = CJ.named "Test" $ CJ.Record.object { name: CJ.string }
    parseTest objectCodec """{ "name": "test" }""" $ CJ.encode objectCodec { name: "test" }

    let
      complexCodec = CJ.named "Complex" $ CJ.Record.object { complex: CJ.Record.object { nested: CJ.string, bool: CJ.boolean } }
      complexJson = { complex: { nested: "json", bool: true } }

    parseTest complexCodec (JSON.print $ CJ.encode complexCodec complexJson) (CJ.encode complexCodec complexJson)

  Spec.describe "Fixable JSON" do
    let testObjectCodec = CJ.named "Test" $ CJ.Record.object { trailing: CJ.string }
    parseTest testObjectCodec """{ "trailing": "comma", }""" $ CJ.encode testObjectCodec { trailing: "comma" }

    let testArrayCodec = CJ.array CJ.string
    parseTest testArrayCodec """[ "trailing comma", ]""" $ CJ.encode testArrayCodec [ "trailing comma" ]

  Spec.describe "Unfixable JSON" do
    let
      failParse :: forall a. CJ.Codec a -> String -> Spec.Spec Unit
      failParse codec str = Spec.it str do
        parseString codec str `Assert.shouldSatisfy` Either.isLeft

    failParse (CJ.named "Test" $ CJ.Record.object { name: CJ.string }) "name: test"
    failParse (CJ.named "Test" $ CJ.Record.object { key: CJ.string }) """{ "horrendously invalid json" }"""

parseString :: forall a. CJ.Codec a -> String -> Either String String
parseString codec input = do
  parsed <- JSON.parse (JsonRepair.tryRepair input)
  decoded <- lmap CJ.DecodeError.print $ CJ.decode codec parsed
  pure $ JSON.print $ CJ.encode codec decoded

parseTest :: forall a. CJ.Codec a -> String -> JSON -> Spec.Spec Unit
parseTest codec str json = Spec.it str do
  parseString codec str `Assert.shouldContain` JSON.print json
