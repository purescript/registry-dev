module Test.Registry.PackageSet (spec) where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Registry.PackageSet as PackageSet
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec Unit
spec = do
  Spec.it "Round-trips package set fixtures" do
    let
      fixtures =
        [ Tuple "0.0.1 package set" initialPackageSet
        ]

      parse (Tuple name input) =
        case lmap CA.printJsonDecodeError <<< CA.decode PackageSet.codec =<< Argonaut.Parser.jsonParser input of
          Left error -> Left { name, input, error }
          Right result -> Right { name, input, result }

      { fail, success } = Utils.partitionEithers $ map parse fixtures

      formatError { name, error } = name <> ": " <> error

    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed package set strings were not parsed correctly:"
        , Array.foldMap (append "\n  - " <<< formatError) fail
        ]

    let
      roundtrip = success <#> \{ name, input, result } -> do
        let printed = Argonaut.stringifyWithIndent 2 $ CA.encode PackageSet.codec result
        if String.trim input == String.trim printed then Right unit else Left { name, input, printed }

      roundtripResult = Utils.partitionEithers roundtrip

      formatRoundtripError { name, input, printed } =
        String.joinWith "\n"
          [ name <> " input does not match output."
          , String.joinWith "\n" [ input, "/=", printed ]
          ]

    unless (Array.null roundtripResult.fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed manifests did not round-trip:"
        , Array.foldMap (append "\n  - " <<< formatRoundtripError) roundtripResult.fail
        ]

-- A trimmed version of the first package set release
initialPackageSet :: String
initialPackageSet =
  """
{
  "version": "0.0.1",
  "compiler": "0.15.4",
  "published": "2022-09-24",
  "packages": {
    "ace": "9.0.0",
    "aff": "7.1.0",
    "aff-bus": "6.0.0",
    "aff-coroutines": "9.0.0",
    "aff-promise": "4.0.0",
    "aff-retry": "2.0.0",
    "affjax": "13.0.0",
    "affjax-node": "1.0.0",
    "affjax-web": "1.0.0",
    "ansi": "7.0.0",
    "argonaut": "9.0.0",
    "argonaut-codecs": "9.1.0",
    "argonaut-core": "7.0.0",
    "argonaut-generic": "8.0.0",
    "argonaut-traversals": "10.0.0",
    "argparse-basic": "2.0.0"
  }
}
"""
