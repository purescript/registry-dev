module Registry.App.CLI.PursVersions where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.String as String
import Node.Library.Execa as Execa
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

pursVersions :: forall r. Run (EXCEPT String + AFF + r) (NonEmptyArray Version)
pursVersions = do
  result <- Run.liftAff $ _.result =<< Execa.execa "purs-versions" [] identity
  { stdout } <- Except.rethrow $ lmap (\{ stdout, stderr } -> stdout <> stderr) result
  let { fail, success } = partitionEithers $ map Version.parse (String.split (String.Pattern " ") stdout)

  when (Array.length fail > 0) do
    Except.throw (String.joinWith ", " fail)

  case NEA.fromArray success of
    Nothing -> Except.throw "No purs versions"
    Just arr -> pure arr
