module Registry.App.CLI.PursVersions where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.String as String
import Node.ChildProcess.Types (Exit(..))
import Node.Library.Execa as Execa
import Registry.Version as Version
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- | Returns a sorted array of PureScript compilers supported by the Registry
pursVersions :: forall r. Run (EXCEPT String + AFF + r) (NonEmptyArray Version)
pursVersions = do
  result <- Run.liftAff $ _.getResult =<< Execa.execa "purs-versions" [] identity
  case result.exit of
    Normally 0 -> pure unit
    _ -> Except.throw $ result.stdout <> result.stderr
  let { fail, success } = partitionEithers $ map Version.parse (String.split (String.Pattern " ") result.stdout)

  when (Array.length fail > 0) do
    Except.throw (String.joinWith ", " fail)

  case NEA.fromArray success of
    Nothing -> Except.throw "No purs versions"
    Just arr -> pure $ NEA.sort arr
