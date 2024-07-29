module Registry.App.CLI.Licensee where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Parallel as Parallel
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Node.ChildProcess.Types (Exit(..))
import Node.FS.Aff as FS
import Node.Library.Execa as Execa
import Node.Path as Path
import Registry.Foreign.Tmp as Tmp

-- | Attempt to detect the license associated with a set of provided files
detectFiles :: Array { name :: FilePath, contents :: String } -> Aff (Either String (Array String))
detectFiles files = do
  tmp <- Tmp.mkTmpDir
  files # Parallel.parTraverse_ \{ name, contents } ->
    FS.writeTextFile UTF8 (Path.concat [ tmp, name ]) contents
  detect tmp

-- | Attempt to detect the license for the package in the given directory using
-- | the `licensee` CLI tool.
detect :: FilePath -> Aff (Either String (Array String))
detect directory = do
  let cmd = "licensee"
  let args = [ "detect", "--json", directory ]
  result <- _.getResult =<< Execa.execa cmd args identity
  pure case result.exit of
    -- Licensee will exit with `1` if it didn't parse any licenses,
    -- but we consider this valid Licensee output.
    Normally n | n == 0 || n == 1 -> do
      let
        parse :: String -> Either CJ.DecodeError (Array String)
        parse str = map (map _.spdx_id <<< _.licenses) $ flip parseJson str $ CJ.named "Licenses" $ CJ.Record.object
          { licenses: CJ.array $ CJ.named "SPDXIds" $ CJ.Record.object
              { spdx_id: CJ.string }
          }

      case parse result.stdout of
        Left error -> do
          let printedError = CJ.DecodeError.print error
          Left printedError
        Right out -> do
          -- A NOASSERTION result means that a LICENSE file could not be parsed.
          -- For the purposes of the registry we disregard this result, since
          -- we retrieve the license via the package manifest(s) as well.
          Right $ Array.filter (_ /= "NOASSERTION") out

    _ ->
      Left result.stderr
