module Registry.App.CLI.Licensee where

import Registry.App.Prelude

import Control.Parallel as Parallel
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Effect.Class.Console as Console
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.App.Json as Json
import Registry.Foreign.Tmp as Tmp
import Sunde as Process

-- | Attempt to detect the license associated with a set of provided files
detectFiles :: Array { name :: FilePath, contents :: String } -> Aff (Either String (Array String))
detectFiles files = do
  tmp <- liftEffect Tmp.mkTmpDir
  files # Parallel.parTraverse_ \{ name, contents } ->
    FS.writeTextFile UTF8 (Path.concat [ tmp, name ]) contents
  detect tmp

-- | Attempt to detect the license for the package in the given directory using
-- | the `licensee` CLI tool.
detect :: FilePath -> Aff (Either String (Array String))
detect directory = do
  let cmd = "licensee"
  let stdin = Nothing
  let args = [ "detect", "--json", directory ]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    -- Licensee will exit with `Normally 1` if it didn't parse any licenses,
    -- but we consider this valid Licensee output.
    NodeProcess.Normally n | n == 0 || n == 1 -> do
      let
        parse :: String -> Either String (Array String)
        parse str = map (map _.spdx_id <<< _.licenses) $ flip Json.parseJson str $ CA.Record.object "Licenses"
          { licenses: CA.array $ CA.Record.object "SPDXIds"
              { spdx_id: CA.string }
          }

      case parse result.stdout of
        Left error -> do
          Console.log "Licensee failed to decode output: "
          Console.log error
          Console.log "arising from the result: "
          Console.log result.stdout
          pure $ Left error
        Right out -> do
          -- A NOASSERTION result means that a LICENSE file could not be parsed.
          -- For the purposes of the registry we disregard this result, since
          -- we retrieve the license via the package manifest(s) as well.
          pure $ Right $ Array.filter (_ /= "NOASSERTION") out
    _ -> do
      Console.log $ "Licensee process exited unexpectedly: " <> result.stderr
      pure $ Left result.stderr
