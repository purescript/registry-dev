module Foreign.Licensee where

import Registry.Prelude

import Control.Parallel as Parallel
import Data.Argonaut as Json
import Data.String as String
import Foreign.Tmp as Tmp
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS
import Sunde as Process

-- | Attempt to detect the license associated with a set of provided files
detectFiles :: Array { name :: FilePath, contents :: String } -> Aff (Either String (Array String))
detectFiles files = do
  tmp <- liftEffect Tmp.mkTmpDir
  files # Parallel.parTraverse_ \{ name, contents } ->
    FS.writeTextFile UTF8 (tmp <> "/" <> name) contents
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
        parse :: String -> Either Json.JsonDecodeError (Array String)
        parse str = Json.parseJson (String.trim str) >>= \json -> do
          obj <- Json.decodeJson json
          licenses <- obj `Json.getField` "licenses"
          spdxIds <- traverse (_ `Json.getField` "spdx_id") licenses
          pure spdxIds

      case parse result.stdout of
        Left err -> do
          let printed = Json.printJsonDecodeError err
          log "Licensee failed to decode output: "
          log printed
          log "arising from the result: "
          log result.stdout
          pure $ Left printed
        Right out ->
          pure $ Right out
    _ ->
      pure $ Left result.stderr
