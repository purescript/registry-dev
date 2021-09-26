module Foreign.Licensee where

import Registry.Prelude

import Data.Argonaut as Json
import Foreign.Jsonic as Jsonic
import Foreign.Tmp as Tmp
import Node.ChildProcess as NodeProcess
import Node.FS.Aff as FS
import Sunde as Process

type LicenseeOutput =
  { licenses :: Array DetectedLicense
  , matched_files :: Array MatchedFile
  }

type DetectedLicense =
  { key :: String
  , spdx_id :: String
  }

type MatchedFile =
  { filename :: String
  , matched_license :: String
  }

-- | Attempt to detect the license associated with a set of provided files
detectFiles :: Array { name :: FilePath, contents :: Json.Json } -> Aff (Either String LicenseeOutput)
detectFiles files = do
  tmp <- liftEffect Tmp.mkTmpDir
  for_ files \{ name, contents } ->
    writeJsonFile (tmp <> "/" <> name) contents
  result <- detect tmp
  FS.rmdir tmp
  pure result

-- | Attempt to detect the license for the package in the given directory using
-- | the `licensee` CLI tool.
detect :: FilePath -> Aff (Either String LicenseeOutput)
detect directory = do
  let cmd = "licensee"
  let stdin = Nothing
  let args = [ "detect", "--json", directory ]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  pure $ case result.exit of
    NodeProcess.Normally 0 -> lmap Json.printJsonDecodeError do
      json <- Jsonic.parseJson result.stdout
      Json.decodeJson json
    _ ->
      throwError result.stderr
