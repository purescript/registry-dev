module Registry.Scripts.ManifestRoundtrip where

import Registry.Prelude

import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Effect.Aff as Aff
import Effect.Aff as Exception
import Node.FS.Aff as FS
import Partial.Unsafe (unsafeCrashWith)
import Registry.Schema (Manifest)

main :: Effect Unit
main = Aff.launchAff_ do
  -- List all the example Manifests recursively
  let examplesDir = "../examples/"
  packages <- FS.readdir examplesDir
  for packages \package -> do
    let packageDir = examplesDir <> package
    manifests <- FS.readdir packageDir
    for manifests \manifestFile -> do
      let manifestPath = packageDir <> "/" <> manifestFile
      log $ "Roundrip check for " <> show manifestPath
      -- Now we read every manifest to our purescript type
      manifestStr <- FS.readTextFile UTF8 manifestPath
      case (Json.jsonParser manifestStr >>= (lmap Json.printJsonDecodeError <<< Json.decodeJson)) of
        Left err -> do
          error $ "Got error while parsing manifest"
          Aff.throwError $ Exception.error err
        Right (manifest :: Manifest) -> do
          -- And if that works, we then try to convert them back to JSON, and
          -- error out if any differ
          let newManifestStr = stringifyWithIndent 2 $ Json.encodeJson manifest
          when (manifestStr /= newManifestStr) do
            error "\nManifests don't roundtrip. Old manifest:"
            error manifestStr
            error "\n\nNew manifest:"
            error newManifestStr
            unsafeCrashWith "\n"
