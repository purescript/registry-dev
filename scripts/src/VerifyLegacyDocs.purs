-- | Verify that historical Pursuit package artifacts still decode with the
-- | legacy codec used by registry-docgen.
-- |
-- | Run against a pursuit-backups checkout with:
-- |
-- |   nix run .#verify-legacy-docs -- /path/to/pursuit-backups
module Registry.Scripts.VerifyLegacyDocs where

import Registry.App.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.Array as Array
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console as Console
import JSON as JSON
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
import Node.Path as Path
import Node.Process as Process
import Registry.Docgen.Legacy.JSON as Legacy.JSON

main :: Effect Unit
main = launchAff_ do
  roots <- Array.drop 2 <$> liftEffect Process.argv
  when (Array.null roots) do
    Console.error "Expected at least one pursuit-backups file or directory."
    liftEffect $ Process.exit' 1

  paths <- Array.sort <<< Array.concat <$> traverse jsonFiles roots
  let skipped = Array.filter shouldSkip paths
  let candidates = Array.filter (not <<< shouldSkip) paths
  results <- for candidates \path -> do
    contents <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
    pure $ case contents of
      Left error -> Left { path, error: Aff.message error }
      Right source -> case JSON.parse source of
        Left error -> Left { path, error }
        Right json -> case Legacy.JSON.decodeDocPackage json of
          Left error -> Left { path, error: DecodeError.print error }
          Right _ -> Right unit

  let failures = Array.mapMaybe hushFailure results
  Console.log $ "Decoded " <> show (Array.length candidates - Array.length failures) <> " of " <> show (Array.length candidates) <> " supported historical documentation artifacts."
  unless (Array.null skipped) do
    Console.log $ "Skipped " <> show (Array.length skipped) <> " known incompatible historical documentation artifacts."
  unless (Array.null failures) do
    for_ failures \{ path, error } ->
      Console.error $ path <> ": " <> String.replaceAll (String.Pattern "\n") (String.Replacement "\n  ") error
    liftEffect $ Process.exit' 1
  where
  hushFailure = case _ of
    Left failure -> Just failure
    Right _ -> Nothing

-- These artifacts contain package names, dependency names, versions, or
-- licenses which cannot be represented by the registry's canonical types.
-- They were intentionally omitted when migrating the historical Pursuit
-- corpus. Keeping the allowlist exact ensures newly incompatible artifacts
-- still fail verification rather than being silently dropped.
skippedArtifacts :: Array String
skippedArtifacts =
  [ "purescript-clappr/0.6.0.json"
  , "purescript-clappr/0.6.1.json"
  , "purescript-concur-react/0.1.0.json"
  , "purescript-dotenv/0.3.0.json"
  , "purescript-email-validate/5.0.0.json"
  , "purescript-generic-graphviz/1.0.1.json"
  , "purescript-generic-graphviz/1.0.2.json"
  , "purescript-generic-graphviz/1.1.1.json"
  , "purescript-generic-graphviz/2.0.0.json"
  , "purescript-generic-graphviz/2.0.1.json"
  , "purescript-graphql/1.0.0-rc.json"
  , "purescript-graphql/1.0.0-rc1.json"
  , "purescript-graphql/1.0.0-rc2.json"
  , "purescript-graphviz/1.0.0.json"
  , "purescript-graphviz/1.1.1.json"
  , "purescript-graphviz/1.2.0.json"
  , "purescript-graphviz/1.3.0.json"
  , "purescript-milkis/7.0.1.json"
  , "purescript-purescript-node-jwt/1.0.0.json"
  , "purescript-purescript-openapi3/1.0.0.json"
  , "purescript-pux-redux/0.2.0.json"
  , "purescript-pux-redux/0.2.1.json"
  , "purescript-pux-redux/0.2.2.json"
  , "purescript-querydsl/0.7.1.json"
  , "purescript-querydsl/0.7.2.json"
  , "purescript-querydsl/0.8.0.json"
  , "purescript-specular/0.13.1.json"
  ]

shouldSkip :: FilePath -> Boolean
shouldSkip path = Array.elem artifact skippedArtifacts
  where
  artifact = Path.basename (Path.dirname path) <> "/" <> Path.basename path

jsonFiles :: FilePath -> Aff (Array FilePath)
jsonFiles path = do
  stats <- FS.Aff.stat path
  if FS.Stats.isDirectory stats then do
    children <- FS.Aff.readdir path
    Array.concat <$> traverse (jsonFiles <<< Path.concat <<< Array.cons path <<< pure) children
  else
    pure if Path.extname path == ".json" then [ path ] else []
