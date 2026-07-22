-- | Report the latest registry versions that are newer than their versions in
-- | the latest package set.
-- |
-- | Packages that have never appeared in a package set are intentionally not
-- | included; discovering those packages is tracked as separate follow-up work.
-- |
-- | Run via Nix:
-- |   nix run .#version-check
module Registry.Scripts.VersionCheck where

import Registry.App.Prelude

import Data.Map as Map
import Effect.Class.Console as Console
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY_READ)
import Registry.App.Effect.Registry as Registry
import Registry.PackageName as PackageName
import Registry.Scripts.PackageSetUpdater as PackageSetUpdater
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

main :: Effect Unit
main = launchAff_ do
  registryCacheRef <- Cache.newCacheRef
  debouncer <- Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { jobId: Nothing
      , pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  result <- runVersionCheck
    # Except.runExcept
    # Registry.interpretRead (Registry.handleRead registryEnv)
    # Log.interpret (Log.handleTerminal Normal)
    # Run.runBaseAff'

  case result of
    Left err -> do
      Console.error $ "Error: " <> err
      liftEffect $ Process.exit' 1
    Right _ -> pure unit

type VersionCheckEffects = (REGISTRY_READ + LOG + EXCEPT String + AFF + EFFECT + ())

runVersionCheck :: Run VersionCheckEffects (Map PackageName Version)
runVersionCheck = do
  Log.info "Registry Version Check: checking for pending package set updates..."
  Registry.readLatestPackageSet >>= case _ of
    Nothing -> do
      Log.warn "No package set found, skipping version check."
      pure Map.empty
    Just packageSet -> do
      pending <- PackageSetUpdater.findPackageSetCandidates PackageSetUpdater.AllPending packageSet
      if Map.isEmpty pending then
        Log.info "No package versions are pending a package set update."
      else do
        Log.info $ "Found " <> show (Map.size pending) <> " package versions pending a package set update:"
        for_ (Map.toUnfoldable pending :: Array _) \(Tuple name version) ->
          Log.info $ "  - " <> PackageName.print name <> "@" <> Version.print version
      pure pending
