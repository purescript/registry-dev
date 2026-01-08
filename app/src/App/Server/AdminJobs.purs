-- | Execution of admin jobs (scheduled tasks and manual package set operations).
-- | The scheduled job implementations are stubbed for now - actual scripts will
-- | be plugged in later.
module Registry.App.Server.AdminJobs
  ( executeAdminJob
  ) where

import Registry.App.Prelude

import Registry.API.V1 (AdminJobType(..))
import Registry.API.V1 as V1
import Registry.App.API as API
import Registry.App.Effect.Log as Log
import Registry.App.Server.Env (ServerEffects)
import Run (Run)

-- | Execute an admin job based on its type. The scheduled job implementations
-- | (PackageTransfer, LegacyImport, PackageSetUpdate) are currently stubbed.
-- | Only AdminPackageSetOperation (manual API requests) is fully implemented.
executeAdminJob :: AdminJobType -> Run ServerEffects Unit
executeAdminJob = case _ of
  AdminPackageTransfer -> do
    Log.info "Running scheduled PackageTransfer job..."
    Log.warn "TODO: PackageTransfer execution not yet implemented"

  AdminLegacyImport mode -> do
    Log.info $ "Running scheduled LegacyImport job with mode: " <> V1.printLegacyImportMode mode
    Log.warn "TODO: LegacyImport execution not yet implemented"

  AdminPackageSetUpdate mode -> do
    Log.info $ "Running scheduled PackageSetUpdate job with mode: " <> V1.printPackageSetUpdateMode mode
    Log.warn "TODO: PackageSetUpdate execution not yet implemented"

  AdminPackageSetOperation operation -> do
    Log.info "Running manual package set operation from API..."
    API.packageSetUpdate operation
