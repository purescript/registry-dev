module TestPush where

import Registry.Prelude

import Data.Map as Map
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import GitHub (IssueNumber(..))
import Registry.RegistryM (comment, mkEnv, runRegistryM)

issueNumber :: IssueNumber
issueNumber = IssueNumber 149

main :: Effect Unit
main = launchAff_ $ runRegistryM (mkEnv (unsafePerformEffect $ Ref.new Map.empty) issueNumber) do
  comment "Test @purescript/packaging"