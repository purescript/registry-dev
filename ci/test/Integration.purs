module Test.Integration where

import Registry.Prelude

import Control.Alternative (guard)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Exception as Exception
import Foreign.Git as Git
import Foreign.Tmp as Tmp
import Node.Path as Path
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location(..), Manifest(..))
import Registry.Solver as Solver
import Registry.Version (Version)
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ do
  index <- setup
  let
    depsOnly :: Solver.Dependencies
    depsOnly = map (map (\(Manifest m) -> m.dependencies)) index

    chunkSize :: Int
    chunkSize = 250

    chunk :: Array _ -> Array (Array _)
    chunk elems = case Array.take chunkSize elems of
      newElems | Array.length newElems == chunkSize -> Array.cons newElems (chunk (Array.drop chunkSize elems))
      newElems -> [ newElems ]

    -- Adjust this range to narrow down to a particular segment of packages. We
    -- have to chunk things in order to preserve stack safety.
    indices :: Array Int
    indices = Array.range 0 1 -- 45

    -- Adjust this filter to include specific packages
    shouldInclude :: Manifest -> Boolean
    shouldInclude (Manifest { location }) = case location of
      GitHub { owner } | owner == "purescript" -> true
      _ -> false

    flatIndex :: Array (Array Manifest)
    flatIndex = chunk do
      Tuple _ versions <- Map.toUnfoldableUnordered index
      Tuple _ manifest <- Map.toUnfoldableUnordered versions
      guard (shouldInclude manifest)
      pure manifest

  runSpec' defaultConfig [ consoleReporter ] do
    void $ for indices \ix -> do
      Spec.describe ("Solves packages from " <> show (ix * chunkSize) <> " to " <> show (ix * chunkSize + chunkSize) <> " of registry index") do
        for_ (Array.index flatIndex ix) \piece -> for piece \(Manifest { name, version, dependencies }) -> do
          Spec.it (Array.fold [ PackageName.print name, "@", Version.printVersion version ]) do
            case Solver.solve depsOnly dependencies of
              Left err ->
                unless (Set.member (Tuple name version) knownFailures) do
                  Assert.fail $ PackageName.print name <> "@" <> Version.printVersion version <> "\n" <> (String.joinWith "\n" $ NEA.toArray $ map Solver.printSolverError err)
              Right _ ->
                when (Set.member (Tuple name version) knownFailures) do
                  Assert.fail "Unexpected failure! Should have succeeded."
  where
  setup :: Aff RegistryIndex
  setup = do
    tmp <- liftEffect Tmp.mkTmpDir
    Except.runExceptT (Git.runGit_ [ "clone", "https://github.com/purescript/registry-index", "--depth", "1" ] (Just tmp)) >>= case _ of
      Left err -> throwError (Exception.error err)
      Right _ -> pure unit
    Index.readRegistryIndex (Path.concat [ tmp, "registry-index" ])

knownFailures :: Set (Tuple PackageName Version)
knownFailures = Set.fromFoldable $ map unsafeParse
  [ "aff-coroutines" /\ "4.0.0"

  , "halogen-formless" /\ "4.0.0"

  , "logging-journald" /\ "0.4.0"

  , "ps-cst" /\ "1.2.0"

  , "stac" /\ "1.0.0"

  , "telegraf" /\ "0.5.0"
  , "test-unit" /\ "10.0.1"
  , "trout" /\ "0.11.0"

  , "uint-instances" /\ "0.0.0"
  , "uint" /\ "4.1.0"

  , "virtual-dom-typed" /\ "0.1.0"
  , "virtual-dom-typed" /\ "0.0.2"
  , "vom" /\ "0.1.0"

  , "webgl" /\ "1.1.0"
  , "webgl" /\ "1.1.1"
  , "webgl" /\ "1.1.2"
  , "webgl" /\ "1.1.3"
  , "webgl" /\ "1.2.0"
  , "webworkers" /\ "0.1.2"
  , "webworkers" /\ "0.1.1"

  , "zipperarray" /\ "1.0.0"
  , "zipperarray" /\ "1.0.1"
  , "zipperarray" /\ "1.1.0"
  , "z85" /\ "0.0.2"
  ]
  where
  unsafeParse (Tuple p v) = unsafeFromRight (PackageName.parse p) /\ unsafeFromRight (Version.parseVersion Version.Strict v)
