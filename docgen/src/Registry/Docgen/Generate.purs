module Registry.Docgen.Generate
  ( GenerationError(..)
  , ModuleInput
  , PackageInput
  , generatePackage
  , printGenerationError
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import PureScript.CST.Types as CST
import Registry.Docgen.Convert as Convert
import Registry.Docgen.Docs (DocModule(..), DocPackage(..), ModuleName, RawRange, Readme, SourceArtifact, schemaVersion)
import Registry.Docgen.Legacy.Docs as Legacy
import Registry.Docgen.Reexports (ReexportError)
import Registry.Docgen.Reexports as Reexports
import Registry.License (License)
import Registry.Location (Location)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Safe.Coerce (coerce)

type ModuleInput =
  { docs :: Legacy.DocModule
  , package :: PackageName
  , source :: CST.ModuleHeader Void
  , sourcePath :: String
  }

type PackageInput =
  { compilerVersion :: Version
  , dependencies :: Map PackageName RawRange
  , description :: Maybe String
  , license :: License
  , location :: Location
  , locationRef :: Maybe String
  , modules :: Array ModuleInput
  , name :: PackageName
  , readme :: Maybe Readme
  , resolvedDependencies :: Map PackageName Version
  , sourceArtifact :: SourceArtifact
  , version :: Version
  }

data GenerationError
  = MismatchedModuleNames ModuleName ModuleName
  | DuplicateModule ModuleName
  | InvalidSourcePath ModuleName String
  | UnknownModulePackage ModuleName PackageName
  | NoPackageModules PackageName
  | ReexportFailure ReexportError

printGenerationError :: GenerationError -> String
printGenerationError = case _ of
  MismatchedModuleNames docsName sourceName ->
    "Compiler docs for module " <> unwrap docsName <> " were paired with source module " <> unwrap sourceName
  DuplicateModule name ->
    "Received duplicate documentation inputs for module " <> unwrap name
  InvalidSourcePath name path ->
    "Module " <> unwrap name <> " has a source path that is not relative to the package tarball root: " <> show path
  UnknownModulePackage moduleName package ->
    "Module " <> unwrap moduleName <> " belongs to " <> PackageName.print package <> ", which is not in the exact package resolutions"
  NoPackageModules package ->
    "No documentation modules belong to package " <> PackageName.print package
  ReexportFailure error ->
    Reexports.printReexportError error

generatePackage :: PackageInput -> Either GenerationError DocPackage
generatePackage input = do
  let sortedInputs = Array.sortBy (comparing docsModuleName) input.modules
  _ <- foldM validateModule Set.empty sortedInputs
  let packageModules = Array.filter (_.package >>> eq input.name) sortedInputs
  if Array.null packageModules then
    Left $ NoPackageModules input.name
  else do
    let sourcePaths = Map.fromFoldable $ map (\moduleInput -> Tuple (docsModuleName moduleInput) moduleInput.sourcePath) sortedInputs
    let converted = map (Convert.fromLegacyModule sourcePaths <<< _.docs) sortedInputs
    resolved <- lmap ReexportFailure $ Reexports.modulesWithReexports converted (map _.source sortedInputs)
    let modulePackages = Map.fromFoldable $ map (\moduleInput -> Tuple (docsModuleName moduleInput) moduleInput.package) sortedInputs
    pure $ DocPackage
      { schemaVersion
      , compilerVersion: input.compilerVersion
      , sourceArtifact: input.sourceArtifact
      , dependencies: input.dependencies
      , description: input.description
      , license: input.license
      , location: input.location
      , locationRef: input.locationRef
      , modules: Array.filter (\(DocModule { name }) -> Map.lookup name modulePackages == Just input.name) resolved
      , name: input.name
      , readme: input.readme
      , resolvedDependencies: input.resolvedDependencies
      , resolvedModulePackages: modulePackages
      , version: input.version
      }
  where
  validateModule seen moduleInput = do
    let docsName = docsModuleName moduleInput
    let sourceName = sourceModuleName moduleInput.source
    if docsName /= sourceName then
      Left $ MismatchedModuleNames docsName sourceName
    else if Set.member docsName seen then
      Left $ DuplicateModule docsName
    else if not (isPackageRelativePath moduleInput.sourcePath) then
      Left $ InvalidSourcePath docsName moduleInput.sourcePath
    else if moduleInput.package /= input.name && not (Map.member moduleInput.package input.resolvedDependencies) then
      Left $ UnknownModulePackage docsName moduleInput.package
    else
      Right $ Set.insert docsName seen

docsModuleName :: ModuleInput -> ModuleName
docsModuleName { docs: Legacy.DocModule { name } } = name

sourceModuleName :: CST.ModuleHeader Void -> ModuleName
sourceModuleName (CST.ModuleHeader { name: CST.Name { name } }) = coerce name

isPackageRelativePath :: String -> Boolean
isPackageRelativePath path =
  not (String.null path)
    && String.take 1 path /= "/"
    && not (String.contains (Pattern "\\") path)
    && Array.all validPart (String.split (Pattern "/") path)
  where
  validPart part = not (String.null part) && part /= "." && part /= ".."
