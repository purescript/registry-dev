module Registry.Docgen.Reexports
  ( modulesWithReexports
  , ReexportError(..)
  , printReexportError
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, foldl)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import PureScript.CST.Types as CST
import Registry.Docgen.Docs (DataConstructorName(..), DocChildDeclaration(..), DocChildDeclarationInfo(..), DocConstraint(..), DocDeclaration(..), DocDeclarationInfo(..), DocModule(..), DocReexport(..), DocType(..), ForallBinding(..), InfixAlias(..), ModuleName, OperatorName(..), Qualified(..), TypeName(..), TypeVar(..), ValueName(..), isPrim)
import Safe.Coerce (coerce)

data RefSet a = RefAll | RefSet a | RefHiding a

instance Semigroup (RefSet ModuleMemberSet) where
  append = case _, _ of
    RefSet a, RefSet b -> RefSet (a <> b)
    RefHiding hidden, RefSet selected -> RefHiding (differenceMemberSet hidden selected)
    RefSet selected, RefHiding hidden -> RefHiding (differenceMemberSet hidden selected)
    RefHiding a, RefHiding b -> RefHiding (intersectMemberSet a b)
    RefAll, _ -> RefAll
    _, RefAll -> RefAll

newtype ModuleMemberSet = ModuleMemberSet
  { operators :: Set OperatorName
  , typeClasses :: Set TypeName
  , typeOperators :: Set OperatorName
  , types :: Map TypeName (RefSet (Set DataConstructorName))
  , values :: Set ValueName
  }

instance Semigroup ModuleMemberSet where
  append (ModuleMemberSet a) (ModuleMemberSet b) = ModuleMemberSet
    { operators: a.operators <> b.operators
    , typeClasses: a.typeClasses <> b.typeClasses
    , typeOperators: a.typeOperators <> b.typeOperators
    , types: Map.unionWith mergeConstructorRefs a.types b.types
    , values: a.values <> b.values
    }

mergeConstructorRefs :: RefSet (Set DataConstructorName) -> RefSet (Set DataConstructorName) -> RefSet (Set DataConstructorName)
mergeConstructorRefs = case _, _ of
  RefSet a, RefSet b -> RefSet (a <> b)
  RefAll, _ -> RefAll
  _, RefAll -> RefAll
  RefHiding a, RefHiding b -> RefHiding $ Set.intersection a b
  RefHiding hidden, RefSet selected -> RefHiding $ Set.difference hidden selected
  RefSet selected, RefHiding hidden -> RefHiding $ Set.difference hidden selected

differenceConstructorRefs
  :: RefSet (Set DataConstructorName)
  -> RefSet (Set DataConstructorName)
  -> Maybe (RefSet (Set DataConstructorName))
differenceConstructorRefs = case _, _ of
  RefSet a, RefSet b -> fromSet $ Set.difference a b
  RefSet _, RefAll -> Nothing
  RefSet a, RefHiding b -> fromSet $ Set.intersection a b
  RefAll, RefSet b -> Just $ RefHiding b
  RefAll, RefAll -> Nothing
  RefAll, RefHiding b -> fromSet b
  RefHiding a, RefSet b -> Just $ RefHiding $ a <> b
  RefHiding _, RefAll -> Nothing
  RefHiding a, RefHiding b -> fromSet $ Set.difference b a
  where
  fromSet set
    | Set.isEmpty set = Nothing
    | otherwise = Just $ RefSet set

instance Monoid ModuleMemberSet where
  mempty = ModuleMemberSet
    { operators: mempty
    , typeClasses: mempty
    , typeOperators: mempty
    , types: Map.empty
    , values: mempty
    }

differenceMemberSet :: ModuleMemberSet -> ModuleMemberSet -> ModuleMemberSet
differenceMemberSet (ModuleMemberSet a) (ModuleMemberSet b) = ModuleMemberSet
  { operators: Set.difference a.operators b.operators
  , typeClasses: Set.difference a.typeClasses b.typeClasses
  , typeOperators: Set.difference a.typeOperators b.typeOperators
  , types: foldl subtractType a.types (Map.toUnfoldable b.types :: Array (Tuple TypeName (RefSet (Set DataConstructorName))))
  , values: Set.difference a.values b.values
  }
  where
  subtractType types (Tuple name selected) = case Map.lookup name types >>= flip differenceConstructorRefs selected of
    Nothing -> Map.delete name types
    Just remaining -> Map.insert name remaining types

intersectMemberSet :: ModuleMemberSet -> ModuleMemberSet -> ModuleMemberSet
intersectMemberSet (ModuleMemberSet a) (ModuleMemberSet b) = ModuleMemberSet
  { operators: Set.intersection a.operators b.operators
  , typeClasses: Set.intersection a.typeClasses b.typeClasses
  , typeOperators: Set.intersection a.typeOperators b.typeOperators
  , types: Map.intersectionWith intersectConstructors a.types b.types
  , values: Set.intersection a.values b.values
  }
  where
  intersectConstructors = case _, _ of
    RefAll, refs -> refs
    refs, RefAll -> refs
    RefSet x, RefSet y -> RefSet $ Set.intersection x y
    RefHiding x, RefHiding y -> RefHiding $ x <> y
    RefHiding hidden, RefSet selected -> RefSet $ Set.difference selected hidden
    RefSet selected, RefHiding hidden -> RefSet $ Set.difference selected hidden

type ImportModuleSet = SemigroupMap ModuleName (RefSet ModuleMemberSet)

data ReexportError
  = MissingSourceHeaders (NonEmptyArray ModuleName)
  | MissingDocsTarget ModuleName ModuleName
  | ReexportCycle (NonEmptyArray ModuleName)
  | TransitivelyBlocked ModuleName ModuleName ReexportError

printReexportError :: ReexportError -> String
printReexportError = case _ of
  MissingSourceHeaders names ->
    "Missing parsed source headers for documentation modules: " <> printChain names
  MissingDocsTarget owner dependency ->
    "Module " <> printName owner <> " reexports missing documentation module " <> printName dependency
  ReexportCycle names ->
    "Reexport cycle: " <> printChain names
  TransitivelyBlocked owner dependency cause ->
    "Module " <> printName owner <> " is blocked by reexport " <> printName dependency <> ": " <> printReexportError cause
  where
  printName = coerce
  printChain = NonEmptyArray.toArray >>> map printName >>> Array.intercalate " -> "

modulesWithReexports :: Array DocModule -> Array (CST.ModuleHeader Void) -> Either ReexportError (Array DocModule)
modulesWithReexports allDocs allSourceModules = do
  case NonEmptyArray.fromArray missingHeaders of
    Just missing -> Left $ MissingSourceHeaders missing
    Nothing -> traverse (resolve []) allDocs
  where
  sourceModulesByName = Map.fromFoldable $ map
    ( \(sourceModule@(CST.ModuleHeader { name: CST.Name { name } })) ->
        Tuple (coerce name) sourceModule
    )
    allSourceModules

  docsByName = Map.fromFoldable $ map (\doc@(DocModule { name }) -> Tuple name doc) allDocs

  missingHeaders = Array.mapMaybe
    (\(DocModule { name }) -> if isPrim name || Map.member name sourceModulesByName then Nothing else Just name)
    allDocs

  importsFor name
    | isPrim name = Map.empty
    | otherwise = maybe Map.empty (Map.delete name <<< reexportsOf) $ Map.lookup name sourceModulesByName

  resolve chain (DocModule docs)
    | Array.elem docs.name chain =
        Left $ ReexportCycle $ NonEmptyArray.cons' docs.name (Array.drop 1 (Array.dropWhile (_ /= docs.name) chain) <> [ docs.name ])
    | otherwise =
        do
          reexports <- traverse resolveDependency $ Map.toUnfoldable $ importsFor docs.name
          pure $ DocModule docs { reexports = mergeReexports $ Array.concat reexports }
        where
        resolveDependency (Tuple dependency refs) = case Map.lookup dependency docsByName of
          Nothing -> Left $ MissingDocsTarget docs.name dependency
          Just dependencyDocs -> case resolve (Array.snoc chain docs.name) dependencyDocs of
            Left err@(ReexportCycle _) -> Left err
            Left err -> Left $ TransitivelyBlocked docs.name dependency err
            Right resolved -> Right $ map
              (\(Tuple moduleName declarations) -> DocReexport { moduleName, declarations: NonEmptyArray.toArray declarations })
              (matchingExportsOf refs resolved)

matchingExportsOf :: RefSet ModuleMemberSet -> DocModule -> Array (Tuple ModuleName (NonEmptyArray DocDeclaration))
matchingExportsOf refSet (DocModule { declarations, name, reexports }) =
  groupByModuleName case refSet of
    RefAll -> allDecls
    RefSet ms -> traverse (declInMemberSet ms) =<< allDecls
    RefHiding hidden -> bindFlipped (\(Tuple moduleName decl) -> map (Tuple moduleName) $ declNotInMemberSet hidden decl) allDecls
  where
  allDecls =
    map (Tuple name) declarations <>
      bindFlipped (\(DocReexport re) -> map (Tuple re.moduleName) re.declarations) reexports

  groupByModuleName =
    Array.groupAllBy (comparing fst) >>> map \decls -> do
      let (Tuple moduleName _) = NonEmptyArray.head decls
      Tuple moduleName (snd <$> decls)

declInMemberSet :: ModuleMemberSet -> DocDeclaration -> Array DocDeclaration
declInMemberSet (ModuleMemberSet members) decl@(DocDeclaration { children, info, comments, sourceSpan }) = case info of
  DeclValue { name }
    | Set.member (unqualify name) members.values ->
        pure decl
  DeclData { name }
    | Just ctorsRef <- Map.lookup (unqualify name) members.types ->
        pure $ DocDeclaration
          { children: case ctorsRef of
              RefAll -> children
              RefSet ctors ->
                Array.filter
                  case _ of
                    DocChildDeclaration { info: ChildDeclConstructor { name: ctorName } } ->
                      Set.member (unqualify ctorName) ctors
                    _ ->
                      true
                  children
              RefHiding ctors ->
                Array.filter
                  case _ of
                    DocChildDeclaration { info: ChildDeclConstructor { name: ctorName } } ->
                      not $ Set.member (unqualify ctorName) ctors
                    _ ->
                      true
                  children
          , info
          , comments
          , sourceSpan
          }
  DeclForeignData { name }
    | Map.member (unqualify name) members.types ->
        pure decl
  DeclType { name }
    | Map.member (unqualify name) members.types ->
        pure decl
  DeclTypeClass { name, vars }
    | Set.member (unqualify name) members.typeClasses ->
        pure $ DocDeclaration
          { children: Array.filter
              case _ of
                DocChildDeclaration { info: ChildDeclTypeClassMember { name: childName } } ->
                  Set.member (unqualify childName) members.values
                _ ->
                  true
              children
          , info
          , comments
          , sourceSpan
          }
    | otherwise ->
        Array.mapMaybe
          case _ of
            DocChildDeclaration
              { comments: childComments
              , info: ChildDeclTypeClassMember { name: childName, signature }
              , sourceSpan: childSourceSpan
              }
              | Set.member (unqualify childName) members.values ->
                  Just $ DocDeclaration
                    { children: []
                    , info: DeclValue
                        { name: childName
                        , signature: promotedMemberSignature name vars signature
                        }
                    , comments: childComments
                    , sourceSpan: childSourceSpan
                    }
            _ ->
              Nothing
          children
  DeclInfix { name, alias: AliasConstructor _ }
    | Set.member (unqualify name) members.operators ->
        pure decl
  DeclInfix { name, alias: AliasValue _ }
    | Set.member (unqualify name) members.operators ->
        pure decl
  DeclInfix { name, alias: AliasType _ }
    | Set.member (unqualify name) members.typeOperators ->
        pure decl
  _ ->
    []
  where
  unqualify :: forall a. Qualified a -> a
  unqualify (Qualified { name }) = name

declNotInMemberSet :: ModuleMemberSet -> DocDeclaration -> Array DocDeclaration
declNotInMemberSet (ModuleMemberSet members) decl@(DocDeclaration declaration@{ children, info }) = case info of
  DeclData { name } -> case Map.lookup (unqualify name) members.types of
    Nothing -> pure decl
    Just RefAll -> []
    Just (RefSet hidden)
      | Set.isEmpty hidden -> []
      | otherwise -> pure $ DocDeclaration declaration { children = filterConstructors (not <<< flip Set.member hidden) children }
    Just (RefHiding visible) ->
      pure $ DocDeclaration declaration { children = filterConstructors (flip Set.member visible) children }
  DeclTypeClass { name }
    | Set.member (unqualify name) members.typeClasses -> []
    | otherwise -> pure $ DocDeclaration declaration
        { children = Array.filter
            case _ of
              DocChildDeclaration { info: ChildDeclTypeClassMember { name: childName } } ->
                not $ Set.member (unqualify childName) members.values
              _ -> true
            declaration.children
        }
  _
    | Array.null $ declInMemberSet (ModuleMemberSet members) decl -> pure decl
    | otherwise -> []
  where
  filterConstructors keep = Array.filter case _ of
    DocChildDeclaration { info: ChildDeclConstructor { name } } -> keep $ unqualify name
    _ -> true

  unqualify :: forall a. Qualified a -> a
  unqualify (Qualified { name }) = name

promotedMemberSignature :: Qualified TypeName -> Array TypeVar -> DocType -> DocType
promotedMemberSignature className vars signature =
  maybe constrained (\bindings -> TypeForall { bindings: map toBinding bindings, body: constrained }) $ NonEmptyArray.fromArray vars
  where
  constrained = TypeConstrained
    { constraint: DocConstraint
        { args: map (\(TypeVar { ident }) -> TypeIdent ident) vars
        , name: className
        }
    , result: signature
    }
  toBinding (TypeVar { ident, signature: kind }) = ForallBinding { isVisible: false, name: ident, signature: kind }

mergeReexports :: Array DocReexport -> Array DocReexport
mergeReexports = foldl insert []
  where
  insert acc (DocReexport next) = case Array.findIndex (\(DocReexport re) -> re.moduleName == next.moduleName) acc of
    Nothing -> Array.snoc acc (DocReexport next)
    Just index -> fromMaybe acc $ Array.modifyAt index
      (\(DocReexport re) -> DocReexport re { declarations = foldl appendUnique re.declarations next.declarations })
      acc

  appendUnique declarations declaration
    | Array.any (sameDeclaration declaration) declarations = declarations
    | otherwise = Array.snoc declarations declaration

  sameDeclaration a b = declarationKey a == declarationKey b

  declarationKey (DocDeclaration { info }) = case info of
    DeclValue { name } -> Tuple "value" (qualifiedValueName name)
    DeclData { name } -> Tuple "type" (qualifiedTypeName name)
    DeclForeignData { name } -> Tuple "type" (qualifiedTypeName name)
    DeclType { name } -> Tuple "type" (qualifiedTypeName name)
    DeclTypeClass { name } -> Tuple "type" (qualifiedTypeName name)
    DeclInfix { name, alias: AliasType _ } -> Tuple "type" (qualifiedOperatorName name)
    DeclInfix { name } -> Tuple "value" (qualifiedOperatorName name)

  qualifiedValueName (Qualified { moduleName, name }) = (coerce moduleName :: String) <> "." <> (coerce name :: String)
  qualifiedTypeName (Qualified { moduleName, name }) = (coerce moduleName :: String) <> "." <> (coerce name :: String)
  qualifiedOperatorName (Qualified { moduleName, name }) = (coerce moduleName :: String) <> "." <> (coerce name :: String)

reexportsOf :: CST.ModuleHeader Void -> Map ModuleName (RefSet ModuleMemberSet)
reexportsOf sourceModule@(CST.ModuleHeader { exports }) =
  un SemigroupMap $ fold $ Map.filterWithKey (\k _ -> Set.member k moduleKeys) importSet
  where
  SemigroupMap importSet = importSetOf sourceModule
  moduleKeys = foldMap (foldMap moduleKeyOf <<< delimitedNonEmptyToArray) exports
  moduleKeyOf = case _ of
    CST.ExportModule _ (CST.Name { name }) ->
      Set.singleton (coerce name)
    _ ->
      mempty

importSetOf :: CST.ModuleHeader Void -> SemigroupMap ModuleName ImportModuleSet
importSetOf (CST.ModuleHeader { imports }) =
  foldMap fromImportDecl imports
  where
  ModuleMemberSet initial = mempty

  fromImportDecl (CST.ImportDecl { module: CST.Name { name: importModule }, names, qualified }) =
    SemigroupMap
      $ Map.singleton qualifiedModule
      $ SemigroupMap
      $ Map.singleton (coerce importModule) memberSet
    where
    qualifiedModule = case qualified of
      Just (Tuple _ (CST.Name { name })) ->
        coerce name
      Nothing ->
        coerce importModule

    memberSet = case names of
      Just (Tuple hiding importList) -> case hiding of
        Just _ -> RefHiding members
        Nothing -> RefSet members
        where
        members = foldMap fromImport $ delimitedNonEmptyToArray importList
      Nothing ->
        RefAll

  fromImport = ModuleMemberSet <<< case _ of
    CST.ImportValue (CST.Name { name }) ->
      initial { values = Set.singleton (coerce name) }
    CST.ImportOp (CST.Name { name }) ->
      initial { operators = Set.singleton (coerce name) }
    CST.ImportType (CST.Name { name }) members ->
      initial { types = Map.singleton (coerce name) $ fromDataMembers members }
    CST.ImportTypeOp _ (CST.Name { name }) ->
      initial { typeOperators = Set.singleton (coerce name) }
    CST.ImportClass _ (CST.Name { name }) ->
      initial { typeClasses = Set.singleton (coerce name) }
    CST.ImportError _ ->
      initial

  fromDataMembers = case _ of
    Just (CST.DataAll _) ->
      RefAll
    Just (CST.DataEnumerated names) ->
      RefSet
        $ Set.fromFoldable
        $ map (\(CST.Name { name }) -> coerce name)
        $ delimitedToArray names
    Nothing ->
      RefSet mempty

delimitedNonEmptyToArray :: forall a. CST.DelimitedNonEmpty a -> Array a
delimitedNonEmptyToArray (CST.Wrapped { value }) = separatedToArray value

delimitedToArray :: forall a. CST.Delimited a -> Array a
delimitedToArray (CST.Wrapped { value }) = foldMap separatedToArray value

separatedToArray :: forall a. CST.Separated a -> Array a
separatedToArray (CST.Separated { head, tail }) = Array.cons head $ snd <$> tail
