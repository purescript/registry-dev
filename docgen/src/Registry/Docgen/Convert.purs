module Registry.Docgen.Convert
  ( fromLegacyPackage
  , fromLegacyModule
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Traversable (traverse)
import Registry.Docgen.Docs (DataConstructorName(..), DocChildDeclaration(..), DocChildDeclarationInfo(..), DocConstraint(..), DocDeclaration(..), DocDeclarationInfo(..), DocModule(..), DocPackage(..), DocReexport(..), DocType(..), ForallBinding(..), FunDep(..), Ident(..), InfixAlias(..), IntLiteral(..), ModuleName(..), OperatorName(..), Qualified(..), Readme, RowLabel(..), RowRep, SourceArtifact, SourceSpan(..), StringLiteral, TypeName(..), TypeVar(..), ValueName(..), isPrim, schemaVersion)
import Registry.Docgen.Legacy.Docs (InPackage(..))
import Registry.Docgen.Legacy.Docs as L
import Registry.Location (Location(..))
import Registry.Version as Version
import Safe.Coerce (coerce)

fromLegacyPackage :: Map ModuleName String -> SourceArtifact -> Maybe Readme -> L.DocPackage -> Either String DocPackage
fromLegacyPackage sourcePaths sourceArtifact readme (L.DocPackage pkg@{ github: L.GithubData github, packageMeta: L.DocPackageMeta meta }) = do
  compilerVersion <- Version.parse pkg.compilerVersion
  modules <- traverse convertModule pkg.modules
  pure $ DocPackage
    { schemaVersion
    , compilerVersion
    , sourceArtifact
    , dependencies: meta.dependencies
    , description: meta.description
    , license: meta.license
    , location
    , locationRef: Just pkg.versionTag
    , name: coerce meta.name
    , modules
    , readme
    , resolvedDependencies: pkg.resolvedDependencies
    , resolvedModulePackages: pkg.moduleMap
    , version: pkg.version
    }
  where
  convertModule legacy@(L.DocModule { name })
    | isPrim name = Right $ fromLegacyModule sourcePaths legacy
    | otherwise = case Map.lookup name sourcePaths of
        Nothing -> Left $ "Missing package-relative source path for module " <> unwrap name
        Just _ -> Right $ fromLegacyModule sourcePaths legacy

  location :: Location
  location = GitHub
    { owner: github.user
    , repo: github.repo
    , subdir: Nothing
    }

fromLegacyModule :: Map ModuleName String -> L.DocModule -> DocModule
fromLegacyModule sourcePaths (L.DocModule mod@{ name: ownerModule }) =
  DocModule
    { comments: mod.comments
    , declarations: fromLegacyDeclaration ownerModule <$> mod.declarations
    , name: mod.name
    , reexports: fromLegacyReExport <$> mod.reExports
    }
  where
  fromLegacyReExport :: L.ReExport -> DocReexport
  fromLegacyReExport (L.ReExport { declarations, moduleName: InPackage { item: moduleName } }) =
    DocReexport
      { declarations: fromLegacyDeclaration moduleName <$> declarations
      , moduleName
      }

  fromLegacyDeclaration :: ModuleName -> L.Declaration -> DocDeclaration
  fromLegacyDeclaration declarationModule (L.Declaration decl) =
    DocDeclaration
      { children: fromLegacyChildDeclaration declarationModule <$> decl.children
      , info: fromLegacyDeclarationInfo declarationModule decl.title (fromLegacyType declarationModule <<< _.kind <<< unwrap <$> decl.kindInfo) decl.info
      , comments: decl.comments
      , sourceSpan: replaceSourcePath <$> Map.lookup declarationModule sourcePaths <*> decl.sourceSpan
      }

  fromLegacyDeclarationInfo :: ModuleName -> String -> Maybe DocType -> L.DeclarationInfo -> DocDeclarationInfo
  fromLegacyDeclarationInfo currentModule title signature = case _ of
    L.ValueDeclaration ty ->
      DeclValue
        { name: Qualified { moduleName: currentModule, name: ValueName title }
        , signature: fromLegacyType currentModule ty
        }
    L.DataDeclaration declType tyVars roles ->
      DeclData
        { isNewtype: case declType of
            L.Newtype -> true
            L.Data -> false
        , name: Qualified { moduleName: currentModule, name: TypeName title }
        , roles
        , signature
        , vars: fromLegacyTypeVar currentModule <$> tyVars
        }
    L.TypeSynonymDeclaration tyVars body ->
      DeclType
        { body: fromLegacyType currentModule body
        , name: Qualified { moduleName: currentModule, name: TypeName title }
        , signature
        , vars: fromLegacyTypeVar currentModule <$> tyVars
        }
    L.TypeClassDeclaration tyVars cons funDeps ->
      DeclTypeClass
        { funDeps: fromLegacyFunDep <$> funDeps
        , name: Qualified { moduleName: currentModule, name: TypeName title }
        , signature
        , superClasses: fromLegacyConstraint currentModule <$> cons
        , vars: fromLegacyTypeVar currentModule <$> tyVars
        }
    L.AliasDeclaration (L.Fixity { associativity, precedence }) qual@(L.Qualified _ alias) ->
      DeclInfix
        { alias: case alias of
            Left ty ->
              AliasType $ fromLegacyQualified currentModule (qual $> ty)
            Right (Left ident) ->
              AliasValue $ fromLegacyQualified currentModule (qual $> coerce ident)
            Right (Right ctor) ->
              AliasConstructor $ fromLegacyQualified currentModule (qual $> ctor)
        , associativity
        , name: Qualified { moduleName: currentModule, name: unsafeOperatorNameFromTitle title }
        , precedence
        }
    L.ExternDataDeclaration ty roles ->
      DeclForeignData
        { name: Qualified { moduleName: currentModule, name: TypeName title }
        , roles
        , signature: fromLegacyType currentModule ty
        }

  fromLegacyChildDeclaration :: ModuleName -> L.ChildDeclaration -> DocChildDeclaration
  fromLegacyChildDeclaration currentModule (L.ChildDeclaration decl) =
    DocChildDeclaration
      { comments: decl.comments
      , info: fromLegacyChildDeclarationInfo currentModule decl.title decl.info
      , sourceSpan: replaceSourcePath <$> Map.lookup currentModule sourcePaths <*> decl.sourceSpan
      }

  fromLegacyChildDeclarationInfo :: ModuleName -> String -> L.ChildDeclarationInfo -> DocChildDeclarationInfo
  fromLegacyChildDeclarationInfo currentModule title = case _ of
    L.ChildInstance cons ty ->
      ChildDeclInstance
        { constraints: fromLegacyConstraint currentModule <$> cons
        , head: fromLegacyType currentModule ty
        , name: Qualified { moduleName: currentModule, name: ValueName title }
        }
    L.ChildDataConstructor tys ->
      ChildDeclConstructor
        { args: fromLegacyTypePrec currentModule PrecAtom <$> tys
        , name: Qualified { moduleName: currentModule, name: DataConstructorName title }
        }
    L.ChildTypeClassMember ty ->
      ChildDeclTypeClassMember
        { signature: fromLegacyType currentModule ty
        , name: Qualified { moduleName: currentModule, name: ValueName title }
        }

  fromLegacyConstraint :: ModuleName -> L.DocConstraint -> DocConstraint
  fromLegacyConstraint currentModule (L.DocConstraint name _ args) =
    DocConstraint
      { args: fromLegacyTypePrec currentModule PrecAtom <$> args
      , name: fromLegacyQualified currentModule name
      }

  fromLegacyQualified :: forall a. ModuleName -> L.Qualified a -> Qualified a
  fromLegacyQualified currentModule (L.Qualified qb name) = case qb of
    L.ByModuleName moduleName ->
      Qualified { moduleName, name }
    L.BySourcePos _ ->
      Qualified { moduleName: currentModule, name }

  fromLegacyTypeVar :: ModuleName -> L.DocTypeVar -> TypeVar
  fromLegacyTypeVar currentModule (L.DocTypeVar ident signature) =
    TypeVar
      { ident
      , signature: fromLegacyType currentModule <$> signature
      }

  fromLegacyFunDep :: L.Fundep -> FunDep
  fromLegacyFunDep (L.Fundep determiners determinees) =
    FunDep
      { determiners
      , determinees
      }

  fromLegacyTypePrec :: ModuleName -> TypePrec -> L.DocType -> DocType
  fromLegacyTypePrec currentModule prec ty = do
    let ty' = fromLegacyType currentModule ty
    if precOfType ty' <= prec then
      ty'
    else
      TypeParens ty'

  fromLegacyType :: ModuleName -> L.DocType -> DocType
  fromLegacyType currentModule = case _ of
    L.TypeVar ident ->
      TypeIdent $ Ident ident
    L.TypeLevelString str ->
      TypeString str
    L.TypeLevelInt int ->
      TypeInt $ IntSmall int
    L.TypeWildcard _ ->
      TypeWildcard
    L.TypeConstructor name ->
      TypeConstructor $ fromLegacyQualified currentModule name
    L.TypeOp name ->
      TypeOperator $ fromLegacyQualified currentModule name
    L.TypeApp ty1 ty2
      | Just arg <- isFunction ty1 ->
          TypeFunction
            { arg: fromLegacyTypePrec currentModule PrecApp arg
            , result: fromLegacyTypePrec currentModule PrecArrow ty2
            }
      | isRecord ty1
      , L.RCons label ty3 ty4 <- ty2 ->
          TypeRecord $ toRowRep currentModule [ toRowLabel currentModule label ty3 ] ty4
      | otherwise ->
          TypeApp
            { arg: fromLegacyTypePrec currentModule PrecApp ty2
            , function: fromLegacyTypePrec currentModule PrecApp ty1
            }
    L.KindApp ty1 ty2 ->
      TypeKindApp
        { arg: fromLegacyTypePrec currentModule PrecApp ty2
        , function: fromLegacyTypePrec currentModule PrecApp ty1
        }
    L.ForAll vis ident sig ty ->
      TypeForall $ go (NonEmptyArray.singleton (toForallBinding vis ident sig)) ty
      where
      toForallBinding vis' ident' sig' =
        ForallBinding
          { isVisible: case vis' of
              L.TypeVarVisible -> true
              L.TypeVarInvisible -> false
          , name: Ident ident'
          , signature: fromLegacyType currentModule <$> sig'
          }
      go bindings = case _ of
        L.ForAll vis' ident' sig' ty' ->
          go (NonEmptyArray.snoc bindings (toForallBinding vis' ident' sig')) ty'
        body ->
          { bindings, body: fromLegacyType currentModule body }
    L.ConstrainedType con ty ->
      TypeConstrained
        { constraint: fromLegacyConstraint currentModule con
        , result: fromLegacyTypePrec currentModule PrecArrow ty
        }
    L.KindedType ty1 ty2 ->
      TypeKindSignature
        { signature: fromLegacyTypePrec currentModule PrecArrow ty2
        , term: fromLegacyType currentModule ty1
        }
    L.BinaryNoParensType ty1 ty2 ty3 ->
      TypeInfixApp
        { argLhs: fromLegacyTypePrec currentModule PrecApp ty2
        , argRhs: fromLegacyTypePrec currentModule PrecApp ty3
        , operator: fromLegacyType currentModule ty1
        }
    L.ParensInType ty ->
      TypeParens $ fromLegacyType currentModule ty
    L.RCons label ty1 ty2 ->
      TypeRow $ toRowRep currentModule [ toRowLabel currentModule label ty1 ] ty2
    L.REmpty ->
      TypeRow { labels: [], tail: Nothing }

  toRowLabel :: ModuleName -> StringLiteral -> L.DocType -> RowLabel
  toRowLabel currentModule label sig =
    RowLabel
      { label
      , signature: fromLegacyType currentModule sig
      }

  toRowRep :: ModuleName -> Array RowLabel -> L.DocType -> RowRep
  toRowRep currentModule labels = case _ of
    L.RCons label ty1 ty2 ->
      toRowRep currentModule (Array.snoc labels (toRowLabel currentModule label ty1)) ty2
    L.REmpty ->
      { labels, tail: Nothing }
    tail ->
      { labels, tail: Just $ fromLegacyType currentModule tail }

unsafeOperatorNameFromTitle :: String -> OperatorName
unsafeOperatorNameFromTitle title =
  OperatorName $ fromMaybe title $ valueOperator <|> typeOperator
  where
  valueOperator = do
    String.stripPrefix (String.Pattern "(") title
      >>= String.stripSuffix (String.Pattern ")")

  typeOperator = do
    String.stripPrefix (String.Pattern "type (") title
      >>= String.stripSuffix (String.Pattern ")")

replaceSourcePath :: String -> SourceSpan -> SourceSpan
replaceSourcePath path (SourceSpan span) = SourceSpan $ span { path = path }

data TypePrec
  = PrecBottom
  | PrecAtom
  | PrecApp
  | PrecInfix
  | PrecArrow
  | PrecKinded
  | PrecTop

derive instance Eq TypePrec
derive instance Ord TypePrec

precOfType :: DocType -> TypePrec
precOfType = case _ of
  TypeApp _ -> PrecApp
  TypeKindApp _ -> PrecApp
  TypeInfixApp _ -> PrecInfix
  TypeForall _ -> PrecArrow
  TypeConstrained _ -> PrecArrow
  TypeFunction _ -> PrecArrow
  TypeKindSignature _ -> PrecKinded
  _ -> PrecAtom

isFunction :: L.DocType -> Maybe L.DocType
isFunction = case _ of
  L.TypeApp (L.TypeConstructor (L.Qualified (L.ByModuleName (ModuleName "Prim")) (TypeName "Function"))) arg ->
    Just arg
  _ ->
    Nothing

isRecord :: L.DocType -> Boolean
isRecord = case _ of
  L.TypeConstructor (L.Qualified (L.ByModuleName (ModuleName "Prim")) (TypeName "Record")) ->
    true
  _ ->
    false
