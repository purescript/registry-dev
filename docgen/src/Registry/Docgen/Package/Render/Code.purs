module Registry.Docgen.Package.Render.Code where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Foldable (fold, foldMap, foldl)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.String.CodeUnits (fromCharArray)
import Registry.Docgen.Docs (Associativity(..), ChildDeclConstructorRep, ChildDeclInstanceRep, ChildDeclTypeClassMemberRep, DeclDataRep, DeclForeignDataRep, DeclInfixRep, DeclTypeClassRep, DeclTypeRep, DeclValueRep, DocConstraint(..), DocType(..), ForallBinding(..), FunDep(..), Ident(..), InfixAlias(..), IntLiteral(..), Qualified(..), Role(..), RowLabel(..), RowRep, StringLiteral(..), TypeName, TypeVar(..))
import Registry.Docgen.Package.Types (ModuleRef(..), Namespace(..))

type CodeRenderer rep =
  { keyword :: String -> rep
  , label :: String -> rep
  , line :: Int -> rep -> rep
  , reference :: ModuleRef -> String -> rep
  , role :: String -> rep
  , space :: rep
  , syntax :: String -> rep
  }

renderType :: forall rep. Monoid rep => CodeRenderer rep -> DocType -> rep
renderType code@{ keyword, reference, space, syntax } = render
  where
  render :: DocType -> rep
  render = case _ of
    TypeWildcard ->
      syntax "_"
    TypeIdent ident ->
      reference Local (unwrap ident)
    TypeString str ->
      syntax $ renderStringLiteral str
    TypeInt n ->
      renderIntLiteral n
    TypeConstructor name ->
      renderQualified code NSType name
    TypeOperator name ->
      renderQualified code NSType name
    TypeApp { function, arg } ->
      render function
        <> space
        <> render arg
    TypeKindApp { function, arg } ->
      render function
        <> space
        <> syntax "@"
        <> render arg
    TypeInfixApp { argLhs, operator: TypeOperator name, argRhs } ->
      render argLhs
        <> space
        <> renderQualified code NSType name
        <> space
        <> render argRhs
    TypeInfixApp { argLhs, operator, argRhs } ->
      render argLhs
        <> space
        <> syntax "`"
        <> render operator
        <> syntax "`"
        <> space
        <> render argRhs
    TypeForall { bindings, body } ->
      keyword "forall"
        <> space
        <> intercalateMap space renderBinding bindings
        <> syntax "."
        <> space
        <> render body
    TypeConstrained { constraint, result } ->
      render (constraintAsType constraint)
        <> space
        <> syntax "=>"
        <> space
        <> render result
    TypeFunction { arg, result } ->
      render arg
        <> space
        <> syntax "->"
        <> space
        <> render result
    TypeKindSignature { term, signature } ->
      render term
        <> space
        <> syntax "::"
        <> space
        <> render signature
    TypeRecord { labels: [], tail: Nothing } ->
      syntax "{}"
    TypeRecord row ->
      syntax "{"
        <> space
        <> renderRow row
        <> space
        <> syntax "}"
    TypeRow { labels: [], tail: Nothing } ->
      syntax "()"
    TypeRow row ->
      syntax "("
        <> renderRow row
        <> syntax ")"
    TypeParens ty ->
      syntax "("
        <> render ty
        <> syntax ")"

  renderRow :: RowRep -> rep
  renderRow { labels, tail } =
    intercalate (syntax "," <> space) (renderLabel code <$> labels)
      <> foldMap (\ty -> space <> syntax "|" <> space <> render ty) tail

  renderBinding :: ForallBinding -> rep
  renderBinding (ForallBinding { isVisible, name, signature }) = case signature of
    Just ty ->
      syntax "("
        <> variable
        <> space
        <> syntax "::"
        <> space
        <> render ty
    Nothing ->
      variable
    where
    variable =
      guard isVisible (syntax "@")
        <> reference Local (unwrap name)

  renderIntLiteral :: IntLiteral -> rep
  renderIntLiteral = case _ of
    IntSmall n ->
      syntax $ show n
    IntBig n ->
      syntax n

renderQualified :: forall rep a. Newtype a String => CodeRenderer rep -> Namespace -> Qualified a -> rep
renderQualified { reference } namespace (Qualified { moduleName, name }) =
  reference (ModuleRef (Just moduleName) namespace) $ unwrap name

renderLabel :: forall rep. Monoid rep => CodeRenderer rep -> RowLabel -> rep
renderLabel code@{ label, space, syntax } (RowLabel { label: lbl, signature }) =
  label (renderStringLiteral lbl)
    <> space
    <> syntax "::"
    <> space
    <> renderType code signature

renderStringLiteral :: StringLiteral -> String
renderStringLiteral = case _ of
  StringValue str ->
    show str
  StringCodeUnits arr ->
    show $ fromCharArray $ Array.mapMaybe fromCharCode arr

constraintAsType :: DocConstraint -> DocType
constraintAsType (DocConstraint { args, name }) =
  foldl (\function arg -> TypeApp { function, arg }) (TypeConstructor name) args

renderTypeVar :: forall rep. Monoid rep => CodeRenderer rep -> TypeVar -> rep
renderTypeVar code@{ reference, space, syntax } (TypeVar { ident: Ident name, signature }) = case signature of
  Nothing ->
    reference Local name
  Just kind -> fold
    [ syntax "("
    , reference Local name
    , space
    , syntax "::"
    , space
    , renderType code kind
    , syntax ")"
    ]

renderTypeVarsWithRoles :: forall rep. Monoid rep => CodeRenderer rep -> Array TypeVar -> Array Role -> rep
renderTypeVarsWithRoles code@{ space } vars roles =
  intercalate space $ Array.zipWith (\var r -> renderTypeVar code var <> renderRole code r) vars roles

renderRole :: forall rep. Monoid rep => CodeRenderer rep -> Role -> rep
renderRole { role } = case _ of
  Representational -> mempty
  Nominal -> role "nominal"
  Phantom -> role "phantom"

renderConstraintContext :: forall rep. Monoid rep => CodeRenderer rep -> Array (DocConstraint) -> rep -> rep
renderConstraintContext code@{ syntax, space } constraints arrow
  | Array.null constraints = mempty
  | otherwise = fold
      [ syntax "("
      , intercalate (syntax "," <> space) $
          map (renderType code <<< constraintAsType) constraints
      , syntax ")"
      , space
      , arrow
      , space
      ]

renderDeclSignature :: forall rep. Monoid rep => CodeRenderer rep -> String -> Qualified TypeName -> DocType -> rep
renderDeclSignature code@{ keyword, space, syntax } kw name signature =
  intercalate space
    [ keyword kw
    , renderQualified code NSType name
    , syntax "::"
    , renderType code signature
    ]

renderDeclValue :: forall rep. Monoid rep => CodeRenderer rep -> DeclValueRep -> rep
renderDeclValue code@{ space, syntax } { name, signature } =
  intercalate space
    [ renderQualified code NSValue name
    , syntax "::"
    , renderType code signature
    ]

renderDeclData :: forall rep. Monoid rep => CodeRenderer rep -> DeclDataRep -> rep
renderDeclData code@{ keyword, line, space } { isNewtype, name, roles, signature, vars } =
  fold
    [ foldMap (line 0 <<< renderDeclSignature code dataKw name) signature
    , line 0 $ keyword dataKw
        <> space
        <> renderQualified code NSType name
        <> renderTypeVarsWithRoles code vars roles
    ]
  where
  dataKw
    | isNewtype = "newtype"
    | otherwise = "data"

renderDeclForeignData :: forall rep. Monoid rep => CodeRenderer rep -> DeclForeignDataRep -> rep
renderDeclForeignData code@{ line } { name, roles, signature } = fold
  [ line 0 $ renderDeclSignature code "foreign import data" name signature
  , line 0 $ renderTypeVarsWithRoles code [] roles
  ]

renderDeclType :: forall rep. Monoid rep => CodeRenderer rep -> DeclTypeRep -> rep
renderDeclType code@{ keyword, line, space, syntax } { body, name, signature, vars } =
  fold
    [ foldMap (line 0 <<< renderDeclSignature code "type" name) signature
    , line 0 $ intercalate space
        [ keyword "type"
        , renderQualified code NSType name
        , intercalate space (renderTypeVar code <$> vars)
        , syntax "="
        , renderType code body
        ]
    ]

renderDeclTypeClass :: forall rep. Monoid rep => CodeRenderer rep -> DeclTypeClassRep -> rep
renderDeclTypeClass code@{ keyword, line, reference, space, syntax } { funDeps, name, signature, superClasses, vars } =
  fold
    [ foldMap (line 0 <<< renderDeclSignature code "class" name) signature
    , line 0 $ fold
        [ keyword "class"
        , space
        , renderConstraintContext code superClasses (syntax "<=")
        , renderQualified code NSType name
        , space
        , intercalate space (renderTypeVar code <$> vars)
        , renderedFunDeps
        ]
    ]
  where
  renderedFunDeps
    | Array.null funDeps = mempty
    | otherwise = fold
        [ space
        , syntax "|"
        , space
        , intercalate (syntax "," <> space) $ renderFunDep <$> funDeps
        ]

  renderFunDep (FunDep { determinees, determiners }) =
    fold
      [ intercalate space (reference Local <<< unwrap <$> determiners)
      , space
      , syntax "->"
      , space
      , intercalate space (reference Local <<< unwrap <$> determinees)
      ]

renderDeclInfix :: forall rep. Monoid rep => CodeRenderer rep -> DeclInfixRep -> rep
renderDeclInfix code@{ keyword, space, syntax } { alias, associativity, name, precedence } =
  fold
    [ keyword infixKw
    , space
    , syntax (show precedence)
    , typeKw
    , space
    , aliasName
    , space
    , keyword "as"
    , space
    , renderQualified code (if isTypeOp then NSType else NSValue) name
    ]

  where
  isTypeOp = case alias of
    AliasType _ -> true
    _ -> false

  infixKw = case associativity of
    Infix -> "infix"
    Infixl -> "infixl"
    Infixr -> "infixr"

  typeKw =
    guard isTypeOp $ space <> keyword "type"

  aliasName = case alias of
    AliasType ty ->
      renderQualified code NSType ty
    AliasConstructor ctor ->
      renderQualified code NSValue ctor
    AliasValue val ->
      renderQualified code NSValue val

renderChildDeclConstructor :: forall rep. Monoid rep => CodeRenderer rep -> ChildDeclConstructorRep -> rep
renderChildDeclConstructor code@{ space } { args, name } =
  foldl go (renderQualified code NSValue name) args
  where
  go acc arg = acc <> space <> renderType code arg

renderChildDeclInstance :: forall rep. Monoid rep => CodeRenderer rep -> ChildDeclInstanceRep -> rep
renderChildDeclInstance code@{ syntax } { constraints, head } =
  renderConstraintContext code constraints (syntax "=>")
    <> renderType code head

renderChildDeclTypeClassMember :: forall rep. Monoid rep => CodeRenderer rep -> ChildDeclTypeClassMemberRep -> rep
renderChildDeclTypeClassMember code@{ space, syntax } { name, signature } =
  intercalate space
    [ renderQualified code NSValue name
    , syntax "::"
    , renderType code signature
    ]
