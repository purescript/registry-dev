module Registry.Docgen.Legacy.JSON
  ( decodeDocPackage
  , decodeDocModule
  ) where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import JSON (JObject, JSON)
import JSON as JSON
import JSON.Path as JP
import Registry.Docgen.Decoder (Decoder, decodeArray, decodeIndex, decodeInt, decodeJObject, decodeMap, decodeNull, decodeOptional, decodeProp, decodePropOptional, decodeString, defer, guardLength, liftEither, runDecoder, throw, traversedAtIndex)
import Registry.Docgen.Docs (Associativity(..), DataConstructorName(..), Ident(..), ModuleName(..), OperatorName(..), RawRange(..), Role(..), SourcePos(..), SourceSpan(..), StringLiteral(..), TypeName(..))
import Registry.Docgen.Legacy.Docs (Author(..), ChildDeclaration(..), ChildDeclarationInfo(..), DataDeclType(..), Declaration(..), DeclarationInfo(..), DocConstraint(..), DocModule(..), DocPackage(..), DocPackageMeta(..), DocType(..), DocTypeVar(..), Fixity(..), FixityAlias, Fundep(..), GithubData(..), InPackage(..), KindInfo(..), KindSignatureFor(..), Qualified(..), QualifiedBy(..), ReExport(..), Repository(..), TypeVarVisibility(..), WildcardData(..))
import Registry.License (License)
import Registry.License as License
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version
import Safe.Coerce (coerce)

decodeDocPackage :: JSON -> Either DecodeError DocPackage
decodeDocPackage = runDecoder decodeDocPackage'

decodeDocModule :: JSON -> Either DecodeError DocModule
decodeDocModule = runDecoder decodeDocModule'

decodeDocPackage' :: Decoder JSON DocPackage
decodeDocPackage' = decodeJObject ado
  compilerVersion <- decodeProp "compilerVersion" decodeString
  github <- decodeProp "github" decodeGithubData
  moduleMap <- decodeProp "moduleMap" decodeModuleMap
  modules <- decodeProp "modules" $ decodeArray (traversedAtIndex decodeDocModule')
  packageMeta <- decodeProp "packageMeta" decodePackageMeta
  resolvedDependencies <- decodePropOptional "resolvedDependencies" $ decodeDependencies decodeVersion
  version <- decodeProp "version" decodeVersion
  versionTag <- decodeProp "versionTag" decodeString
  in
    DocPackage
      { compilerVersion
      , github
      , moduleMap
      , modules
      , packageMeta
      , resolvedDependencies: fromMaybe Map.empty resolvedDependencies
      , version
      , versionTag
      }

decodePackageName :: Decoder JSON PackageName
decodePackageName = map fixName decodeString >>> liftEither PackageName.parse
  where
  fixName name
    | Array.elem name allowedPrefixNames = name
    | otherwise = PackageName.stripPureScriptPrefix name

  allowedPrefixNames =
    [ "purescript-compiler-backend-utilities"
    ]

decodeModuleMap :: Decoder JSON (Map ModuleName PackageName)
decodeModuleMap = decodeJObject $ decodeMap \key ->
  Tuple (ModuleName key) <$> decodePackageName

decodeGithubData :: Decoder JSON GithubData
decodeGithubData = decodeArray ado
  user <- decodeIndex 0 decodeString
  repo <- decodeIndex 1 decodeString
  in GithubData { user, repo }

decodeVersion :: Decoder JSON Version
decodeVersion = liftEither Version.parse <<< decodeString

decodePackageMeta :: Decoder JSON DocPackageMeta
decodePackageMeta = decodeJObject ado
  authors <- decodePropOptional "authors" $ decodeArray (traversedAtIndex decodeAuthor)
  dependencies <- decodePropOptional "dependencies" $ decodeDependencies decodeRange
  description <- decodePropOptional "description" decodeString
  license <- decodeProp "license" decodeLicense
  name <- decodeProp "name" decodePackageName
  repository <- decodePropOptional "repository" decodeRepository
  in
    DocPackageMeta
      { authors: fromMaybe [] authors
      , dependencies: fromMaybe Map.empty dependencies
      , description
      , license
      , name
      , repository
      }

decodeLicense :: Decoder JSON License
decodeLicense = stringLicense <|> arrayLicense
  where
  stringLicense =
    liftEither License.parse <<< decodeString

  arrayLicense = ado
    licenses <- decodeArray $ traversedAtIndex stringLicense >>> liftEither toNonEmpty
    in License.joinWith License.Or licenses

  toNonEmpty licenses = case NonEmptyArray.fromArray licenses of
    Just nonEmpty -> Right nonEmpty
    Nothing -> Left "Expected non-empty license array"

decodeRepository :: Decoder JSON Repository
decodeRepository = decodeJObject ado
  url <- decodeProp "url" decodeString
  type_ <- decodeProp "type" decodeString
  in Repository { url, type: type_ }

decodeAuthor :: Decoder JSON Author
decodeAuthor = decodeStringAuthor <|> decodeObjectAuthor
  where
  decodeStringAuthor =
    Author <<< { name: _, email: Nothing, homepage: Nothing } <$> decodeString

  decodeObjectAuthor = decodeJObject ado
    name <- decodeProp "name" decodeString
    email <- decodePropOptional "email" decodeString
    homepage <- decodePropOptional "homepage" decodeString
    in Author { name, email, homepage }

decodeDependencies :: forall a. Decoder JSON a -> Decoder JSON (Map PackageName a)
decodeDependencies decoder = decodeJObject $ decodeMap \key ->
  Tuple <$> (pure (JSON.fromString key) >>> decodePackageName) <*> decoder

decodeRange :: Decoder JSON RawRange
decodeRange = RawRange <$> decodeString

decodeDocModule' :: Decoder JSON DocModule
decodeDocModule' = decodeJObject ado
  comments <- decodePropOptional "comments" decodeString
  declarations <- decodeProp "declarations" $ decodeArray (traversedAtIndex decodeDeclaration)
  name <- decodeProp "name" decodeString
  reExports <- decodeProp "reExports" $ decodeArray (traversedAtIndex decodeReExport)
  in
    DocModule
      { comments
      , declarations
      , name: ModuleName name
      , reExports
      }

decodeDeclaration :: Decoder JSON Declaration
decodeDeclaration = defer \_ -> decodeJObject ado
  children <- decodeProp "children" $ decodeArray (traversedAtIndex decodeChildDeclaration)
  comments <- decodePropOptional "comments" decodeString
  info <- decodeProp "info" decodeDeclarationInfo
  kindInfo <- decodePropOptional "kind" decodeKindInfo
  sourceSpan <- decodePropOptional "sourceSpan" decodeSourceSpan
  title <- decodeProp "title" decodeString
  in
    Declaration
      { children
      , comments
      , info
      , kindInfo
      , sourceSpan
      , title
      }

decodeKindInfo :: Decoder JSON KindInfo
decodeKindInfo = decodeJObject ado
  keyword <- decodeProp "keyword" decodeKindSignatureFor
  kind <- decodeProp "kind" decodeDocType
  in KindInfo { keyword, kind }

decodeKindSignatureFor :: Decoder JSON KindSignatureFor
decodeKindSignatureFor = decodeString >>= case _ of
  "data" -> pure DataSig
  "newtype" -> pure NewtypeSig
  "class" -> pure ClassSig
  "type" -> pure TypeSynonymSig
  other -> throw $ DecodeError.basic $ "Unexpected KindSignatureFor: " <> other

decodeChildDeclaration :: Decoder JSON ChildDeclaration
decodeChildDeclaration = decodeJObject ado
  comments <- decodePropOptional "comments" decodeString
  info <- decodeProp "info" decodeChildDeclarationInfo
  sourceSpan <- decodePropOptional "sourceSpan" decodeSourceSpan
  title <- decodeProp "title" decodeString
  in ChildDeclaration { comments, info, sourceSpan, title }

decodeChildDeclarationInfo :: Decoder JSON ChildDeclarationInfo
decodeChildDeclarationInfo = decodeJObject do
  declType <- decodeProp "declType" decodeString
  case declType of
    "instance" -> decodeInstance
    "dataConstructor" -> decodeDataConstructor
    "typeClassMember" -> decodeTypeClassMember
    other -> throw $ DecodeError.error (JP.AtKey "declType" JP.Tip) $ "Unexpected value: " <> other
  where
  decodeInstance = ado
    dependencies <- decodeProp "dependencies" $ decodeArray (traversedAtIndex decodeDocConstraint)
    type_ <- decodeProp "type" decodeDocType
    in ChildInstance dependencies type_

  decodeDataConstructor = ado
    arguments <- decodeProp "arguments" $ decodeArray (traversedAtIndex decodeDocType)
    in ChildDataConstructor arguments

  decodeTypeClassMember = ado
    type_ <- decodeProp "type" decodeDocType
    in ChildTypeClassMember type_

decodeDeclarationInfo :: Decoder JSON DeclarationInfo
decodeDeclarationInfo = decodeJObject do
  declType <- decodeProp "declType" decodeString
  case declType of
    "value" -> decodeValueDeclaration
    "data" -> decodeDataDeclaration
    "externData" -> decodeExternData
    "typeSynonym" -> decodeTypeSynonym
    "typeClass" -> decodeTypeClass
    "alias" -> decodeAlias
    "kind" -> decodeCompatKind
    _ -> throw $ DecodeError.error (JP.AtKey "declType" JP.Tip) $ "Unexpected value: " <> declType
  where
  decodeValueDeclaration = ado
    type_ <- decodeProp "type" decodeDocType
    in ValueDeclaration type_

  decodeDataDeclaration = ado
    dataDeclType <- decodeProp "dataDeclType" decodeDataDeclType
    typeArguments <- decodeProp "typeArguments" $ decodeArray (traversedAtIndex decodeTypeVar)
    roles <- decodePropOptional "roles" $ decodeArray (traversedAtIndex decodeRole)
    in DataDeclaration dataDeclType typeArguments (fromMaybe [] roles)

  decodeExternData = ado
    kind <- decodeProp "kind" decodeDocType
    roles <- decodePropOptional "roles" $ decodeArray (traversedAtIndex decodeRole)
    in ExternDataDeclaration kind (fromMaybe [] roles)

  decodeTypeSynonym = ado
    arguments <- decodeProp "arguments" $ decodeArray (traversedAtIndex decodeTypeVar)
    type_ <- decodeProp "type" decodeDocType
    in TypeSynonymDeclaration arguments type_

  decodeTypeClass = ado
    arguments <- decodeProp "arguments" $ decodeArray (traversedAtIndex decodeTypeVar)
    superclasses <- decodeProp "superclasses" $ decodeArray (traversedAtIndex decodeDocConstraint)
    fundeps <- decodePropOptional "fundeps" $ decodeArray (traversedAtIndex decodeFundep)
    in TypeClassDeclaration arguments superclasses (fromMaybe [] fundeps)

  decodeAlias = ado
    fixity <- decodeProp "fixity" decodeFixity
    alias <- decodeProp "alias" decodeFixityAlias
    in AliasDeclaration fixity alias

  decodeCompatKind =
    pure $ ExternDataDeclaration (TypeConstructor primType) []

  primType = Qualified (ByModuleName (ModuleName "Prim")) (TypeName "Type")

decodeFixityAlias :: Decoder JSON FixityAlias
decodeFixityAlias = decodeQualified $ decodeEither decodeTypeName (decodeEither decodeIdent decodeDataConstructorName)

decodeTypeName :: Decoder JSON TypeName
decodeTypeName = TypeName <$> decodeString

decodeDataConstructorName :: Decoder JSON DataConstructorName
decodeDataConstructorName = DataConstructorName <$> decodeString

decodeIdent :: Decoder JSON Ident
decodeIdent = decodeJObject $ decodeProp "Ident" (Ident <$> decodeString)

decodeEither :: forall a b. Decoder JSON a -> Decoder JSON b -> Decoder JSON (Either a b)
decodeEither left right = decodeJObject do
  Left <$> decodeProp "Left" left
    <|> Right <$> decodeProp "Right" right

decodeFixity :: Decoder JSON Fixity
decodeFixity = decodeJObject ado
  associativity <- decodeProp "associativity" decodeAssociativity
  precedence <- decodeProp "precedence" decodeInt
  in Fixity { associativity, precedence }

decodeAssociativity :: Decoder JSON Associativity
decodeAssociativity = decodeString >>= case _ of
  "infixl" -> pure Infixl
  "infixr" -> pure Infixr
  "infix" -> pure Infix
  other -> throw $ DecodeError.basic $ "Unexpected associativity: " <> other

decodeFundep :: Decoder JSON Fundep
decodeFundep = decodeArray ado
  a <- decodeIndex 0 $ decodeArray (traversedAtIndex decodeString)
  b <- decodeIndex 1 $ decodeArray (traversedAtIndex decodeString)
  in Fundep (coerce a) (coerce b)

decodeTypeVar :: Decoder JSON DocTypeVar
decodeTypeVar = decodeArray ado
  name <- decodeIndex 0 decodeString
  type_ <- decodeIndex 1 $ decodeOptional decodeDocType
  in DocTypeVar (Ident name) type_

decodeRole :: Decoder JSON Role
decodeRole = decodeString >>= case _ of
  "Representational" -> pure Representational
  "Nominal" -> pure Nominal
  "Phantom" -> pure Phantom
  other -> throw $ DecodeError.basic $ "Unexpected Role: " <> other

decodeDataDeclType :: Decoder JSON DataDeclType
decodeDataDeclType = decodeString >>= case _ of
  "data" -> pure Data
  "newtype" -> pure Newtype
  other -> throw $ DecodeError.basic $ "Unexpected DataDeclType: " <> other

decodeSourceSpan :: Decoder JSON SourceSpan
decodeSourceSpan = decodeJObject ado
  end <- decodeProp "end" decodeSourcePos
  path <- decodeProp "name" decodeString
  start <- decodeProp "start" decodeSourcePos
  in SourceSpan { end, path, start }

decodeReExport :: Decoder JSON ReExport
decodeReExport = defer \_ -> decodeJObject ado
  moduleName <- decodeProp "moduleName" $ decodeInPackage decodeModuleName
  declarations <- decodeProp "declarations" $ decodeArray (traversedAtIndex decodeDeclaration)
  in ReExport { moduleName, declarations }

decodeInPackage :: forall a. Decoder JSON a -> Decoder JSON (InPackage a)
decodeInPackage decoder = decodeJObject ado
  package <- decodePropOptional "package" decodePackageName
  item <- decodeProp "item" decoder
  in InPackage { package, item }

decodeDocType :: Decoder JSON DocType
decodeDocType = defer \_ ->
  decodeJObject do
    tag <- decodeProp "tag" decodeString
    case tag of
      "TypeVar" -> decodeTypeVar'
      "TypeLevelString" -> decodeTypeLevelString
      "TypeLevelInt" -> decodeTypeLevelInt
      "TypeWildcard" -> decodeTypeWildcard
      "TypeConstructor" -> decodeTypeConstructor
      "TypeOp" -> decodeTypeOp
      "TypeApp" -> decodeTypeApp
      "KindApp" -> decodeKindApp
      "ForAll" -> decodeForAll
      "ConstrainedType" -> decodeConstrainedType
      "KindedType" -> decodeKindedType
      "BinaryNoParensType" -> decodeBinaryNoParensType
      "ParensInType" -> decodeParensInType
      "REmpty" -> pure REmpty
      "RCons" -> decodeRCons
      "Row" -> decodeCompatRow
      "FunKind" -> decodeCompatFunKind
      "NamedKind" -> decodeCompatNamedKind
      _ -> throw $ DecodeError.error (JP.AtKey "tag" JP.Tip) $ "Unexpected value: " <> tag
  where
  decodeTypeVar' =
    decodeContents $ TypeVar <$> decodeString

  decodeTypeLevelString =
    decodeContents $ TypeLevelString <$> decodePSString

  decodeTypeLevelInt =
    decodeContents $ TypeLevelInt <$> decodeInt

  decodeTypeWildcard =
    decodeContents $ TypeWildcard <$> decodeWildcardData

  decodeTypeConstructor =
    decodeContents $ TypeConstructor <<< coerce <$> decodeQualified decodeString

  decodeTypeOp =
    decodeContents $ TypeOp <<< coerce <$> decodeQualified decodeString

  decodeTypeApp = defer \_ ->
    decodeContents $ decodeArray ado
      lhs <- decodeIndex 0 decodeDocType
      rhs <- decodeIndex 1 decodeDocType
      in TypeApp lhs rhs

  decodeKindApp = defer \_ ->
    decodeContents $ decodeArray ado
      lhs <- decodeIndex 0 decodeDocType
      rhs <- decodeIndex 1 decodeDocType
      in KindApp lhs rhs

  decodeForAll = defer \_ -> do
    let
      asObject = decodeJObject ado
        visibility <- decodeProp "visibility" decodeTypeVarVisibility
        identifier <- decodeProp "identifier" decodeString
        kind <- decodePropOptional "kind" decodeDocType
        type_ <- decodeProp "type" decodeDocType
        in ForAll visibility identifier kind type_

      withoutKind = decodeArray do
        guardLength 3
        identifier <- decodeIndex 0 decodeString
        type_ <- decodeIndex 1 decodeDocType
        pure $ ForAll TypeVarInvisible identifier Nothing type_

      withKind = decodeArray do
        guardLength 4
        identifier <- decodeIndex 0 decodeString
        kind <- decodeIndex 1 $ decodeOptional decodeDocType
        type_ <- decodeIndex 2 decodeDocType
        pure $ ForAll TypeVarInvisible identifier kind type_

    decodeContents $ asObject <|> withoutKind <|> withKind

  decodeConstrainedType = defer \_ ->
    decodeContents $ decodeArray ado
      constraint <- decodeIndex 0 decodeDocConstraint
      type_ <- decodeIndex 1 decodeDocType
      in ConstrainedType constraint type_

  decodeKindedType = defer \_ ->
    decodeContents $ decodeArray ado
      a <- decodeIndex 0 decodeDocType
      b <- decodeIndex 1 decodeDocType
      in KindedType a b

  decodeBinaryNoParensType = defer \_ ->
    decodeContents $ decodeArray ado
      a <- decodeIndex 0 decodeDocType
      b <- decodeIndex 1 decodeDocType
      c <- decodeIndex 2 decodeDocType
      in BinaryNoParensType a b c

  decodeParensInType = defer \_ ->
    decodeContents $ ParensInType <$> decodeDocType

  decodeRCons = defer \_ ->
    decodeContents $ decodeArray ado
      label <- decodeIndex 0 decodePSString
      type_ <- decodeIndex 1 decodeDocType
      tail <- decodeIndex 2 decodeDocType
      in RCons label type_ tail

  decodeCompatRow = defer \_ ->
    decodeContents ado
      type_ <- decodeDocType
      in TypeApp (TypeConstructor primRow) type_

  decodeCompatFunKind = defer \_ ->
    decodeContents $ decodeArray ado
      a <- decodeIndex 0 decodeDocType
      b <- decodeIndex 1 decodeDocType
      in TypeApp (TypeApp (TypeConstructor primFunction) a) b

  decodeCompatNamedKind =
    decodeContents $ TypeConstructor <<< coerce <$> decodeQualified decodeString

  primRow = Qualified (ByModuleName (ModuleName "Prim")) (TypeName "Row")
  primFunction = Qualified (ByModuleName (ModuleName "Prim")) (TypeName "Function")

decodeContents :: forall a. Decoder JSON a -> Decoder JObject a
decodeContents = decodeProp "contents"

decodeDocConstraint :: Decoder JSON DocConstraint
decodeDocConstraint = defer \_ ->
  decodeJObject ado
    class_ <- decodeProp "constraintClass" $ decodeQualified decodeString
    kindArgs <- decodePropOptional "constraintKindArgs" $ decodeArray (traversedAtIndex decodeDocType)
    args <- decodeProp "constraintArgs" $ decodeArray (traversedAtIndex decodeDocType)
    in DocConstraint (coerce class_) (fromMaybe [] kindArgs) args

decodeTypeVarVisibility :: Decoder JSON TypeVarVisibility
decodeTypeVarVisibility =
  decodeString >>= case _ of
    "TypeVarVisible" -> pure TypeVarVisible
    "TypeVarInvisible" -> pure TypeVarInvisible
    _ -> throw $ DecodeError.basic "Expected TypeVarVisibility"

decodeQualified :: forall a. Decoder JSON a -> Decoder JSON (Qualified a)
decodeQualified decoder = decodeArray ado
  qual <- decodeIndex 0 decodeQualifiedBy
  value <- decodeIndex 1 decoder
  in Qualified qual value

decodeModuleName :: Decoder JSON ModuleName
decodeModuleName = ModuleName <<< Array.intercalate "." <$> decodeArray (traversedAtIndex decodeString)

decodeQualifiedBy :: Decoder JSON QualifiedBy
decodeQualifiedBy =
  ByModuleName <$> decodeModuleName
    <|> BySourcePos <$> decodeSourcePos
    <|> BySourcePos (SourcePos { line: 0, column: 0 }) <$ decodeNull

decodeSourcePos :: Decoder JSON SourcePos
decodeSourcePos = decodeArray ado
  line <- decodeIndex 0 decodeInt
  column <- decodeIndex 1 decodeInt
  in SourcePos { line, column }

decodeWildcardData :: Decoder JSON WildcardData
decodeWildcardData =
  HoleWildcard <$> decodeString
    <|> pure UnnamedWildcard

decodePSString :: Decoder JSON StringLiteral
decodePSString =
  StringValue <$> decodeString
    <|> StringCodeUnits <$> decodeArray (traversedAtIndex decodeInt)
