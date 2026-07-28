module Registry.Docgen.Codec where

import Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Alt ((<|>))
import Data.Codec (Codec')
import Data.Codec as Codec
import Data.Codec.JSON (Codec)
import Data.Codec.JSON.Common as Common
import Data.Codec.JSON.Record (object, optional)
import Data.Either (Either(..))
import Data.Lazy as Lazy
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import JSON (JSON)
import JSON as JSON
import JSON.Object as JObject
import Registry.Docgen.Decoder (Decoder, decodeArray, decodeInt, decodeJObject, decodeNull, decodeProp, decodeString)
import Registry.Docgen.Decoder as Decoder
import Registry.Docgen.Docs (Associativity(..), DataConstructorName, DocChildDeclaration, DocChildDeclarationInfo(..), DocConstraint, DocDeclaration, DocDeclarationInfo(..), DocModule, DocPackage(..), DocReexport, DocType(..), ForallBinding, FunDep, Ident, InfixAlias(..), IntLiteral(..), ModuleName(..), OperatorName, Qualified, RawRange, Readme, Role(..), RowLabel, SourceArtifact, SourcePos, SourceSpan, StringLiteral(..), TypeName, TypeVar, ValueName, schemaVersion)
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location as Location
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version

defer :: forall m a b. (Unit -> Codec' m a b) -> Codec' m a b
defer k =
  Codec.codec'
    (\a -> Codec.decode (Lazy.force lazy) a)
    (\a -> Codec.encode (Lazy.force lazy) a)
  where
  lazy = Lazy.defer k

taggedSum :: forall a. (String -> Decoder JSON a) -> (a -> Tuple String JSON) -> Codec a
taggedSum decode' encode' = Codec.codec' decode encode
  where
  decode = unwrap $ decodeJObject do
    tag <- decodeProp "tag" decodeString
    decodeProp "value" (decode' tag)

  encode a = do
    let Tuple tag value = encode' a
    JSON.fromJObject $ JObject.fromEntries
      [ Tuple "tag" (JSON.fromString tag)
      , Tuple "value" value
      ]

moduleName :: Codec ModuleName
moduleName = _Newtype Common.string

rawRange :: Codec RawRange
rawRange = _Newtype Common.string

ident :: Codec Ident
ident = _Newtype Common.string

typeName :: Codec TypeName
typeName = _Newtype Common.string

operatorName :: Codec OperatorName
operatorName = _Newtype Common.string

dataConstructorName :: Codec DataConstructorName
dataConstructorName = _Newtype Common.string

valueName :: Codec ValueName
valueName = _Newtype Common.string

readme :: Codec Readme
readme = _Newtype $ object
  { content: Common.string
  , extension: optional Common.string
  }

sourcePos :: Codec SourcePos
sourcePos = _Newtype $ object
  { column: Common.int
  , line: Common.int
  }

sourceSpan :: Codec SourceSpan
sourceSpan = _Newtype $ object
  { end: sourcePos
  , path: Common.string
  , start: sourcePos
  }

qualified :: forall a. Codec a -> Codec (Qualified a)
qualified name = _Newtype $ object { moduleName, name }

typeVar :: Codec TypeVar
typeVar = _Newtype $ object
  { ident
  , signature: optional docType
  }

funDep :: Codec FunDep
funDep = _Newtype $ object
  { determiners: Common.array ident
  , determinees: Common.array ident
  }

stringLiteral :: Codec StringLiteral
stringLiteral = Decoder.toCodec decode encode
  where
  decode =
    StringValue <$> decodeString
      <|> StringCodeUnits <$> decodeArray (traversed decodeInt)

  encode = case _ of
    StringValue value ->
      JSON.fromString value
    StringCodeUnits value ->
      JSON.fromArray $ JSON.fromInt <$> value

intLiteral :: Codec IntLiteral
intLiteral = Decoder.toCodec decode encode
  where
  decode =
    IntSmall <$> decodeInt
      <|> IntBig <$> decodeString

  encode = case _ of
    IntSmall value ->
      JSON.fromInt value
    IntBig value ->
      JSON.fromString value

forallBinding :: Codec ForallBinding
forallBinding = _Newtype $ defer \_ -> object
  { isVisible: Common.boolean
  , name: ident
  , signature: optional docType
  }

rowLabel :: Codec RowLabel
rowLabel = _Newtype $ defer \_ -> object
  { label: stringLiteral
  , signature: docType
  }

role :: Codec Role
role = Decoder.toCodec decode encode
  where
  decode = do
    tag <- decodeString
    case tag of
      "nominal" -> pure Nominal
      "representational" -> pure Representational
      "phantom" -> pure Phantom
      other -> Decoder.throw $ DecodeError.basic $ "Unknown Role: " <> other

  encode = case _ of
    Nominal -> JSON.fromString "nominal"
    Representational -> JSON.fromString "representational"
    Phantom -> JSON.fromString "phantom"

docConstraint :: Codec DocConstraint
docConstraint = _Newtype $ defer \_ -> object
  { args: Common.array docType
  , name: qualified typeName
  }

docType :: Codec DocType
docType = defer \_ -> taggedSum decode encode
  where
  decode = case _ of
    "wildcard" ->
      TypeWildcard <$ decodeNull
    "ident" ->
      TypeIdent <$> Decoder.fromCodec ident
    "string" ->
      TypeString <$> Decoder.fromCodec stringLiteral
    "int" ->
      TypeInt <$> Decoder.fromCodec intLiteral
    "constructor" ->
      TypeConstructor <$> Decoder.fromCodec typeConstructorRep
    "operator" ->
      TypeOperator <$> Decoder.fromCodec typeOperatorRep
    "app" ->
      TypeApp <$> Decoder.fromCodec typeAppRep
    "kindApp" ->
      TypeKindApp <$> Decoder.fromCodec typeAppRep
    "infixApp" ->
      TypeInfixApp <$> Decoder.fromCodec typeInfixAppRep
    "forall" ->
      TypeForall <$> Decoder.fromCodec typeForallRep
    "constrained" ->
      TypeConstrained <$> Decoder.fromCodec typeConstrainedRep
    "function" ->
      TypeFunction <$> Decoder.fromCodec typeFunctionRep
    "kindSignature" ->
      TypeKindSignature <$> Decoder.fromCodec typeKindSignatureRep
    "record" ->
      TypeRecord <$> Decoder.fromCodec typeRowRep
    "row" ->
      TypeRow <$> Decoder.fromCodec typeRowRep
    "parens" ->
      TypeParens <$> Decoder.fromCodec docType
    other ->
      Decoder.throw $ DecodeError.basic $ "Unknown DocType: " <> other

  encode = case _ of
    TypeWildcard ->
      Tuple "wildcard" JSON.null
    TypeIdent value ->
      Tuple "ident" (Codec.encode ident value)
    TypeString value ->
      Tuple "string" (Codec.encode stringLiteral value)
    TypeInt value ->
      Tuple "int" (Codec.encode intLiteral value)
    TypeConstructor value ->
      Tuple "constructor" (Codec.encode typeConstructorRep value)
    TypeOperator value ->
      Tuple "operator" (Codec.encode typeOperatorRep value)
    TypeApp value ->
      Tuple "app" (Codec.encode typeAppRep value)
    TypeKindApp value ->
      Tuple "kindApp" (Codec.encode typeAppRep value)
    TypeInfixApp value ->
      Tuple "infixApp" (Codec.encode typeInfixAppRep value)
    TypeForall value ->
      Tuple "forall" (Codec.encode typeForallRep value)
    TypeConstrained value ->
      Tuple "constrained" (Codec.encode typeConstrainedRep value)
    TypeFunction value ->
      Tuple "function" (Codec.encode typeFunctionRep value)
    TypeKindSignature value ->
      Tuple "kindSignature" (Codec.encode typeKindSignatureRep value)
    TypeRecord value ->
      Tuple "record" (Codec.encode typeRowRep value)
    TypeRow value ->
      Tuple "row" (Codec.encode typeRowRep value)
    TypeParens value ->
      Tuple "parens" (Codec.encode docType value)

  typeConstructorRep =
    qualified typeName

  typeOperatorRep =
    qualified operatorName

  typeAppRep = defer \_ -> object
    { arg: docType
    , function: docType
    }

  typeInfixAppRep = defer \_ -> object
    { argLhs: docType
    , argRhs: docType
    , operator: docType
    }

  typeForallRep = defer \_ -> object
    { bindings: Common.nonEmptyArray forallBinding
    , body: docType
    }

  typeConstrainedRep = defer \_ -> object
    { constraint: docConstraint
    , result: docType
    }

  typeFunctionRep = defer \_ -> object
    { arg: docType
    , result: docType
    }

  typeKindSignatureRep = defer \_ -> object
    { signature: docType
    , term: docType
    }

  typeRowRep = defer \_ -> object
    { labels: Common.array rowLabel
    , tail: optional docType
    }

docChildDeclarationInfo :: Codec DocChildDeclarationInfo
docChildDeclarationInfo = taggedSum decode encode
  where
  decode = case _ of
    "instance" ->
      ChildDeclInstance <$> Decoder.fromCodec childDeclInstanceRep
    "constructor" ->
      ChildDeclConstructor <$> Decoder.fromCodec childDeclConstructorRep
    "typeClassMember" ->
      ChildDeclTypeClassMember <$> Decoder.fromCodec childDeclTypeClassMemberRep
    other ->
      Decoder.throw $ DecodeError.basic $ "Unknown ChildDeclarationInfo: " <> other

  encode = case _ of
    ChildDeclInstance value ->
      Tuple "instance" $ Codec.encode childDeclInstanceRep value
    ChildDeclConstructor value ->
      Tuple "constructor" $ Codec.encode childDeclConstructorRep value
    ChildDeclTypeClassMember value ->
      Tuple "typeClassMember" $ Codec.encode childDeclTypeClassMemberRep value

  childDeclInstanceRep = object
    { constraints: Common.array docConstraint
    , head: docType
    , name: qualified valueName
    }

  childDeclConstructorRep = object
    { name: qualified dataConstructorName
    , args: Common.array docType
    }

  childDeclTypeClassMemberRep = object
    { name: qualified valueName
    , signature: docType
    }

docChildDeclaration :: Codec DocChildDeclaration
docChildDeclaration = _Newtype $ object
  { comments: optional Common.string
  , info: docChildDeclarationInfo
  , sourceSpan: optional sourceSpan
  }

infixAlias :: Codec InfixAlias
infixAlias = taggedSum decode encode
  where
  decode = case _ of
    "type" ->
      AliasType <$> Decoder.fromCodec aliasTypeRep
    "constructor" ->
      AliasConstructor <$> Decoder.fromCodec aliasConstructorRep
    "value" ->
      AliasValue <$> Decoder.fromCodec aliasValueRep
    other ->
      Decoder.throw $ DecodeError.basic $ "Unknown InfixAlias: " <> other

  encode = case _ of
    AliasType value ->
      Tuple "type" $ Codec.encode aliasTypeRep value
    AliasConstructor value ->
      Tuple "constructor" $ Codec.encode aliasConstructorRep value
    AliasValue value ->
      Tuple "value" $ Codec.encode aliasValueRep value

  aliasTypeRep =
    qualified typeName

  aliasConstructorRep =
    qualified dataConstructorName

  aliasValueRep =
    qualified valueName

associativity :: Codec Associativity
associativity = Decoder.toCodec decode encode
  where
  decode = do
    tag <- decodeString
    case tag of
      "infixl" -> pure Infixl
      "infixr" -> pure Infixr
      "infix" -> pure Infix
      other -> Decoder.throw $ DecodeError.basic $ "Unknown Associativity: " <> other

  encode = case _ of
    Infixl -> JSON.fromString "infixl"
    Infixr -> JSON.fromString "infixr"
    Infix -> JSON.fromString "infix"

docDeclarationInfo :: Codec DocDeclarationInfo
docDeclarationInfo = taggedSum decode encode
  where
  decode = case _ of
    "value" ->
      DeclValue <$> Decoder.fromCodec declValueRep
    "data" ->
      DeclData <$> Decoder.fromCodec declDataRep
    "type" ->
      DeclType <$> Decoder.fromCodec declTypeRep
    "typeClass" ->
      DeclTypeClass <$> Decoder.fromCodec declTypeClassRep
    "infix" ->
      DeclInfix <$> Decoder.fromCodec declInfixRep
    "foreignData" ->
      DeclForeignData <$> Decoder.fromCodec declForeignDataRep
    other ->
      Decoder.throw $ DecodeError.basic $ "Unknown DeclarationInfo: " <> other

  encode = case _ of
    DeclValue value ->
      Tuple "value" $ Codec.encode declValueRep value
    DeclData value ->
      Tuple "data" $ Codec.encode declDataRep value
    DeclType value ->
      Tuple "type" $ Codec.encode declTypeRep value
    DeclTypeClass value ->
      Tuple "typeClass" $ Codec.encode declTypeClassRep value
    DeclInfix value ->
      Tuple "infix" $ Codec.encode declInfixRep value
    DeclForeignData value ->
      Tuple "foreignData" $ Codec.encode declForeignDataRep value

  declValueRep = object
    { name: qualified valueName
    , signature: docType
    }

  declDataRep = object
    { isNewtype: Common.boolean
    , name: qualified typeName
    , roles: Common.array role
    , signature: optional docType
    , vars: Common.array typeVar
    }

  declTypeRep = object
    { body: docType
    , name: qualified typeName
    , signature: optional docType
    , vars: Common.array typeVar
    }

  declTypeClassRep = object
    { funDeps: Common.array funDep
    , name: qualified typeName
    , signature: optional docType
    , superClasses: Common.array docConstraint
    , vars: Common.array typeVar
    }

  declInfixRep = object
    { alias: infixAlias
    , associativity
    , name: qualified operatorName
    , precedence: Common.int
    }

  declForeignDataRep = object
    { name: qualified typeName
    , roles: Common.array role
    , signature: docType
    }

docDeclaration :: Codec DocDeclaration
docDeclaration = _Newtype $ object
  { children: Common.array docChildDeclaration
  , info: docDeclarationInfo
  , comments: optional Common.string
  , sourceSpan: optional sourceSpan
  }

docReexport :: Codec DocReexport
docReexport = _Newtype $ object
  { moduleName
  , declarations: Common.array docDeclaration
  }

docModule :: Codec DocModule
docModule = _Newtype $ object
  { comments: optional Common.string
  , declarations: Common.array docDeclaration
  , name: moduleName
  , reexports: Common.array docReexport
  }

sourceArtifact :: Codec SourceArtifact
sourceArtifact = _Newtype $ object
  { bytes: Common.number
  , hash: Sha256.codec
  }

docPackage :: Codec DocPackage
docPackage = Decoder.toCodec decode encode
  where
  rep = object
    { schemaVersion: Common.int
    , compilerVersion: Version.codec
    , sourceArtifact
    , dependencies: Internal.Codec.packageMap rawRange
    , description: optional Common.string
    , license: License.codec
    , location: Location.codec
    , locationRef: optional Common.string
    , modules: Common.array docModule
    , name: PackageName.codec
    , readme: optional readme
    , resolvedDependencies: Internal.Codec.packageMap Version.codec
    , resolvedModulePackages: Internal.Codec.strMap "ModuleName" (Right <<< ModuleName) (\(ModuleName name) -> name) PackageName.codec
    , version: Version.codec
    }

  decode = do
    { schemaVersion: version } <- Decoder.fromCodec $ object { schemaVersion: Common.int }
    if version /= schemaVersion then
      Decoder.throw $ DecodeError.basic $
        "Unsupported documentation schema version " <> show version
    else do
      value <- Decoder.fromCodec rep
      pure $ DocPackage value

  encode (DocPackage value) = Codec.encode rep value
