module Registry.Docgen.Docs where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Map (Map)
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as String
import Registry.License (License)
import Registry.Location (Location)
import Registry.PackageName (PackageName)
import Registry.Sha256 (Sha256)
import Registry.Version (Version)

schemaVersion :: Int
schemaVersion = 1

newtype SourceArtifact = SourceArtifact
  { bytes :: Number
  , hash :: Sha256
  }

derive instance Newtype SourceArtifact _

newtype RawRange = RawRange String

derive newtype instance Eq RawRange
derive newtype instance Ord RawRange
derive instance Newtype RawRange _

newtype ModuleName = ModuleName String

derive newtype instance Eq ModuleName
derive newtype instance Ord ModuleName
derive instance Newtype ModuleName _

newtype Ident = Ident String

derive newtype instance Eq Ident
derive newtype instance Ord Ident
derive instance Newtype Ident _

newtype TypeName = TypeName String

derive newtype instance Eq TypeName
derive newtype instance Ord TypeName
derive instance Newtype TypeName _

newtype OperatorName = OperatorName String

derive newtype instance Eq OperatorName
derive newtype instance Ord OperatorName
derive instance Newtype OperatorName _

newtype DataConstructorName = DataConstructorName String

derive newtype instance Eq DataConstructorName
derive newtype instance Ord DataConstructorName
derive instance Newtype DataConstructorName _

newtype ValueName = ValueName String

derive newtype instance Eq ValueName
derive newtype instance Ord ValueName
derive instance Newtype ValueName _

newtype Readme = Readme
  { content :: String
  , extension :: Maybe String
  }

derive instance Newtype Readme _

newtype SourcePos = SourcePos
  { column :: Int
  , line :: Int
  }

derive instance Newtype SourcePos _

data Role
  = Nominal
  | Representational
  | Phantom

newtype SourceSpan = SourceSpan
  { end :: SourcePos
  , path :: String
  , start :: SourcePos
  }

derive instance Newtype SourceSpan _

newtype Qualified a = Qualified
  { moduleName :: ModuleName
  , name :: a
  }

derive instance Newtype (Qualified a) _

newtype FunDep = FunDep
  { determiners :: Array Ident
  , determinees :: Array Ident
  }

derive instance Newtype FunDep _

data StringLiteral
  = StringValue String
  | StringCodeUnits (Array Int)

data IntLiteral
  = IntSmall Int
  | IntBig String

newtype RowLabel = RowLabel
  { label :: StringLiteral
  , signature :: DocType
  }

derive instance Newtype RowLabel _

newtype ForallBinding = ForallBinding
  { isVisible :: Boolean
  , name :: Ident
  , signature :: Maybe DocType
  }

derive instance Newtype ForallBinding _

type TypeAppRep =
  { arg :: DocType
  , function :: DocType
  }

type InfixAppRep =
  { argLhs :: DocType
  , argRhs :: DocType
  , operator :: DocType
  }

type ForallRep =
  { bindings :: NonEmptyArray ForallBinding
  , body :: DocType
  }

type ConstrainedRep =
  { constraint :: DocConstraint
  , result :: DocType
  }

type FunctionRep =
  { arg :: DocType
  , result :: DocType
  }

type KindSignatureRep =
  { signature :: DocType
  , term :: DocType
  }

type RowRep =
  { labels :: Array RowLabel
  , tail :: Maybe DocType
  }

data DocType
  = TypeWildcard
  | TypeIdent Ident
  | TypeString StringLiteral
  | TypeInt IntLiteral
  | TypeConstructor (Qualified TypeName)
  | TypeOperator (Qualified OperatorName)
  | TypeApp TypeAppRep
  | TypeKindApp TypeAppRep
  | TypeInfixApp InfixAppRep
  | TypeForall ForallRep
  | TypeConstrained ConstrainedRep
  | TypeFunction FunctionRep
  | TypeKindSignature KindSignatureRep
  | TypeRecord RowRep
  | TypeRow RowRep
  | TypeParens DocType

newtype DocConstraint = DocConstraint
  { args :: Array DocType
  , name :: Qualified TypeName
  }

derive instance Newtype DocConstraint _

type ChildDeclInstanceRep =
  { constraints :: Array DocConstraint
  , head :: DocType
  , name :: Qualified ValueName
  }

type ChildDeclConstructorRep =
  { args :: Array DocType
  , name :: Qualified DataConstructorName
  }

type ChildDeclTypeClassMemberRep =
  { name :: Qualified ValueName
  , signature :: DocType
  }

data DocChildDeclarationInfo
  = ChildDeclInstance ChildDeclInstanceRep
  | ChildDeclConstructor ChildDeclConstructorRep
  | ChildDeclTypeClassMember ChildDeclTypeClassMemberRep

newtype DocChildDeclaration = DocChildDeclaration
  { comments :: Maybe String
  , info :: DocChildDeclarationInfo
  , sourceSpan :: Maybe SourceSpan
  }

derive instance Newtype DocChildDeclaration _

newtype TypeVar = TypeVar
  { ident :: Ident
  , signature :: Maybe DocType
  }

derive instance Newtype TypeVar _

type DeclValueRep =
  { name :: Qualified ValueName
  , signature :: DocType
  }

type DeclDataRep =
  { isNewtype :: Boolean
  , name :: Qualified TypeName
  , roles :: Array Role
  , signature :: Maybe DocType
  , vars :: Array TypeVar
  }

type DeclTypeRep =
  { body :: DocType
  , name :: Qualified TypeName
  , signature :: Maybe DocType
  , vars :: Array TypeVar
  }

type DeclTypeClassRep =
  { funDeps :: Array FunDep
  , name :: Qualified TypeName
  , signature :: Maybe DocType
  , superClasses :: Array DocConstraint
  , vars :: Array TypeVar
  }

data Associativity
  = Infixl
  | Infixr
  | Infix

data InfixAlias
  = AliasType (Qualified TypeName)
  | AliasConstructor (Qualified DataConstructorName)
  | AliasValue (Qualified ValueName)

type DeclInfixRep =
  { alias :: InfixAlias
  , associativity :: Associativity
  , name :: Qualified OperatorName
  , precedence :: Int
  }

type DeclForeignDataRep =
  { name :: Qualified TypeName
  , roles :: Array Role
  , signature :: DocType
  }

data DocDeclarationInfo
  = DeclValue DeclValueRep
  | DeclData DeclDataRep
  | DeclType DeclTypeRep
  | DeclTypeClass DeclTypeClassRep
  | DeclInfix DeclInfixRep
  | DeclForeignData DeclForeignDataRep

newtype DocDeclaration = DocDeclaration
  { children :: Array DocChildDeclaration
  , info :: DocDeclarationInfo
  , comments :: Maybe String
  , sourceSpan :: Maybe SourceSpan
  }

derive instance Newtype DocDeclaration _

newtype DocReexport = DocReexport
  { moduleName :: ModuleName
  , declarations :: Array DocDeclaration
  }

derive instance Newtype DocReexport _

newtype DocModule = DocModule
  { comments :: Maybe String
  , declarations :: Array DocDeclaration
  , name :: ModuleName
  , reexports :: Array DocReexport
  }

derive instance Newtype DocModule _

newtype DocPackage = DocPackage
  { schemaVersion :: Int
  , compilerVersion :: Version
  , sourceArtifact :: SourceArtifact
  , dependencies :: Map PackageName RawRange
  , description :: Maybe String
  , license :: License
  , location :: Location
  , locationRef :: Maybe String
  , name :: PackageName
  , modules :: Array DocModule
  , readme :: Maybe Readme
  , resolvedDependencies :: Map PackageName Version
  , resolvedModulePackages :: Map ModuleName PackageName
  , version :: Version
  }

derive instance Newtype DocPackage _

isPrim :: ModuleName -> Boolean
isPrim (ModuleName name) =
  name == "Prim" || isJust (String.stripPrefix (Pattern "Prim.") name)
