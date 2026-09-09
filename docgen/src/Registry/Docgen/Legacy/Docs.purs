module Registry.Docgen.Legacy.Docs where

import Prelude

import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Registry.Docgen.Docs (Associativity, DataConstructorName, Ident, ModuleName, OperatorName, RawRange, Role, SourcePos, SourceSpan, StringLiteral, TypeName)
import Registry.License (License)
import Registry.PackageName (PackageName)
import Registry.Version (Version)

newtype GithubData = GithubData
  { user :: String
  , repo :: String
  }

derive newtype instance Eq GithubData

derive newtype instance Ord GithubData

derive instance Newtype GithubData _

data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName

data Qualified a = Qualified QualifiedBy a

derive instance Functor Qualified

data DataDeclType
  = Data
  | Newtype

data DocTypeVar = DocTypeVar Ident (Maybe DocType)

data Fundep = Fundep (Array Ident) (Array Ident)

newtype Fixity = Fixity
  { associativity :: Associativity
  , precedence :: Int
  }

type FixityAlias = Qualified (Either TypeName (Either Ident DataConstructorName))

data DeclarationInfo
  = ValueDeclaration DocType
  | DataDeclaration DataDeclType (Array DocTypeVar) (Array Role)
  | TypeSynonymDeclaration (Array DocTypeVar) DocType
  | TypeClassDeclaration (Array DocTypeVar) (Array DocConstraint) (Array Fundep)
  | AliasDeclaration Fixity FixityAlias
  | ExternDataDeclaration DocType (Array Role)

data ChildDeclarationInfo
  = ChildInstance (Array DocConstraint) DocType
  | ChildDataConstructor (Array DocType)
  | ChildTypeClassMember DocType

newtype ChildDeclaration = ChildDeclaration
  { title :: String
  , comments :: Maybe String
  , sourceSpan :: Maybe SourceSpan
  , info :: ChildDeclarationInfo
  }

derive instance Newtype ChildDeclaration _

data KindSignatureFor
  = DataSig
  | NewtypeSig
  | TypeSynonymSig
  | ClassSig

data WildcardData
  = HoleWildcard String
  | UnnamedWildcard
  | IgnoredWildcard

data TypeVarVisibility
  = TypeVarVisible
  | TypeVarInvisible

data DocType
  = TypeVar String
  | TypeLevelString StringLiteral
  | TypeLevelInt Int
  | TypeWildcard WildcardData
  | TypeConstructor (Qualified TypeName)
  | TypeOp (Qualified OperatorName)
  | TypeApp DocType DocType
  | KindApp DocType DocType
  | ForAll TypeVarVisibility String (Maybe DocType) DocType
  | ConstrainedType DocConstraint DocType
  | KindedType DocType DocType
  | BinaryNoParensType DocType DocType DocType
  | ParensInType DocType
  | RCons StringLiteral DocType DocType
  | REmpty

data DocConstraint = DocConstraint (Qualified TypeName) (Array DocType) (Array DocType)

newtype KindInfo = KindInfo
  { keyword :: KindSignatureFor
  , kind :: DocType
  }

derive instance Newtype KindInfo _

newtype Declaration = Declaration
  { children :: Array ChildDeclaration
  , comments :: Maybe String
  , info :: DeclarationInfo
  , kindInfo :: Maybe KindInfo
  , sourceSpan :: Maybe SourceSpan
  , title :: String
  }

derive instance Newtype Declaration _

newtype InPackage a = InPackage
  { item :: a
  , package :: Maybe PackageName
  }

derive instance Newtype (InPackage a) _

newtype ReExport = ReExport
  { declarations :: Array Declaration
  , moduleName :: InPackage ModuleName
  }

newtype Author = Author
  { name :: String
  , email :: Maybe String
  , homepage :: Maybe String
  }

derive instance Newtype Author _

newtype Repository = Repository
  { url :: String
  , type :: String
  }

derive instance Newtype Repository _

newtype DocModule = DocModule
  { comments :: Maybe String
  , declarations :: Array Declaration
  , name :: ModuleName
  , reExports :: Array ReExport
  }

derive instance Newtype DocModule _

newtype DocPackageMeta = DocPackageMeta
  { authors :: Array Author
  , dependencies :: Map PackageName RawRange
  , description :: Maybe String
  , license :: License
  , name :: PackageName
  , repository :: Maybe Repository
  }

derive instance Newtype DocPackageMeta _

newtype DocPackage = DocPackage
  { compilerVersion :: String
  , github :: GithubData
  , moduleMap :: Map ModuleName PackageName
  , modules :: Array DocModule
  , packageMeta :: DocPackageMeta
  , resolvedDependencies :: Map PackageName Version
  , version :: Version
  , versionTag :: String
  }

derive instance Newtype DocPackage _
