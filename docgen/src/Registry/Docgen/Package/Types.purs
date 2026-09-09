module Registry.Docgen.Package.Types where

import Data.Maybe (Maybe)
import Registry.Docgen.Docs (ModuleName)

-- | References point to either a Type or Value namespace.
data Namespace
  = NSType
  | NSValue

-- | Linkable references need to point a module and namespace. Local references
-- | Have nothing interesting to link to. A `Nothing` value for the module name
-- | is considered to be the "current" module being processed.
data ModuleRef
  = ModuleRef (Maybe ModuleName) Namespace
  | Local
