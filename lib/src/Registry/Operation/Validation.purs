module Registry.Operation.Validation where

import Prelude

import Data.Array as Array
import Data.Maybe (isJust)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Effect.Aff (Aff)

-- TODO: Link to the spec

-- publish ::
-- transfer ::

-- | Checks that there is at least one purs file within the `src` directory
containsPursFile :: FilePath -> Aff Boolean
containsPursFile parent = do
  children <- FS.Aff.readdir parent
  stats <- traverse (\path -> { path, stats: _ } <$> FS.Aff.stat path) children
  let 
    files = Array.filter (_.stats >>> Stats.isFile) stats
    directories = Array.filter (_.stats >>> Stats.isDirectory) stats

  if Array.any (\{ path } -> isJust (String.stripSuffix (Pattern ".purs") path)) files then
    pure true
  else do
    results <- traverse (_.path >>> containsPursFile) directories 
    pure $ Array.any (eq true) results
