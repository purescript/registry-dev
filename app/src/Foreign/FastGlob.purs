module Foreign.FastGlob
  ( GlobOptions(..)
  , Include(..)
  , match
  , match'
  ) where

import Registry.App.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import ConvertableOptions (class Defaults)
import ConvertableOptions as ConvertableOptions
import Registry.Internal.Path as Internal.Path

data Include = FilesAndDirectories | FilesOnly | DirectoriesOnly

derive instance Eq Include

-- https://github.com/mrmlnc/fast-glob#options-3
type GlobOptions =
  ( ignore :: Array FilePath
  , include :: Include
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  )

defaultGlobOptions :: { | GlobOptions }
defaultGlobOptions =
  { ignore: []
  , include: FilesAndDirectories
  , caseSensitive: true
  , dotfiles: true
  , unique: true
  }

type JSGlobOptions =
  { cwd :: String
  , ignore :: Array String
  , onlyDirectories :: Boolean
  , onlyFiles :: Boolean
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  }

globOptionsToJSGlobOptions :: FilePath -> { | GlobOptions } -> JSGlobOptions
globOptionsToJSGlobOptions cwd options = do
  { cwd
  , ignore: options.ignore
  , onlyDirectories: options.include == DirectoriesOnly
  , onlyFiles: options.include == FilesOnly
  , caseSensitive: options.caseSensitive
  , dotfiles: options.dotfiles
  , unique: options.unique
  }

foreign import matchImpl :: Array String -> JSGlobOptions -> Effect (Promise (Array FilePath))

type SanitizedPaths = { succeeded :: Array FilePath, failed :: Array String }

-- | Match the provided list of glob patterns.
match :: FilePath -> Array String -> Aff SanitizedPaths
match baseDir entries = match' baseDir entries {}

-- | Match the provided list of glob patterns using the given glob options.
match'
  :: forall provided
   . Defaults { | GlobOptions } { | provided } { | GlobOptions }
  => FilePath
  -> Array String
  -> { | provided }
  -> Aff SanitizedPaths
match' baseDirectory entries opts = do
  let jsOptions = globOptionsToJSGlobOptions baseDirectory options
  matches <- Promise.toAffE $ matchImpl entries jsOptions
  results <- traverse (Internal.Path.sanitizePath baseDirectory) matches
  let { success, fail } = partitionEithers results
  pure { succeeded: success, failed: fail }
  where
  options :: { | GlobOptions }
  options = ConvertableOptions.defaults defaultGlobOptions opts
