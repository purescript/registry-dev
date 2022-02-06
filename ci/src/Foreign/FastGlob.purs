module Foreign.FastGlob
  ( Include(..)
  , GlobOptions(..)
  , defaultGlobOptions
  , match
  , match'
  ) where

import Registry.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import ConvertableOptions (class Defaults)
import ConvertableOptions as ConvertableOptions

data Include = FilesAndDirectories | FilesOnly | DirectoriesOnly

derive instance Eq Include

-- https://github.com/mrmlnc/fast-glob#options-3
type GlobOptions =
  ( cwd :: Maybe FilePath
  , ignore :: Array FilePath
  , absolute :: Boolean
  , include :: Include
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  )

defaultGlobOptions :: { | GlobOptions }
defaultGlobOptions =
  { cwd: Nothing
  , ignore: []
  , absolute: false
  , include: FilesAndDirectories
  , caseSensitive: true
  , dotfiles: true
  , unique: true
  }

type JSGlobOptions =
  { cwd :: Nullable FilePath
  , ignore :: Array String
  , absolute :: Boolean
  , onlyDirectories :: Boolean
  , onlyFiles :: Boolean
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  }

globOptionsToJSGlobOptions :: { | GlobOptions } -> JSGlobOptions
globOptionsToJSGlobOptions options =
  { cwd: toNullable options.cwd
  , ignore: options.ignore
  , absolute: options.absolute
  , onlyDirectories: options.include == DirectoriesOnly
  , onlyFiles: options.include == FilesOnly
  , caseSensitive: options.caseSensitive
  , dotfiles: options.dotfiles
  , unique: options.unique
  }

foreign import matchImpl :: Array String -> JSGlobOptions -> Effect (Promise (Array FilePath))

match :: Array String -> Aff (Array FilePath)
match entries = match' entries {}

match'
  :: forall provided
   . Defaults { | GlobOptions } { | provided } { | GlobOptions }
  => Array String
  -> { | provided }
  -> Aff (Array FilePath)
match' entries provided = Promise.toAffE $ matchImpl entries $ globOptionsToJSGlobOptions options
  where
  options :: { | GlobOptions }
  options = ConvertableOptions.defaults defaultGlobOptions provided
