{ name = "registry-ci"
, license = "BSD-3-Clause"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "convertable-options"
  , "crypto"
  , "datetime"
  , "dotenv"
  , "effect"
  , "either"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "graphs"
  , "http-methods"
  , "identity"
  , "integers"
  , "interpolate"
  , "js-date"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "precise-datetime"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "refs"
  , "safe-coerce"
  , "spec"
  , "string-parsers"
  , "strings"
  , "sunde"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
