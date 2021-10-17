{ name = "registry-ci"
, license = "BSD-3-Clause"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "crypto"
  , "datetime"
  , "dotenv"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "identity"
  , "interpolate"
  , "js-date"
  , "filterable"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-glob-basic"
  , "node-path"
  , "node-process"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
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
