{ name = "registry-ci"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "bower-json"
  , "console"
  , "crypto"
  , "debug"
  , "effect"
  , "node-fs-aff"
  , "node-process"
  , "now"
  , "psci-support"
  , "refs"
  , "spec"
  , "string-parsers"
  , "sunde"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
