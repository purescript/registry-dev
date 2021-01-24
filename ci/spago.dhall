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
  , "effect"
  , "node-fs-aff"
  , "now"
  , "psci-support"
  , "refs"
  , "spec"
  , "string-parsers"
  , "sunde"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
