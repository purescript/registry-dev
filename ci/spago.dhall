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
  , "effect"
  , "node-fs-aff"
  , "now"
  , "psci-support"
  , "refs"
  , "string-parsers"
  , "sunde"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
