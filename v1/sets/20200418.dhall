let From = (../Registry.dhall).Address

let compiler = "^v0.14.0"

let packages =
  { prelude = From.Registry { name = "prelude", version = "v4.1.1" }
  , effect = From.Registry { name = "effect", version = "v2.0.0" }
  , simple-json = From.Registry { name = "simple-json", version = "v7.0.0" }
  }

in { packages, compiler }