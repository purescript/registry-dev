let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210308/packages.dhall sha256:5a86da7913f6c84adc2efacfad49ca135af8f62235e7270d9b952a8dda3c4b47

let additions = 
  { node-glob-basic =
        { dependencies =
          [ "aff"
          , "console"
          , "effect"
          , "lists"
          , "maybe"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "ordered-collections"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
        , version = "v1.1.0"
        }
  }

in  upstream // additions
