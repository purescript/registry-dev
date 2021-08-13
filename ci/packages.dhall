let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210613/packages.dhall sha256:5f10380b3ca7d3a32ea5c2b7535e4814a5e3f3590c70692f76e596d6ab0687b3

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
