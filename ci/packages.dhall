let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211109/packages.dhall sha256:e8d8d5b339f6d46d950da90037c6c38e8809f7e34f727373089ab82c080fc709

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
