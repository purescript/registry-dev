let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220103/packages.dhall sha256:6d8302fb12249524ab2f91282935c1750789a1f3d68dc0bcb7ee46441f91f244

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
