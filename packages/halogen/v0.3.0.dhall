let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache 2.0`
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { profunctor = "~0.1.0"
                    , arrays = "~0.3.3"
                    , dom = "~0.1.1"
                    , aff = "~0.5.6"
                    , foldable-traversable = "~0.3.0"
                    , refs = "~0.1.2"
                    , strings = "~0.4.3"
                    , foreign = "~0.4.0"
                    , nullable = "~0.1.1"
                    }
              }
          }
    }
