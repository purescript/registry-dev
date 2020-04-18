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
                    , dom = "~0.1.1"
                    , aff = "~0.5.2"
                    , foldable-traversable = "~0.3.0"
                    , refs = "~0.1.2"
                    }
              }
          }
    }
