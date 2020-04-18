let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.MIT
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies =
                toMap
                  { exceptions = "~0.2.2"
                  , control = "~0.2.2"
                  , either = "~0.1.4"
                  , maybe = "~0.2.1"
                  , monoid = "~0.2.0"
                  , tuples = "~0.3.0"
                  , monad-eff = "~0.1.0"
                  }
            }
          }
    }
