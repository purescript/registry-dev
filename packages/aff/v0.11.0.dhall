let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.MIT
    , targets = toMap
        { src = Registry.Target::{
          , sources = [ "src/**/*.purs" ]
          , dependencies = toMap
              { exceptions = "~0.3.0"
              , control = "~0.3.0"
              , console = "~0.1.0"
              , either = "~0.2.0"
              , maybe = "~0.3.0"
              , monoid = "~0.3.0"
              , prelude = "~0.1.0"
              , eff = "~0.1.0"
              , transformers = "~0.6.0"
              , tuples = "~0.4.0"
              , functions = "~0.1.0"
              }
          }
        }
    }
