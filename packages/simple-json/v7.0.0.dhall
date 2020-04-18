let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "simple-json"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "justinwoo"
            , repo = "purescript-simple-json"
            , version = "v7.0.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { exceptions = "^4.0.0"
                    , variant = "^6.0.1"
                    , arrays = "^5.3.0"
                    , foreign-object = "^2.0.3"
                    , typelevel-prelude = "^5.0.0"
                    , prelude = "^4.1.1"
                    , record = "^2.0.1"
                    , globals = "^4.0.0"
                    , foreign = "^5.0.0"
                    , nullable = "^4.1.1"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap { assert = "^4.1.0", generics-rep = "^6.1.1" }
              }
          }
    }
