let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "simple-json"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "justinwoo"
            , repo = "purescript-simple-json"
            , version = "v5.0.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { exceptions = "^4.0.0"
                    , variant = "^6.0.0"
                    , foreign-object = "^2.0.0"
                    , typelevel-prelude = "^4.0.0"
                    , prelude = "^4.0.0"
                    , record = "^2.0.0"
                    , globals = "^4.0.0"
                    , foreign = "^5.0.0"
                    , nullable = "^4.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap { assert = "^4.0.0", generics-rep = "^6.0.0" }
              }
          }
    }
