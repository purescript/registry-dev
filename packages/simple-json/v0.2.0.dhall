let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "simple-json"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "justinwoo"
            , repo = "purescript-simple-json"
            , version = "v0.2.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { foreign-generic = "^4.2.0"
                    , typelevel-prelude = "^2.3.1"
                    , prelude = "^3.1.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies = toMap { console = "^3.0.0", spec = "^1.0.0" }
              }
          }
    }
