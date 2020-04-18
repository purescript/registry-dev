let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "simple-json"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "justinwoo"
            , repo = "purescript-simple-json"
            , version = "v1.0.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { foreign-generic = "^5.0.0"
                    , typelevel-prelude = "^2.4.0"
                    , prelude = "^3.1.0"
                    , record = "^0.2.0"
                    , nullable = "^3.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap
                    { console = "^3.0.0"
                    , argonaut-core = "^3.1.0"
                    , spec = "^1.0.0"
                    }
              }
          }
    }
