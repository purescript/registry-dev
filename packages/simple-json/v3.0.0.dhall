let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "simple-json"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "justinwoo"
            , repo = "purescript-simple-json"
            , version = "v3.0.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { variant = "^4.1.0"
                    , foreign-generic = "^6.0.0"
                    , typelevel-prelude = "^2.7.0"
                    , prelude = "^3.3.0"
                    , record = "^0.2.6"
                    , nullable = "^3.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap
                    { console = "^3.0.0"
                    , argonaut-core = "^3.1.1"
                    , spec = "^2.0.0"
                    }
              }
          }
    }
