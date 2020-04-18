let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata", repo = "purescript-aff", version = "v1.0.0" }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { exceptions = "^1.0.0"
                    , console = "^1.0.0"
                    , unsafe-coerce = "^1.0.0"
                    , transformers = "^1.0.0"
                    , parallel = "^1.0.0"
                    , functions = "^1.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies = toMap { partial = "^1.1.2" }
              }
          }
    }
