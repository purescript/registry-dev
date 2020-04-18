let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata", repo = "purescript-aff", version = "v0.16.2" }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { exceptions = "^0.3.0"
                    , console = "^0.1.0"
                    , transformers = "^0.8.1"
                    , functions = "^0.1.0"
                    }
              }
          }
    }
