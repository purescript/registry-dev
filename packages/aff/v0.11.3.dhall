let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata", repo = "purescript-aff", version = "v0.11.3" }
        )
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies =
                toMap
                  { exceptions = "^0.3.0"
                  , console = "^0.1.0"
                  , transformers = "^0.6.0"
                  , functions = "^0.1.0"
                  }
            }
          }
    }
