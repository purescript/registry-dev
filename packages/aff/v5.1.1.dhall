let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata", repo = "purescript-aff", version = "v5.1.1" }
        )
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies =
                toMap
                  { exceptions = "^4.0.0"
                  , effect = "^2.0.0"
                  , unsafe-coerce = "^4.0.0"
                  , transformers = "^4.0.0"
                  , parallel = "^4.0.0"
                  , datetime = "^4.0.0"
                  , functions = "^4.0.0"
                  }
            }
          , test = Registry.Target::{
            , sources = [ "src/**/*.purs", "test/**/*.purs" ]
            , dependencies =
                toMap
                  { free = "^5.0.0"
                  , console = "^4.1.0"
                  , minibench = "^2.0.0"
                  , assert = "^4.0.0"
                  , partial = "^2.0.0"
                  }
            }
          }
    }
