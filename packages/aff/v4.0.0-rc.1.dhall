let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-aff"
            , version = "v4.0.0-rc.1"
            }
        )
    , targets =
        toMap
          { src = Registry.Target::{
            , sources = [ "src/**/*.purs" ]
            , dependencies =
                toMap
                  { free = "^4.0.1"
                  , exceptions = "^3.0.0"
                  , console = "^3.0.0"
                  , type-equality = "^2.1.0"
                  , avar = "^1.0.1"
                  , st = "^3.0.0"
                  , unsafe-coerce = "^3.0.0"
                  , transformers = "^3.0.0"
                  , parallel = "^3.0.0"
                  , datetime = "^3.0.0"
                  , functions = "^3.0.0"
                  }
            }
          , test = Registry.Target::{
            , sources = [ "src/**/*.purs", "test/**/*.purs" ]
            , dependencies =
                toMap
                  { minibench = "^1.0.0"
                  , assert = "^3.0.0"
                  , partial = "^1.2.0"
                  }
            }
          }
    }
