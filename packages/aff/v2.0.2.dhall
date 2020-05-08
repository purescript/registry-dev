let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata", repo = "purescript-aff", version = "v2.0.2" }
        )
    , targets = toMap
        { src = Registry.Target::{
          , sources = [ "src/**/*.purs" ]
          , dependencies = toMap
              { exceptions = "^2.0.0"
              , console = "^2.0.0"
              , unsafe-coerce = "^2.0.0"
              , transformers = "^2.0.1"
              , parallel = "^2.0.0"
              , functions = "^2.0.0"
              }
          }
        , test = Registry.Target::{
          , sources = [ "src/**/*.purs", "test/**/*.purs" ]
          , dependencies = toMap { partial = "^1.1.2" }
          }
        }
    }
