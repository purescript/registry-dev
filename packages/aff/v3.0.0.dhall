let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "aff"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata", repo = "purescript-aff", version = "v3.0.0" }
        )
    , targets = toMap
        { src = Registry.Target::{
          , sources = [ "src/**/*.purs" ]
          , dependencies = toMap
              { exceptions = "^3.0.0"
              , console = "^3.0.0"
              , unsafe-coerce = "^3.0.0"
              , transformers = "^3.0.0"
              , parallel = "^3.0.0"
              , datetime = "^3.0.0"
              , functions = "^3.0.0"
              }
          }
        , test = Registry.Target::{
          , sources = [ "src/**/*.purs", "test/**/*.purs" ]
          , dependencies = toMap { partial = "^1.2.0" }
          }
        }
    }
