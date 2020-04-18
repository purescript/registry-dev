let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "prelude"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "purescript"
            , repo = "purescript-prelude"
            , version = "v0.1.4"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies = [] : Registry.Dependencies
              }
          }
    }
