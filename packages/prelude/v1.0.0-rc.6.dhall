let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "prelude"
    , license = Registry.License.MIT
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "purescript"
            , repo = "purescript-prelude"
            , version = "v1.0.0-rc.6"
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
