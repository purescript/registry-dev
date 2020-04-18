let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-halogen"
            , version = "v0.9.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { free = "^1.0.0"
                    , maps = "^1.0.0"
                    , stalling-coroutines = "^1.0.0"
                    , profunctor = "^1.0.0"
                    , dom = "^1.1.0"
                    , aff = "^1.0.0"
                    , unsafe-coerce = "^1.0.0"
                    , aff-coroutines = "^2.0.0"
                    , const = "^1.0.0"
                    , aff-free = "^1.0.0"
                    , foreign = "^1.0.0"
                    , nullable = "^1.0.1"
                    }
              }
          }
    }
