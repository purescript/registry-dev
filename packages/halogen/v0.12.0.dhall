let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-halogen"
            , version = "v0.12.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { free = "^3.0.0"
                    , maps = "^2.0.0"
                    , stalling-coroutines = "^3.0.0"
                    , profunctor = "^2.0.0"
                    , dom = "^3.1.0"
                    , coroutines = "^3.0.0"
                    , aff = "^2.0.1"
                    , unsafe-coerce = "^2.0.0"
                    , aff-coroutines = "^4.0.0"
                    , const = "^2.0.0"
                    , aff-free = "^3.0.0"
                    , foreign = "^3.0.0"
                    , nullable = "^2.0.0"
                    }
              }
          }
    }
