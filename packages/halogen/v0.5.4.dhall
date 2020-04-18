let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache 2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-halogen"
            , version = "v0.5.4"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { free = "^0.9.0"
                    , void = "^0.3.0"
                    , maps = "^0.5.0"
                    , profunctor = "^0.3.1"
                    , dom = "^0.2.6"
                    , inject = "^0.3.0"
                    , unsafe-coerce = "^0.1.0"
                    , aff-coroutines = "^0.4.0"
                    , const = "^0.5.0"
                    , foreign = "^0.7.0"
                    , nullable = "^0.2.0"
                    }
              }
          }
    }
