let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-halogen"
            , version = "v1.0.0"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { free = "^3.4.0"
                    , maps = "^2.1.2"
                    , profunctor = "^2.0.0"
                    , media-types = "^2.0.0"
                    , profunctor-lenses = "^2.6.0"
                    , fork = "^1.1.0"
                    , halogen-vdom = "^1.0.3"
                    , dom = "^3.5.1"
                    , coroutines = "^3.1.0"
                    , aff = "^2.0.3"
                    , unsafe-reference = "^1.0.0"
                    , dom-indexed = "^1.0.0"
                    , unsafe-coerce = "^2.0.0"
                    , const = "^2.0.0"
                    , freeap = "^2.0.0"
                    , parallel = "^2.1.0"
                    , foreign = "^3.2.0"
                    , nullable = "^2.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap
                    { websocket-simple = "^1.0.1"
                    , ace = "^3.0.0"
                    , affjax = "^3.0.2"
                    , aff-coroutines = "^4.0.0"
                    , random = "^2.0.0"
                    }
              }
          }
    }
