let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-halogen"
            , version = "v3.0.1"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { free = "^4.1.0"
                    , maps = "^3.5.2"
                    , profunctor = "^3.2.0"
                    , media-types = "^3.0.0"
                    , profunctor-lenses = "^3.7.0"
                    , fork = "^3.0.0"
                    , halogen-vdom = "^2.0.0"
                    , dom = "^4.10.0"
                    , coroutines = "^4.0.0"
                    , aff = "^4.0.0"
                    , unsafe-reference = "^2.0.0"
                    , dom-indexed = "^5.0.0"
                    , unsafe-coerce = "^3.0.0"
                    , const = "^3.0.0"
                    , freeap = "^3.0.1"
                    , parallel = "^3.3.0"
                    , foreign = "^4.0.1"
                    , nullable = "^3.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap
                    { reflection = "^3.0.0"
                    , ace = "^4.0.0"
                    , affjax = "^5.0.0"
                    , aff-coroutines = "^6.0.0"
                    , random = "^3.0.0"
                    }
              }
          }
    }
