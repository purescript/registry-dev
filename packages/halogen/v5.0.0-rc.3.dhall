let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache-2.0`
    , repository = Some
        ( Registry.Repo.GitHub
            { owner = "slamdata"
            , repo = "purescript-halogen"
            , version = "v5.0.0-rc.3"
            }
        )
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { free = "^5.0.0"
                    , ordered-collections = "^1.0.0"
                    , console = "^4.1.0"
                    , profunctor = "^4.0.0"
                    , media-types = "^4.0.0"
                    , fork = "^4.0.0"
                    , halogen-vdom = "^6.0.0"
                    , web-uievents = "^2.0.0"
                    , coroutines = "^5.0.0"
                    , aff = "^5.0.0"
                    , unsafe-reference = "^3.0.0"
                    , avar = "^3.0.0"
                    , dom-indexed = "^7.0.0"
                    , unsafe-coerce = "^4.0.0"
                    , const = "^4.0.0"
                    , freeap = "^5.0.0"
                    , transformers = "^4.1.0"
                    , parallel = "^4.0.0"
                    , foreign = "^5.0.0"
                    , nullable = "^4.0.0"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies =
                  toMap
                    { ace = "^7.0.0"
                    , assert = "^4.0.0"
                    , affjax = "^9.0.0"
                    , aff-coroutines = "^7.0.0"
                    , web-socket = "^2.0.0"
                    , random = "^4.0.0"
                    }
              }
          }
    }
