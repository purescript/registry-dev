let Registry = ../../v1/Registry.dhall

in  Registry.Package::{
    , name = "halogen"
    , license = Registry.License.`Apache 2.0`
    , targets =
        toMap
          { src =
              { sources = [ "src/**/*.purs" ]
              , dependencies =
                  toMap
                    { void = "~0.2.0"
                    , maps = "~0.3.2"
                    , profunctor = "~0.2.1"
                    , arrays = "~0.3.3"
                    , exists = "~0.1.1"
                    , dom = "~0.1.1"
                    , aff = "~0.9.0"
                    , foldable-traversable = "~0.3.0"
                    , bifunctors = "~0.3.1"
                    , refs = "~0.1.2"
                    , foreign = "~0.4.0"
                    , lists = "~0.6.0"
                    , nullable = "~0.1.1"
                    }
              }
          , test =
              { sources = [ "src/**/*.purs", "test/**/*.purs" ]
              , dependencies = toMap { ace = "~0.1.0" }
              }
          }
    }
