-- Minimal packages.dhall for testing - defines only required dependencies inline
{ prelude =
  { dependencies = [] : List Text
  , repo = "https://github.com/purescript/purescript-prelude.git"
  , version = "v6.0.1"
  }
}
