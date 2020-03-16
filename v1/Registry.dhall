let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v14.0.0/Prelude/package.dhall sha256:c1b3fc613aabfb64a9e17f6c0d70fe82016a030beedd79851730993e9083fde2

let License = ./License.dhall

let Repo = ./Repo.dhall

let PackageType = ./Package.dhall

let packageDefault =
      { license = License.MIT
      , repository = None Repo
      , dependencies = [] : List { mapKey : Text, mapValue : Text }
      , devDependencies = [] : List { mapKey : Text, mapValue : Text }
      , backend = None Text
      , output = "output"
      , sources = [ "src/**/*.purs" ]
      , devSources = [ "src/**/*.purs", "test/**/*.purs" ]
      }

let Registry =
      { License = License
      , Repo = Repo
      , Prelude = Prelude
      , Package = { Type = PackageType, default = packageDefault }
      }

in  Registry
