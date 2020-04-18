{-

This file groups together all the Registry types in a single record for
convenience of use.

Users are expected to import this file in their Package configurations.

Most notably, this wraps the `Package` type with some defaults, so that users
can declare their packages without having to provide all fields, thanks to 
the `::` operator.
For more info on that see the relevant docs:
https://docs.dhall-lang.org/references/Built-in-types.html#id133

-}

let Prelude = ./Prelude.dhall

let License = ./License.dhall

let Address = ./Address.dhall

let Index = ./Index.dhall

let Repo = ./Repo.dhall

let Target = ./Target.dhall

let PackageType = ./Package.dhall

let Dependencies = Prelude.Map.Type Text Text

let packageDefault =
      { license = License.MIT
      , repository = None Repo
      , packages = Index.Registry ([] : Prelude.Map.Type Text Address)
      }

let Registry =
      { License = License
      , Address = Address
      , Index = Index
      , Repo = Repo
      , Prelude = Prelude
      , Dependencies = Dependencies
      , Target = Target
      , Package = { Type = PackageType, default = packageDefault }
      }

in  Registry
