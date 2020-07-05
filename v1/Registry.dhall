{-

This file groups together all the Registry types in a single record for
convenience of use.

-}

let Prelude = ./Prelude.dhall

let Repo = ./Repo.dhall

let Target = ./Target.dhall

let Package = ./Manifest.dhall

let Dependencies = Prelude.Map.Type Text Text

let Registry =
      { Repo
      , Prelude
      , Dependencies
      , Target
      , Package
      }

in  Registry
