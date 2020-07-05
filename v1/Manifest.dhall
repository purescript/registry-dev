{-

The type of a Package version manifest in the Registry

-}

let Map = (./Prelude.dhall).Map.Type

let Target = ./Target.dhall

let Package =
      -- The name of the package
      { name : Text
      -- The SPDX code for the license under which the code is released
      , license : Text
      -- The git repo the package is published at
      , repository : ./Repo.dhall
      -- Compilation targets for the Package
      , targets : Map Text Target
      }

in Package
