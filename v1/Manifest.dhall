{-

The schema for `.purs.json` files.

This object holds all the info that the Registry needs to know about it.

-}

let Map = (./Prelude.dhall).Map.Type

let Owner = ./Owner.dhall

let Target = ./Target.dhall

let Manifest =
      -- The name of the package
      { name : Text
      -- Owners authorized to perform authenticated operations for this package
      , owners : Optional (List Owner)
      -- A short description of the package
      , description : Optional Text
      -- The SPDX code for the license under which the code is released
      , license : Text
      -- The version of this package
      , version : Text
      -- The git repo the package is published at
      , repository : ./Repo.dhall
      -- Compilation targets for the Package
      , targets : Map Text Target
      }

in Manifest
