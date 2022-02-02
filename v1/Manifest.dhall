{-

The schema for `purs.json` files.

This object holds all the info that the Registry needs to know about it.

-}

let Map = (./Prelude.dhall).Map.Type

let Manifest =
      -- The name of the package
      { name : Text
      -- A short description of the package
      , description : Optional Text
      -- The SPDX code for the license under which the code is released
      , license : Text
      -- The version of this package
      , version : Text
      -- The git repo the package is published at
      , repository : ./Repo.dhall
      -- The directories containing source files for this package
      , sources : List Text
      -- The packages this package depends on
      , dependencies : Map Text Text
      }

in Manifest
