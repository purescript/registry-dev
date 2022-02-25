{-

The schema for `purs.json` files.

This object holds all the info that the Registry needs to know about it.

-}

let Map = (./Prelude.dhall).Map.Type

let Owner = ./Owner.dhall

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
      -- The location where package sources can be found
      , location : ./Location.dhall
      -- A list of globs indicating files in addition to the src directory
      -- that should be included in the package tarball
      , files : Optional (List Text)
      -- The packages this package depends on
      , dependencies : Map Text Text
      }

in Manifest
