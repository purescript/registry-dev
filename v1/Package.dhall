{-

The type of a Package version in the Registry.

Note: not all fields are compulsory when compiling the definition, and we do 
provide meaningful defaults for many of them in the ./Registry.dhall file.

-}

let Map = (./Prelude.dhall).Map.Type

let Target = (./Target.dhall).Type

let Package =
      -- The name of the package
      { name : Text
      -- The SPDX code for the license under which the code is released
      , license : ./License.dhall
      -- The git repo the package is published at
      , repository : Optional ./Repo.dhall
      -- The source for the package versions listed in the `targets`.
      -- Can be either the Registry or a PackageSet
      , packages : ./Index.dhall
      -- Compilation targets for the Package
      , targets : Map Text Target
      }

in Package
