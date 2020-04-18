{-

The type of a Package version in the Registry.

Note: not all fields are compulsory when compiling the definition, and we do 
provide meaningful defaults for many of them in the ./Registry.dhall file.

-}

let Map = (./Prelude.dhall).Map.Type

{-

Every target can have its own dependencies and source globs.
By convention a package needs to have at least one target called `src`. 
Other common ones are `test`, `dev`, `bench`, etc. 

-}
let Target =
      -- A mapping between package names (as published on the Registry or
      -- included in the Package Set) and SemVer ranges for them.
      { dependencies : Map Text Text
      -- Source globs for the local project to include in the compilation
      -- alongside its dependencies.
      , sources : List Text
      }

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
      -- The output folder for the compiled artifacts from the compiler
      , output : Text
      -- Compilation targets for the Package
      , targets : Map Text Target
      -- A package could be not targeted at the JS backend, and might have
      -- a different default backend.
      , backend :
        -- The command to the alternate backend (if different than `purs`).
        -- Example values: `psgo`, `pskt`, etc.
        { cmd : Optional Text
        -- A list of backends this package is compatible with.
        , compatible : List Text 
        }
      }

in Package
