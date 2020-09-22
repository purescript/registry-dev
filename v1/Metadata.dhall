{-

Schema for the `packages/$package-name.json` file, where for every package we
store all the metadata that the Registry needs in order to keep functioning.

-}

let Map = (./Prelude.dhall).Map.Type

let Repo = ./Repo.dhall

in
  {
  -- The pointer to where the code lives
  , location : Repo
  -- A mapping between version number and the hash of its tarball
  , versions : Map Text Text
  -- A mapping between a version number and the reason for unpublishing
  , unpublished : Map Text Text
  -- The list of maintainers of a package (note: these are GitHub usernames)
  , maintainers : List Text
  }