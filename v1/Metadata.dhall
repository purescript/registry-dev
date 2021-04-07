{-

Schema for the `packages/$package-name.json` file, where for every package we
store all the metadata that the Registry needs in order to keep functioning.

-}

let Map = (./Prelude.dhall).Map.Type

let Repo = ./Repo.dhall

let SemVer = Text

let VersionMetadata = { ref : Text, hash : Text }

in
  {
  -- The pointer to where the code lives
  , location : Repo
  -- A mapping between versions and info about a release
  , releases : Map SemVer VersionMetadata
  -- A mapping between a version number and the reason for unpublishing
  , unpublished : Map Text Text
  }
