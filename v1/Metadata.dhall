{-

Schema for the `packages/$package-name.json` file, where for every package we
store all the metadata that the Registry needs in order to keep functioning.

-}

let Map = (./Prelude.dhall).Map.Type

let Location = ./Location.dhall

let SemVer = Text

-- Information about a single published version
let VersionMetadata =
  {
  -- The ref this version points to (for example, a git commit)
  , ref : Text
  -- The hash of the source tarball fetched from the repo
  , hash : Text
  -- The size in bytes of the tarball
  , bytes : Natural
  }

in
  {
  -- The pointer to where the code lives
  , location : Location
  -- A mapping between versions and info about a release
  , releases : Map SemVer VersionMetadata
  -- A mapping between a version number and the reason for unpublishing
  , unpublished : Map Text Text
  }
