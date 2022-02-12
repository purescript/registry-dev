{-

Schema for the `packages/$package-name.json` file, where for every package we
store all the metadata that the Registry needs in order to keep functioning.

-}

let Map = (./Prelude.dhall).Map.Type

let Location = ./Location.dhall

let Owner = ./Owner.dhall

let SemVer = Text

let RFC3339String = Text

-- Information about a single published version
let VersionMetadata =
  -- The ref this version points to (for example, a git commit)
  { ref : Text
  -- The hash of the source tarball fetched from the repo
  , hash : Text
  -- The size in bytes of the tarball
  , bytes : Natural
  -- The published date of the version as an RFC3339String
  , published : RFC3339String
  }

in
  -- The pointer to where the code lives
  { location : Location
  -- Owners authorized to perform authenticated operations for this package
  , owners : Optional (List Owner)
  -- A mapping between versions and info about a release
  , releases : Map SemVer VersionMetadata
  -- A mapping between a version number and the reason for unpublishing
  , unpublished : Map SemVer Text
  }
