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
let PublishedMetadata =
  -- The ref this version points to (for example, a git commit)
  { ref : Text
  -- The hash of the source tarball fetched from the repo
  , hash : Text
  -- The size in bytes of the tarball
  , bytes : Natural
  -- The published time of the version as an RFC 3339 string
  , publishedTime : RFC3339String
  }

-- Information about a single unpublished version
let UnpublishedMetadata =
  -- The ref this version points to (for example, a git commit)
  { ref : Text
  -- The hash of the source tarball fetched from the repo
  , reason : Text
  -- The published date of the version as an RFC 3339 string
  , publishedTime : RFC3339String
  -- The published date of the version as an RFC 3339 string
  , unpublishedTime : RFC3339String
  }

in
  -- The pointer to where the code lives
  { location : Location
  -- Owners authorized to perform authenticated operations for this package
  , owners : Optional (List Owner)
  -- A mapping between versions and info about a release
  , published : Map SemVer PublishedMetadata
  -- A mapping between a version number and the reason for unpublishing
  , unpublished : Map SemVer UnpublishedMetadata
  }
