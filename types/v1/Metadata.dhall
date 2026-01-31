let Map = (./Prelude.dhall).Map.Type
let NonEmpty = (./Prelude.dhall).NonEmpty.Type

let Owner = ./Owner.dhall

let Location = ./Location.dhall

let Version = Text

let Sha256 = Text

let ISO8601String = Text

-- NOTE: The `ref` field is DEPRECATED and WILL BE REMOVED after 2027-01-31.
-- It is only present for backwards compatibility with older package managers.
-- Do not rely on its presence!
let PublishedMetadata =
      { hash : Sha256
      , bytes : Natural
      , publishedTime : ISO8601String
      , compilers : NonEmpty Version
      , ref : Optional Text
      }

let UnpublishedMetadata =
      { reason : Text
      , publishedTime : ISO8601String
      , unpublishedTime : ISO8601String
      }

let Metadata =
    { location : Location
    , owners : Optional (List Owner)
    , published : Map Version PublishedMetadata
    , unpublished : Map Version UnpublishedMetadata
    }

in Metadata
