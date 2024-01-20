let Map = (./Prelude.dhall).Map.Type

let Owner = ./Owner.dhall

let Location = ./Location.dhall

let Version = Text

let Sha256 = Text

let ISO8601String = Text

let PublishedMetadata =
      { hash : Sha256
      , bytes : Natural
      , publishedTime : ISO8601String
      , compilers : < Single : Version | Many : List Version >
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
