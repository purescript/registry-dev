let Map = (./Prelude.dhall).Map.Type

let PackageName = Text

let License = Text

let Version = Text

let Range = Text

let Manifest =
    { name : PackageName
    , license : License
    , version : Version
    , location : ./Location.dhall
    , owners : Optional (List ./Owner.dhall)
    , description : Optional Text
    , files : Optional (List Text)
    , dependencies : Optional (Map PackageName Range)
    }

in Manifest
