let Prelude = ./Prelude.dhall

let Address = ./Address.dhall

let Map = (./Prelude.dhall).Map.Type

let PackageSet =
    { compiler : Text
    , packages : Map Text Address
    }

in PackageSet
