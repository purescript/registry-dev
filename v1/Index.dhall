{-

The source of packages in a given package manifest.

It could either be:
- *the Registry*: this means the index (i.e. the published packages) at the 
  current moment.
  This source can be extended with a mapping between package names and a list
  of `Address`es (see ./Address.dhall), so that users can import local packages
  or things from outside the registry in general.
- *a Package Set*: that is, a list of package versions that are known to
  compile together. A Set is compatible with a range of PureScript versions,
  and is a mapping from package names to `Address`es.
  Package Sets published on the Registry will only contain an `Address` pointing
  at the Registry, but users can override packages with local and/or external
  ones.

-}
let Map = (./Prelude.dhall).Map.Type

let Address = ./Address.dhall

in
  < Registry :  Map Text Address
  | PackageSet : 
    { compiler : Text
    , packages : Map Text Address
    }
  >