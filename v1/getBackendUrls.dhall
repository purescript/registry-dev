{-

Function that returns the URL to fetch a package, for all the storage backends.

Example command line invocation if you'd like to know the URL for the package
`foo` at version `v1.2.3`:

```
$ dhall <<< './v1/getBackendUrls.dhall { name = "foo", version = "v1.2.3" }'

[ "https://example.com/purescriptRegistry/foo/v1.2.3.tar.gz" ]
```

-}

let Prelude = ./Prelude.dhall

let Package = { name : Text, version : Text}

let Backend = { mapKey : Text, mapValue : (Package -> Text) }

let getPackageUrls = \(package : Package) -> Prelude.List.map Backend Text (\(backend : Backend) -> backend.mapValue package) (./backends.dhall)

in getPackageUrls