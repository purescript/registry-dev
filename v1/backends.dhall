{-

A "storage backend" is a function from "packageName + version" to a URL to fetch
the tarball for that version.

Here we store a mapping from "name of the backend" to their "URL function".

See the `./getBackendUrls.dhall` file for a utility to get all the URLs of a
package on our storage backends.

Clients that can evaluate Dhall can invoke that function directly, or reimplement
the specification defined here.

-}

let Map = (./Prelude.dhall).Map.Type

let Package = { name : Text, version : Text }

let Backend = Package -> Text

let backends = toMap
  { main = \(package : Package) -> "https://packages.purescript.org/${package.name}/${package.version}.tar.gz"
  }

in backends : Map Text Backend
