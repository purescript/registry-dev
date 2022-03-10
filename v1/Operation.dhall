{-

A type describing all the possible operations for the Registry API.

-}

let Location = ./Location.dhall

-- A map of package names to package versions describing the dependencies
-- necessary to compile this package, along with the compiler version to use.
--
-- The compiler version must be a non-pre-release version with no build
-- metadata, such as '0.14.0'. Compiler versions are accepted from 0.13.0
-- onward; earlier compilers are not supported.
let BuildPlan = { compiler : SemVer, resolutions : Map Text SemVer }

-- An operation that must be signed with the key listed in the owners field of
-- the manifest.
let AuthenticatedOperation =
  < Unpublish : { packageName : Text, unpublishVersion : Text, unpublishReason : Text }
  | Transfer : { packageName : Text, newPackageLocation : Location }
  >

in
  < Addition : { packageName : Text, newPackageLocation : Location, newRef : Text, buildPlan : BuildPlan }
  | Update : { packageName : Text, updateRef : Text, buildPlan : BuildPlan }
  | Authenticated : { payload : AuthenticatedOperation, signature : List Text, email : Text }
  >
