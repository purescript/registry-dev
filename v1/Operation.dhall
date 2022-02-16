{-

A type describing all the possible operations for the Registry API.

-}

let Repo = ./Repo.dhall

-- An operation that must be signed with the key listed in the owners field of
-- the manifest.
let AuthenticatedOperation =
  < Unpublish : { packageName : Text, unpublishVersion : Text, unpublishReason : Text }
  | Transfer : { packageName : Text, updateLocation : Repo }
  >

in
  < Addition : { packageName : Text, newPackageLocation : Repo, newRef : Text }
  | Update : { packageName : Text, updateRef : Text }
  | Authenticated : { payload : AuthenticatedOperation, signature : List Text, email : Text }
  >
