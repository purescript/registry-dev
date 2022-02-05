{-

A type describing all the possible operations for the Registry API.

-}

let Repo = ./Repo.dhall

in
  < Addition : { packageName : Text, newPackageLocation : Repo, newRef : Text, addToPackageSet: Bool, legacy : Bool }
  | Update : { packageName : Text, updateRef : Text, legacy : Bool }
  | Unpublish : { packageName : Text, unpublishVersion : Text, unpublishReason: Text }
  >
