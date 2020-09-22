{-

A type describing all the possible operations for the Registry API.

-}

let Repo = ./Repo.dhall

in
  < Addition : { packageName : Text, newPackageLocation : Repo, addToPackageSet: Bool }
  | Update : { packageName : Text, newVersion : Text }
  | Unpublish : { packageName : Text, unpublishVersion : Text, unpublishReason: Text }
  >