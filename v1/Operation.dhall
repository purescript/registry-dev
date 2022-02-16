{-

A type describing all the possible operations for the Registry API.

-}

let Location = ./Location.dhall

in
  < Addition : { packageName : Text, newPackageLocation : Location, newRef : Text, addToPackageSet: Bool }
  | Update : { packageName : Text, updateRef : Text }
  | Unpublish : { packageName : Text, unpublishVersion : Text, unpublishReason: Text }
  >
