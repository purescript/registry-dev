let PackageType =
      { name : Text
      , license : ./License.dhall
      , repository : Optional ./Repo.dhall
      , dependencies : List { mapKey : Text, mapValue : Text }
      , devDependencies : List { mapKey : Text, mapValue : Text }
      , backend : Optional Text
      , output : Text
      , sources : List Text
      , devSources : List Text
      }

in PackageType
