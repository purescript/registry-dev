{-

Coordinates for a package. We support a set of providers where we understand
how to fetch the package, such as arbitrary Git URLs or GitHub's storage.

-}

let CommonData =
  { subdir : Optional Text
  }

let GitHubData = CommonData //\\
  { githubOwner : Text
  , githubRepo : Text
  }

let GitData = CommonData //\\
  { gitUrl : Text }

in < GitHub : GitHubData | Git : GitData >
