{-

Coordinates for a package, i.e. “some kind of Git place”.

We have special support for GitHub because it’s easier for us if packages are
there, as we use their API to do things, e.g. to fetch commit tarballs.
However, we should always be able to support a “generic git thing”, so that
we can allow hosting packages on other providers too.

-}

let CommonData =
  { subdir : Optional Text
  }

let GitHubData = CommonData //\\
  { githubOwner : Text
  , githubRepo : Text
  }

let GitData = CommonData //\\
  { url : Text }

in < GitHub : GitHubData | Git : GitData >
